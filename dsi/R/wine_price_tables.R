# Extracting accurate prices from tables

# TO DO
# (long-term) expand search area for column headers if not easily caught above table 
# addMissing can be streamlined, fix so it doesn't break if run twice, finish missings ids once i see an example
# smarter buffer
# make column recognition incorporate text justification
# cluster tops and bottoms if multiple charts on page
# have addPrice and addIDs use better page segmentation mode (probably 3)
# make sure page segmentation mode is being reset properly at all levels (see warning)
# In nameBoxes without ID, add row justification to help choose bottom or top for number of rows

# After benchmark:
# resolve code differences between dynamic name box size in id vs. non-id case. which is better?
# make min.col.width in pageTables visible to user/global?
# Make recognition of column header (e.g. Bottle, Case, Quart) smarter with levenschtein distance
# in pageTables, add a check here to eliminate changepoint before a space that separates names from price cols

# workflow:
  # 1. Deskew and get boxes -- use existing Rtess functions. helpful to deskew first.
  # 2. Get initial column info -> find more possible prices -> get final column info 
  # -- bounds and slopes, justification of column text, character height
  # 3. Extract text from columns, find more prices and IDs
  # 4. Find table locations using changepoint method, group columns by table
  # 5. Check again for missing prices and now IDs 
  # 6. Locate and extract names boxes (words and images)
  # 7. Return final results (ids, prices, name boxes)

####################################################################################################
# Setup ####
####################################################################################################

wd = "~/Documents/DSI" #path to wine-price-extraction repo
setwd(wd)

library(Rtesseract)
library(tidyverse)
library(stringr)
library(jpeg)
library(cluster)
library(changepoint)

source("wine-price-extraction/dsi/R/wine_price_tables_functions.R")
source("wine-price-extraction/dsi/R/helper.R") 

# MAIN  FUNCTION, encompassing all numbered steps below ----
price_table_extraction <- function(file1, image.check = FALSE, data1 = NULL, pix.threshold = NULL, pix.newValue = NULL, save.root = ".") {
  img1 = paste("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/", file1, ".jpg", sep = "")
  if (!file.exists(img1)) {
    if (!exists("sample_files")) {sample_files = readRDS("~/Documents/DSI/wine-price-extraction/dsi/Data/sample_files.RDS")}
    folder = sample_files[which(sample_files$file == file1),"Sample"]
    img1 = paste("~/Documents/DSI/OCR_SherryLehmann/Sample/", folder, "/", file1, ".jpg", sep = "")
  }
  api = tesseract(img1, pageSegMode = 6, engineMode = 3)
  height1 = dim(readJPEG(img1))[1] #note we'll use the image attribute here later
  
  ############ img check 1 ####
  if(image.check) {
    gb1 = GetBoxes(api)
    prices1 = gb1[isPrice(gb1$text),] 
    plot(api, cropToBoxes = F, bbox = prices1)
  }
  # tried removing low-confidence prices -- removed real prices, so don't do that
  
  # 1 ####
  px1 = deskew(pixConvertTo8(pixRead(img1)), binaryThreshold = 50)
  if (!is.null(pix.threshold) & !is.null(pix.newValue)) {px1 = pixThresholdToValue(px1, pix.threshold, pix.newValue)}
  if (is.null(data1)) {data1 = GetBoxes(px1, pageSegMode = 6, engineMode = 3)}
  #data1[isPrice(data1$text),]
  
  # 2 ####
  page.cols = pageCols(data1, img.height = height1)
  
  ############# img check 2 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1)
  if(image.check & !is.null(page.cols$ids)) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1)
  
  # 3 ####
  
  # check for new prices and return updated page.cols ####
  page.cols = addPrices(page.cols, px1) 
  
  # check for new ids ####
  if (!is.null(page.cols$ids)) {
    page.cols = addIds(page.cols, px1)
  }
  
  ############# img check 3 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  if(image.check & !is.null(page.cols$ids)) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1, confidence = FALSE)
  
  #plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", prices), img = px1)
  
  ############# table check 1 ####
  if (!is.null(page.cols$ids)) {
    cat("We guess there's", page.cols$n_id_cols, "table(s)\n")
    tmp.maybe_missing = rep(page.cols$id_cols$entries, each = page.cols$n_price_cols/page.cols$n_id_cols) - sapply(page.cols$prices, nrow)
    if (max(tmp.maybe_missing) > 0) {
      cat("Column(s)", which(tmp.maybe_missing>0), "may be missing", tmp.maybe_missing[tmp.maybe_missing>0], "# of entry")
    } else if (min(tmp.maybe_missing) < 0) {
      cat("ID column(s) may be missing up to", -1*min(tmp.maybe_missing), "entrie(s)")
    } else {cat("Columns don't seem to be missing entries\n")}
  }
  
  # 4 ####
  
  # Find table locations using changepoint method, return tables item with table, border and column locations
  page.tables = pageTables(data1, page.cols$prices, page.cols, buffer = page.cols$charheight/3)
  # Add table and column var to price column table
  page.cols$price_cols$table = whichTable(page.tables, page.cols)
  page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% mutate(column = rank(cluster))
  
  ############# table check 2 ####
  # Check that left edge based on ID columns, if present, equals changepoint method (page.tables) left-edge
  # Default to method based on ID columns
  if (!is.null(page.cols$ids)) {
    tmp.left.diffs = page.cols$id_cols$col_left - round(unlist(lapply(page.tables$tables, attr, "left")))
    cat("Difference in left-edge detected using ID col method vs. changepoint method",
        tmp.left.diffs, "\n")
    cat("This is", tmp.left.diffs/page.cols$charheight,  "times estimated character height\n")
    if(max(100*(tmp.left.diffs)/page.cols$charheight) > 100) {cat("Error?? Greater than character's difference")}
  }
  
  # 5 ####
  
  if (!is.null(page.cols$ids)) {
    page.cols = removeExtra(page.cols, removeType = "ids", charwidth.cutoff = 2) #default remove type is prices
  }
  
  page.cols = addMissing(page.cols, page.tables, buffer = page.cols$charheight, img.height = height1, px1)
  
  page.cols = removeExtra(page.cols, removeType = "all") 
  
  ############# img check 4 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  if(image.check & !is.null(page.cols$ids)) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1, confidence = FALSE)
  
  # 6 (this will fail if no words in the name) #### 
  
  try({name.boxes = nameBoxes(data1, page.cols = page.cols, prices = page.cols$prices, px = px1, 
                              buffer = page.cols$charheight/2, page.tables = page.tables,
                              text.level = "word", psm = 3)}) #will want to experimetn with textline vs. word here - textline good when words split by spaces when they shouldn't be
  
  # 7 ####
  
  # ids (if there) and prices
  final.prices = vector("list", max(page.cols$price_cols$table))
  final.prices = lapply(1:max(page.cols$price_cols$table), function(x) {
    final.prices[[x]] = list(
      if (!is.null(page.cols$ids)) {ids = unlist(page.cols$ids %>% filter(table == x) %>% .$text)},
      prices = lapply(subset(page.cols$prices, page.cols$price_cols$table == x), 
                      function(y) y$text.new)
    )
    names(final.prices[[x]]$prices) = subset(page.cols$price_cols, page.cols$price_cols$table == x)$column.header
    final.prices[[x]]
  })
  
  # all
  if (exists("name.boxes")) {
    
    final.data = list(prices = final.prices, name.locations = name.boxes[["locations"]],
                      name.words = name.boxes[["words"]], name.words.old = name.boxes[["words_old"]])
    
    ################## image check 4a / save name box image ####
    tmp.boxes = do.call("rbind", name.boxes[[1]])
    png(paste("wine-price-extraction/dsi/Data/", file1, "_name_boxes.png", sep=""), width = 1000, height = 1500)
    plot(tesseract(px1), cropToBoxes = F, bbox = tmp.boxes, img = px1, confidence = FALSE)
    dev.off()
    
  } else {
    final.data = list(prices = final.prices)
  }
  
  saveRDS(final.data, paste(save.root,"/wine-price-extraction/dsi/Data/", file1, ".RDS", sep = ""))
  
  ################## image check 4b / save price image ####
  png(paste("wine-price-extraction/dsi/Data/", file1, "_price_boxes.png", sep=""), width = 1000, height = 1500)
  plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  dev.off()
  
  #may want to save deskewed image for post-processing
  #png(paste("wine-price-extraction/dsi/Data/", file1, "_deskew.png", sep=""), width = 4000, height = 6000)
  #plot(tesseract(px1), img = px1)
  #dev.off()
  
  return(list(final.data = final.data, page.cols = page.cols))
}

####################################################################################################
# RUN ####
####################################################################################################

# Example ----

fullBoxes = readRDS("~/Documents/DSI/OCR_SherryLehmann/FullBoxes.rds")

file1 = "UCD_Lehmann_0208" #0011, 1106, 0237, 3392, 1452 (hard), 0069, 1802, 3943, 0066, 0455
#debugonce(price_table_extraction)
#price_table_extraction(file1, image.check = FALSE, save.root = wd) #pix.threshold = 150, pix.newValue = 0
data1 = fullBoxes[[paste0(file1,".jpg")]]
price_table_extraction(file1, image.check = FALSE, save.root = wd, data1 = data1) #pix.threshold = 150, pix.newValue = 0

# Run on a fileset ----
fileset = str_extract(list.files("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages"), ".*[^\\.jpg]")
fileset = subset(fileset, str_detect(fileset, "UCD_Lehmann_[0-9]{4}"))
output = vector("list", length(fileset))
names(output) = fileset

i = 0; pix.threshold = NULL; pix.newValue = NULL
for (file1 in fileset) {
  i = i+1; cat(file1, i)
  img1 = paste("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/", file1, ".jpg", sep = "")
  if (!file.exists(img1)) {
    if (!exists("sample_files")) {sample_files = readRDS("~/Documents/DSI/wine-price-extraction/dsi/Data/sample_files.RDS")}
    folder = sample_files[which(sample_files$file == file1),"Sample"]
    img1 = paste("~/Documents/DSI/OCR_SherryLehmann/Sample/", folder, "/", file1, ".jpg", sep = "")
  }
  api = tesseract(img1, pageSegMode = 6, engineMode = 3)
  height1 = dim(readJPEG(img1))[1] #note we'll use the image attribute here later
  
  px1 = deskew(pixConvertTo8(pixRead(img1)))
  data1 = fullBoxes[[paste0(file1,".jpg")]]
  #data1 = GetBoxes(px1, pageSegMode = 6, engineMode = 3)
  if (median(data1$confidence) > 30 & sum(isPrice(data1$text, dollar = FALSE)) >= 3) {
    try({output[[i]] = price_table_extraction(file1, image.check = FALSE, data1)})
  }
  if (is.null(output[[i]])) {
    output[[i]]= c("median_conf" = median(data1$confidence), "n_price" = sum(isPrice(data1$text, dollar = FALSE)))
  }
}

# Results ----

#2. Second round

# 37 null
# 30 with some results
# 18 name boxes

output.text = output[sapply(output, length)==2] #34
table(unlist(lapply(output.text, function(x) {length((x[[1]])[[1]][[1]][[1]])})))
tmp = lapply(output.text, function(x) {(x[[1]])[[1]][[1]][2]})
length(tmp)
#1. First round 
#58 are null (code failed)
#output.null = fileset[sapply(output, is.null)] #58
#output.text = fileset[sapply(output, length)==2] #34
#data.frame(file1 = output.text, do.call("rbind", output[sapply(output, length)==2]))


#moved the no price table files to a seperate folder
for (file in paste0(output.text, ".jpg"))
  system(paste0("mv OCR_SherryLehmann/SampleCatalogPages/", file," OCR_SherryLehmann/SampleCatalogPages/no_price_tables"))
# FALSE NEGATIVE: 1176
# Note that some other pages have prices or tables but not both in the price-table format.
# The most common case we might be intrested in is names and ids with a large price nearby

# Create an option to try failures with 
# If still fail, try new pix thresholds. This should help with 1176, for example

price_table_extraction("UCD_Lehmann_1176", image.check = FALSE, pix.threshold = 150, pix.newValue = 0)



