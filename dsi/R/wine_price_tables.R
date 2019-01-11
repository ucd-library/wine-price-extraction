# Extracting accurate prices from tables

# TO DO
# (short term)
#   based on truth making -> code to eliminate duplicates in DIFFERENT tables/merge columns vertically (0027)
#   
# (after benchmark)
#   resolve code differences between dynamic name box size in id vs. non-id case. which is better?
#   make the search for “stuff” (words) between columns in pageTables better 
# (long-term)
#   expand search area for column headers if not easily caught above table 
#   smarter buffer with more exposure to user
#   add row justifiation, e.g. for better outlining of nameBoxes
#   make min.col.width in pageTables visible to user/global?

# workflow:
  # 1. Deskew and get boxes -- use existing Rtess functions. helpful to deskew first.
  # 2. Get initial column info -> more possible prices -> get final column info 
  # -- bounds and slopes, justification of column text, character height
  # 3. Extract text from columns, find more prices and IDs
  # 4. Find table locations using changepoint method, group columns by table, order tables by row on page, then left to right
  # 5. Check again for missing prices and now IDs, remove/clean extraneous or duplicates
  # 6. Locate and extract names boxes (words and images)
  # 7. Return final results (ids, prices, name boxes)

####################################################################################################
# Setup ####
####################################################################################################

library(Rtesseract)
library(tidyverse)
library(stringr)
library(jpeg)
library(cluster)
library(changepoint)
library(RecordLinkage)

wd = "~/Documents/DSI" #path to wine-price-extraction repo
setwd(wd)

source("wine-price-extraction/dsi/R/wine_price_tables_functions.R")
source("wine-price-extraction/dsi/R/helper.R") 

# MAIN  FUNCTION, encompassing all numbered steps below ----
price_table_extraction <- function(file1, image.check = FALSE, data1 = NULL, pix.threshold = NULL, pix.newValue = NULL, save.root = ".",
                                   column.header = c("bottle", "case", "quart", "fifth")) {
  
  #column.header is convered to lower for comparison
  
  # 0 Setup ####
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
  if(! file.exists(paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))) {saveRDS(data1, paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))}
  
  # 2 ####
  if (sum(isPrice(data1$text)) + sum(isPrice(data1$text, maybe = T)=="*price") == 0) {return("No prices detected. If prices suspected try a new pix.threshold.")}
  page.cols = pageCols(data1, img.height = height1, column.header = column.header)
  
  ############# img check 2 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1)
  if(image.check & !is.null(page.cols$ids)) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1)
  
  # 3 ####
  
  # check for new prices ####
  page.cols = addPrices(page.cols, px1)
  
  # check for new ids ####
  if (!is.null(page.cols$ids)) { page.cols = addIds(page.cols, px1) }
  
  ############# img check 3 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  if(image.check & !is.null(page.cols$ids)) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1, confidence = FALSE)
  
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
  page.cols = pageTables(data1, page.cols, buffer = page.cols$charheight/3)
  
  ############# table check 2 ####
  # Check that left edge based on ID columns, if present, equals changepoint method (page.tables) left-edge
  # Default to method based on ID columns
  if (!is.null(page.cols$ids)) {
    tmp.left.diffs = page.cols$id_cols$col.left - page.cols$price_cols$table.left.cpt
    cat("Difference in left-edge detected using ID col method vs. changepoint method",
        tmp.left.diffs, "\n")
    cat("This is", tmp.left.diffs/page.cols$charheight,  "times estimated character height\n")
    if(max(100*(tmp.left.diffs)/page.cols$charheight) > 100) {cat("Mistake? Greater than character's difference\n")}
  }
  
  # 5 ####
  
  page.cols = removeExtra(page.cols, removeType = "all", charwidth.cutoff = 2) #default remove type is prices
  
  page.cols = addMissing(page.cols, buffer = page.cols$charheight, img.height = height1, px1)
  
  page.cols = removeExtra(page.cols, removeType = "all") 
  
  ############# img check 4 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  if(image.check & !is.null(page.cols$ids)) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1, confidence = FALSE)
  
  # 6 (this will fail if no words in the name) #### 
  try({name.boxes = nameBoxes(data1, page.cols = page.cols, prices = page.cols$prices, px = px1, 
                              buffer = page.cols$charheight/2, text.level = "word", psm = 3)}) #will want to experimetn with textline vs. word here - textline good when words split by spaces when they shouldn't be
  
  
  # 7 ####
  
  # ids (if there) and prices
  final.prices = vector("list", max(page.cols$price_cols$table))
  final.prices = lapply(1:max(page.cols$price_cols$table), function(x) {
    
    table.prices = subset(page.cols$prices, sapply(page.cols$prices, function(x) {x$table[1]}) == x)
    # within table, make sure columns ordered from left to right
    table.prices = table.prices[order(sapply(table.prices, function(x) {min(x$left)}))] 
    
    final.prices[[x]] = list(
      ids = ifelse(!is.null(page.cols$ids) && sum(page.cols$ids$table == x) > 0,
                   list(page.cols$ids %>% filter(table == x) %>% select(row, text)),
                   list(NULL)) ,
      prices = lapply(table.prices, select, row, text.new))
    
    names(final.prices[[x]]$prices) = (filter(page.cols$price_cols, table == x) %>% 
                                         arrange(col.left))[["col.header"]]
    final.prices[[x]]
  })
  
  # all
  if (exists("name.boxes")) {
    
    final.data = list(prices = final.prices, name.locations = name.boxes[["locations"]],
                      name.words = name.boxes[["words"]], name.words.old = name.boxes[["words_old"]], page.cols = page.cols)
    
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

# 1. Example ----

#don't use this because not deskewed
#fullBoxes = readRDS("~/Documents/DSI/OCR_SherryLehmann/FullBoxes.rds")
#data1 = fullBoxes[[paste0(file1,".jpg")]]
#price_table_extraction(file1, image.check = FALSE, save.root = wd, data1 = data1) #pix.threshold = 150, pix.newValue = 0

file1 = "UCD_Lehmann_0011" #0455, 3392 
#checked 0069, 3943, 0066, 0011, 0237, 0190, 1452 (hard), 1802, 0644 (mixed ID types), 1176 (needs new color threshold?)
# 1591 (dollar sign used)
data1 = readRDS(paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))

#img1 = paste("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/", file1, ".jpg", sep = ""); px1 = deskew(pixConvertTo8(pixRead(img1)), binaryThreshold = 50);  height1 = dim(readJPEG(img1))[1] #note we'll use the image attribute here later
#plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1)

#debugonce(price_table_extraction)
price_table_extraction(file1, image.check = FALSE, save.root = wd, data1 = data1) #pix.threshold = 150, pix.newValue = 0,

# 2. Run on a fileset ----
fileset = str_extract(list.files("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages"), ".*[^\\.jpg]")
fileset = subset(fileset, str_detect(fileset, "UCD_Lehmann_[0-9]{4}"))
output = vector("list", length(fileset))
names(output) = fileset

i = 0; pix.threshold = NULL; pix.newValue = NULL
for (file1 in fileset) {
  i = i+1; cat(file1, i, "\n")
  img1 = paste("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/", file1, ".jpg", sep = "")
  if (!file.exists(img1)) {
    if (!exists("sample_files")) {sample_files = readRDS("~/Documents/DSI/wine-price-extraction/dsi/Data/sample_files.RDS")}
    folder = sample_files[which(sample_files$file == file1),"Sample"]
    img1 = paste("~/Documents/DSI/OCR_SherryLehmann/Sample/", folder, "/", file1, ".jpg", sep = "")
  }
  api = tesseract(img1, pageSegMode = 6, engineMode = 3)
  height1 = dim(readJPEG(img1))[1] #note we'll use the image attribute here later
  
  px1 = deskew(pixConvertTo8(pixRead(img1)))
  #data1 = fullBoxes[[paste0(file1,".jpg")]] # don't use this because not deskewed
  if(file.exists(paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))) {
    data1 = readRDS(paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))
  } else {data1 = GetBoxes(px1, pageSegMode = 6, engineMode = 3)}
  if (median(data1$confidence) > 30 & sum(isPrice(data1$text, dollar = FALSE)) >= 3) {
    try({output[[i]] = price_table_extraction(file1, image.check = FALSE, data1)})
  }
  if (is.null(output[[i]])) {
    output[[i]]= c("median_conf" = median(data1$confidence), "n_price" = sum(isPrice(data1$text, dollar = FALSE)))
  }
}

# 3. Results ----

# (Third round (beta 0) --> all 61 ran (up from about 1/6 and then 1/2)

### Analyze ###

# truth in "~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/"
# list.files("~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/")

file.number = "0011"
test.prices = readRDS(paste0("~/Documents/DSI/wine-price-extraction/dsi/Data/UCD_Lehmann_",file.number,".RDS"))$prices
test.prices.truth = readRDS(paste0("~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/UCD_Lehmann_",
              file.number,"_price_truth.RDS"))$prices

# From mine
test.prices$page.cols$price_cols = test.prices$page.cols$price_cols %>% arrange(table.row, col.left)
test.stat = list(n.tables = length(test.prices),
n.columns.per.table = sapply(test.prices, function(x) {length(x$prices)}),
n.entries.per.column = sapply(test.prices, function(x) {sapply(x$prices, nrow)})) # each column is a table

# Compare truth
truth.stat = list(n.tables = length(test.prices.truth),
n.columns.per.table = sapply(test.prices.truth, function(x) {length(x$prices)}),
n.entries.per.column = sapply(test.prices.truth, function(x) {sapply(x$prices, nrow)})) # each column is a table

# Where same number of entries, compare values

apply(which(test.stat$n.entries.per.column == truth.stat$n.entries.per.column, arr.ind = T), function(x) {
  
  # differences in dollar amounts
  as.numeric(test.prices.truth[[x[2]]]$prices[[x[1]]]$text.new) - 
    as.numeric(test.prices[[x[2]]]$prices[[x[1]]]$text.new)
  
  # digit differences or levenschtein?
  sapply(1:test.stat$n.entries.per.column[x[1],x[2]], function(i) {
    levenshteinDist(test.prices.truth[[x[2]]]$prices[[x[1]]]$text.new[i], 
                   test.prices[[x[2]]]$prices[[x[1]]]$text.new[i]) })
  
  lapply(1:test.stat$n.entries.per.column[x[1],x[2]], function(i) {
         strsplit(test.prices.truth[[x[2]]]$prices[[x[1]]]$text.new[i], split = "")[[1]] == 
         strsplit(test.prices[[x[2]]]$prices[[x[1]]]$text.new[i], split = "")[[1]]}) 
}

# Where same number of total rows, compare values
# Otherwise, list mismatch size


