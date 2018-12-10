# Extracting accurate prices from tables

# TO DO
# add dynamic name box size for non-id pages (nameBoxes)
# addMissing can be streamlined, fix so it doesn't break if run twice, finish missings ids once i see an example
# smarter buffer
# get bottle and case info if not easily caught above table
# case if column is exactly vertical?
# make column recognition incorporate text justification
# cluster tops and bottoms if multiple charts on page
# incorporate confidence in compareTypes?


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
source("wine-price-extraction/dsi/R/helper.R") #sourced from above

####################################################################################################
# RUN ####
####################################################################################################
# preprocess ####

price_table_extraction <- function(file1, image.check = FALSE, data1 = NULL) {
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
  px1 = deskew(pixConvertTo8(pixRead(img1)))
  if  (is.null(data1)) {data1 = GetBoxes(px1, pageSegMode = 6, engineMode = 3)}
  #data1[isPrice(data1$text),]
  
  # 2 ####
  page.cols = pageCols(data1, img.height = height1)
  cat(length(page.cols), "test")
  ############# img check 2 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1)
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1)
  
  # 3 ####
  
  # check for new prices and return updated page.cols ####
  page.cols = addPrices(page.cols) 
  
  # check for new ids ####
  if (!is.null(page.cols$ids)) {
    page.cols = addIds(page.cols)
  }
  
  
  ############# img check 3 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1, confidence = FALSE)
  
  #plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", prices), img = px1)
  
  ############# table check 1 ####
  if (!is.null(page.cols$ids)) {
    cat("We guess there's", page.cols$n_id_cols, "table(s)\n")
    tmp.maybe_missing = rep(page.cols$id_cols$entries, each = page.cols$n_price_cols/page.cols$n_id_cols) - sapply(page.cols$prices, nrow)
    if (max(tmp.maybe_missing) > 0) {
      cat("Column(s)", which(tmp.maybe_missing>0), "may be missing", tmp.maybe_missing[tmp.maybe_missing>0], "# of entry")
    } else {cat("Columns don't seem to be missing entries")}
  }
  
  # 4 ####
  
  # Find table locations using changepoint method 
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
    cat("This is",  100*(tmp.left.diffs)/page.cols$charheight,  "percent of estimated character height")
    if(max(100*(tmp.left.diffs)/page.cols$charheight) > 100) {cat("Error?? Greater than character's difference")}
  }
  
  # 5 ####
  
  page.cols = removeExtra(page.cols, removeType = "ids", charwidth.cutoff = 2) #default remove type is prices
  
  page.cols = addMissing(page.cols, buffer = page.cols$charheight)
  
  page.cols = removeExtra(page.cols, removeType = "all") 
  
  ############# img check 4 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1, confidence = FALSE)
  
  # 6 ####
  
  name.boxes = nameBoxes(data1, page.cols = page.cols, prices = page.cols$prices, px = px1, 
                         buffer = page.tables$charheight/2, page.tables = page.tables,
                         text.level = "word", psm = 3) #will want to experimetn with textline vs. word here - textline good when words split by spaces when they shouldn't be
  
  # 7 ####
  
  # ids (if there) and prices
  final.prices = vector("list", max(page.cols$price_cols$table))
  final.prices = lapply(1:max(page.cols$price_cols$table), function(x) {
    final.prices[[x]] = list(
      if (!is.null(page.cols$ids)) {ids = unlist(page.cols$ids %>% filter(table == x) %>% .$text)},
      prices = lapply(subset(page.cols$prices, page.cols$price_cols$table == x), 
                      function(y) y$text.new)
    )
    names(final.prices[[x]]$prices) = subset(page.cols$price_cols, page.cols$price_cols$table == x)$bottle.or.case
    final.prices[[x]]
  })
  
  
  # all
  final.data = list(prices = final.prices, name.locations = name.boxes[["locations"]],
                    name.words = name.boxes[["words"]], name.words.old = name.boxes[["words_old"]])
  
  setwd(wd)
  saveRDS(final.data, paste("wine-price-extraction/dsi/Data/", file1, ".RDS", sep = ""))
  
  ################## check 4 / share image####
  #make fake confidence for box colors
  
  tmp.boxes = do.call("rbind", name.boxes[[1]])
  png(paste("wine-price-extraction/dsi/Data/", file1, "_name_boxes.png", sep=""), width = 1000, height = 1500)
  plot(tesseract(px1), cropToBoxes = F, bbox = tmp.boxes, img = px1, confidence = FALSE)
  dev.off()
  png(paste("wine-price-extraction/dsi/Data/", file1, "_price_boxes.png", sep=""), width = 1000, height = 1500)
  plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  dev.off()
  
  #png(paste("wine-price-extraction/dsi/Data/", file1, "_deskew.png", sep=""), width = 4000, height = 6000)
  #plot(tesseract(px1), img = px1)
  #dev.off()
  
  return(final.data)
}

price_table_extraction(file1 = "UCD_Lehmann_3392")
file1 = "UCD_Lehmann_3392" #0011, 1106, 0237, 3392, 1452 (hard), 0069  
  