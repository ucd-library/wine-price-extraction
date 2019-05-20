# Extracting accurate prices from tables

# TO DO
# (short term)
#   based on truth making -> code to eliminate duplicates in DIFFERENT tables/merge columns vertically (0027)
#   better decision-making between years and prices missing periods (e.g $19.50 vs. 1950). See isPrice.
# (after benchmark)
#   resolve code differences between dynamic name box size in id vs. non-id case. which is better?
#   make the search for “stuff” (words) between columns in pageTables better 
#   switch paste statements and file locations to universal (e.g. use file.path and image argument)
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
library(jpeg)
library(cluster)
library(stringr)
library(changepoint)
library(RecordLinkage)

#source("wine_price_pageCols.R")
#source("wine_price_tables_functions.R")
#source("wine_price_nameBoxes.R")
#source("helper.R") 

# MAIN FUNCTION, encompassing all numbered steps below ----
# for examples of this code being run see run_wine_price_tables.R in the adjacent scripts folder

price_table_extraction <- function(file1,
                                   data1 = NULL,
                                   output.folder = ".",
                                   data.output.folder = NULL,
                                   save.data = FALSE,
                                   save.deskewed = FALSE,
                                   pix.threshold = NULL, pix.newValue = NULL, #pix.threshold = 200, pix.newValue = 0
                                   binary.threshold = 150,
                                   ocr.only = FALSE,
                                   column.header = c("bottle", "case", "quart", "fifth", "half", "of", "24"),
                                   res1 = 600,
                                   image.check = FALSE, 
                                   show.ggplot = TRUE) {
  # res1 is default resolution which is 600 for wine images. Only set if img resolution is missing.
  # column.header is convered to lower for comparison
  # save.data will save the output of GetBoxes to the data.output.folder
  # save.deskewed will save the clean deskewed image to the output.folder
  # pix.threshold and pix.newValue effectively set the binary cutoff for OCR, and matter, e.g., if color on page
  # binary threshold sets the binaryThreshold argument for deskew(ing).
    # Can change angle slightly but often doesn't matter much
    # Sticking with default of 150 for now -- improves examples like 4043 and 3737
  
  cat("************** setup (0-1) **************\n")
  
  
  # 0 Setup ####
  
  print(file1)
  
  # Check image name input
  if (file.exists(file1)) {
    img1 = file1
    file1 = strsplit(basename(file1), "\\.")[[1]][1] #file1 now is stub name
  } else {
    stop("Image does not exist. Must supply valid path to image as as file1 argument.")
  }
  
  image.dims = attributes(readJPEG(img1))$dim #note we hope to use Rtesseract attribute here later so we can skip this
  height1 = image.dims[1]
  width1 = image.dims[2]
  
  ############ img check 1 ####
  api0 = tesseract(img1, pageSegMode = 6, engineMode = 3)
  
  if(image.check) {
    gb1 = GetBoxes(api0)
    prices1 = gb1[isPrice(gb1$text),] 
    plot(api0, cropToBoxes = F, bbox = prices1)
  }
  # tried removing low-confidence prices -- removed real prices, so don't do that
  
  # 1 ####
  
  # pre- process the image by deskewing and thresholding if necessary
  px1 = pixRead(img1)
  angle1 = pixFindSkew(pixThresholdToBinary(pixConvertTo8(pixRead(img1)), binary.threshold))
  
  px1 = deskew(px1, binaryThreshold = binary.threshold)
  
  if (!is.null(pix.threshold) & !is.null(pix.newValue)) {
    px1 = pixThresholdToValue(px1, pix.threshold, pix.newValue)
  }
  
  # Set resolution to avoid warnings
  api1 = tesseract(px1)
  if(GetSourceYResolution(api1)==0) {SetSourceResolution(api1, res1)}
  
  if (is.null(data1)) {
    data1 = GetBoxes(api1, pageSegMode = 6, engineMode = 3)
  }
  
  # Add angle to data1 attributes so that it's stored even if we ocr.only is TRUE
  attr(data1, "imageDims") = c(attr(data1, "imageDims"), 
                        angle1 = angle1[1], angle2 = angle1[2])
  
  attr(data1, "imageThresholds") = c(binaryThreshold = binary.threshold,
                                     pixThreshold = pix.threshold,
                                     pixNewValue = pix.newValue)
  
  if (save.data) {
    saveRDS(data1, file.path(data.output.folder, paste0(file1,"_data1.RDS")))
  }
  
  if (ocr.only == TRUE) { return(data.frame(result = paste("OCR_ONLY:", file1)) )}
  
  # may want to save deskewed image for post-processing
  if (save.deskewed) {
    pixWrite(px1, file.path(output.folder, paste0(file1, "_deskew.png")))
    #return()
  }
  
  if (sum(isPrice(data1$text)) + sum(isPrice(data1$text, maybe = T)=="*price") <= 1) {
    return("No price tables detected. (At most 1 price detected.) If prices suspected try a new pix.threshold.")
  }
  
  # 2 ####
  cat("****** get price and id columns (2) *****\n")
  page.cols = pageCols(data1, img.height = height1, column.header = column.header, show.plot = show.ggplot)
  
  if (is.null(page.cols)) {
    return("Prices are scattered. No price *tables* detected")
  }
  
  ############# img check 2 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1)
  if(image.check & !is.null(page.cols$ids)) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1)
  
  # 3 ####
  cat("\n******* add prices and ids (3) **********\n")
  # check for new prices ####
  page.cols = addPrices(page.cols, px1)
  
  # check for new ids ####
  if (!is.null(page.cols$ids)) { page.cols = addIds(page.cols, px1) }
  
  ############# img check 3 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  if(image.check & !is.null(page.cols$ids)) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1, confidence = FALSE)
  
  ############# table check 1 ####
  if (!is.null(page.cols$ids)) {
    cat("We think there's", page.cols$n_id_cols, "table(s)\n")
    tmp.maybe_missing = rep(page.cols$id_cols$entries, each = page.cols$n_price_cols/page.cols$n_id_cols) - sapply(page.cols$prices, nrow)
    if (max(tmp.maybe_missing) > 0) {
      cat("Column(s)", which(tmp.maybe_missing>0), "may be missing", tmp.maybe_missing[tmp.maybe_missing>0], "# of entry\n")
    } else if (min(tmp.maybe_missing) < 0) {
      cat("ID column(s) may be missing up to", -1*min(tmp.maybe_missing), "entry(s)")
    } else {cat("Columns don't seem to be missing entries\n")}
  }
  
  # 4 ####
  cat("****** get page table structure (4) ******\n")
  # Find table locations using changepoint method, return tables item with table, border and column locations
  page.cols = pageTables(data1, page.cols, buffer = page.cols$charheight/3)
  
  ############# table check 2 ####
  # Check that left edge based on ID columns, if present, equals changepoint method (page.tables) left-edge
  # Default to method based on ID columns
  if (!is.null(page.cols$ids)) {
    tmp.left.diffs = page.cols$id_cols$col.left - page.cols$price_cols$table.left.cpt
    cat("\nDifference in left-edge detected using ID col method vs. changepoint method",
        tmp.left.diffs)
    cat("\nThis is", tmp.left.diffs/page.cols$charheight,  "times estimated character height\n")
    if(max(100*(tmp.left.diffs)/page.cols$charheight) > 100) {cat("Mistake? Greater than character's difference\n")}
  }
  
  # 5 ####
  cat("*** fill in missing/remove extra (5) *****\n")
  page.cols = removeExtra(page.cols, removeType = "all", charwidth.cutoff = 2) #default remove type is prices
  
  page.cols = addMissing(page.cols, buffer = page.cols$charheight, img.height = height1, px1)
  
  page.cols = removeExtra(page.cols, removeType = "all") 
  
  # final attempt to fix price formatting before saving
  page.cols$prices = lapply(page.cols$prices, function(table.col) {
    can.convert.to.price = which(isPrice(extractPrice(table.col$text.new), maybe = FALSE) & 
                                   !isPrice(table.col$text.new, maybe = FALSE))
    if (length(can.convert.to.price) > 0) {
      table.col$text.new[can.convert.to.price] = extractPrice(table.col$text.new)[can.convert.to.price]
    }
    return(table.col)
  })
  
  
  #store image reading settings
  page.cols$pix.threshold = pix.threshold
  page.cols$pix.newValue = pix.newValue
  page.cols$binary.threshold = binary.threshold
  page.cols$angle = angle1
  page.cols$height_orig = height1
  page.cols$width_orig = width1
  
  ############# img check 4 ####
  if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  if(image.check & !is.null(page.cols$ids)) plot(tesseract(px1), cropToBoxes = F, bbox = page.cols$ids, img = px1, confidence = FALSE)
  
  # 6 (this will fail if no words in the name) #### 
  cat("************** get names (6) *************\n")
  
  try({name.boxes = nameBoxes(data1, page.cols = page.cols, prices = page.cols$prices, px = px1, 
                              buffer = page.cols$charheight/2, text.level = "word", psm = 3)}) #will want to experimetn with textline vs. word here - textline good when words split by spaces when they shouldn't be
  
  
  # 7 ####
  
  cat("*********** save final data (7) **********\n")
  
  # ids (if there) and prices
  final.prices = vector("list", max(page.cols$price_cols$table))
  final.prices = lapply(1:max(page.cols$price_cols$table), function(x) {
    
    table.prices = subset(page.cols$prices, sapply(page.cols$prices, function(x) {x$table[1]}) == x)
    # within table, make sure columns ordered from left to right
    table.prices = table.prices[order(sapply(table.prices, function(x) {min(x$left)}))] 
    
    final.prices[[x]] = list(
      ids = ifelse(!is.null(page.cols$ids) && sum(page.cols$ids$table == x) > 0,
                   list(page.cols$ids %>% filter(table == x) %>% dplyr::select(row, text)),
                   list(NULL)) ,
      prices = lapply(table.prices, dplyr::select, row, text.new))
    
    names(final.prices[[x]]$prices) = (filter(page.cols$price_cols, table == x) %>% 
                                         arrange(col.left))[["col.header"]]
    final.prices[[x]]
  })
  
  # all
  if (exists("name.boxes")) {
    
    final.data = list(prices = final.prices, name.locations = name.boxes[["locations"]],
                      name.words = name.boxes[["words"]], name.words.old = name.boxes[["words_old"]], page.cols = page.cols)
    
    #### save name box image ####
    tmp.boxes = do.call("rbind", name.boxes[[1]])
    png(file.path(output.folder, paste0(file1, "_name_boxes.png")), width = 1000, height = 1500)
    plot(tesseract(px1), cropToBoxes = F, bbox = tmp.boxes, img = px1, confidence = FALSE)
    dev.off()
    
  } else {
    final.data = list(prices = final.prices)
  }
  
  saveRDS(final.data, file.path(output.folder, paste0(file1, ".RDS")))
  
  #### save price image ####
  png(file.path(output.folder, paste0(file1, "_price_boxes.png")), width = 1000, height = 1500)
  #page.cols$prices entries should only have class data.frame, not also tbl_df or other
  plot(tesseract(px1), cropToBoxes = FALSE, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
  dev.off()
  
  return(list(final.data = final.data, page.cols = page.cols))
}