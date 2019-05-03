# See R/wine_price_tables.R for overview of the workflow and To Do's associated with price_table_extraction function called below
# Jane Carlen
# Notes, assumes .jpg image files

####################################################################################################
# Setup ####
####################################################################################################

library(dplyr)
library(Rtesseract)
library(tidyverse)
library(stringr)
library(jpeg)
library(cluster)
library(changepoint)
library(RecordLinkage)
#library(MASS) #<- Should have MASS installed for rlm, but load creates conflict with select

source("wine-price-extraction/dsi/R/wine_price_pageCols.R") #redundant
source("wine-price-extraction/dsi/R/wine_price_tables_functions.R") #redundant
source("wine-price-extraction/dsi/R/wine_price_nameBoxes.R") #redundant
source("wine-price-extraction/dsi/R/helper.R") #redundant
source("wine-price-extraction/dsi/R/wine_price_tables.R")

FILESET = "/Users/janecarlen/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages"
OUTPUT.FOLDER = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data"
DATA.INPUT.FOLDER = "/Users/janecarlen/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed"
DATA.OUTPUT.FOLDER = "/Users/janecarlen/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed"
SAVE.DATA = FALSE

args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  FILESET = args[1]
  RUN.FILE = ifelse (grepl(FILESET, pattern = "\\.[a-zA-Z]+$"), TRUE, FALSE) #otherwise run folder
  if (length(args) > 1) {
    OUTPUT.FOLDER = args[2] 
    if (length(args) > 2) {
      DATA.INPUT.FOLDER = args[3] #path to folder where pre-ocr'd data is stored
      if (length(args) > 3)
        DATA.OUTPUT.FOLDER = args[4] #path to folder where ocr'd data will be saved if save.data is TRUE
    }
  }
}

if (!file.exists (FILESET) ) {
  stop("Path to image file or folder containing images not valid. Stopping.")
}

if (!file.exists (OUTPUT.FOLDER) ) {
  stop("Path to store output not valid. Using current directory instead")
}

if (!file.exists (DATA.INPUT.FOLDER) ) {
  DATA.INPUT.FOLDER = NULL
  DATA1 = NULL
  warning("path to existing data not valid, re-OCRing instead")
}

if (!file.exists (DATA.OUTPUT.FOLDER) ) {
  DATA.OUTPUT.FOLDER = NULL
  SAVE.DATA = FALSE
  warning("path to store data not valid, will not store")
}

####################################################################################################
# RUN ####
####################################################################################################

# 0. Notes on files run ----

# 0551, 3392 0260, 3001 (hard, lots of un-aligned tables), 
# 0208 & 0194 (hard, year columns)
# 4105 (unaligned price in table)
# 1591 (dollar sign used)
# 1544 (repeat col), 
# 2535 (needs new pix.threshold 100), 0939 -> if prices but not price tables, try with  pix.treshold 100 and 2000
# 2504 (many rotated)
# 1452 (hard, lots of text no IDs)
# 0644 (mixed ID types)
# 1835 (before fixing, too few words in name), 0008, 1470
# 0550 two types of IDs on one page

# Other possible filesets:

# pages with lots of marks
# marks_top_files = read.csv("wine-price-extraction/dsi/Data/marks_top_files.csv", stringsAsFactors = F)
# 0939.jpg needs higher threshold, eg  200

# pages we have truth for
# truth_files = unique(unlist(sapply(list.files("~/Downloads/price_id_truth_RDS_files", recursive = T), str_extract, pattern =   "UCD_Lehmann_[0-9]{4}"))) #downloaded then deleted
# truth_files = truth_files[-grep(truth_files, pattern = "2759")] #remove bad one
#-- files in truth not in sample folder: fileset = c("UCD_Lehmann_0008", "UCD_Lehmann_0027", "UCD_Lehmann_0267", "UCD_Lehmann_1470  ", "UCD_Lehmann_1994", "UCD_Lehmann_1544", "UCD_Lehmann_1835")

# 1. Example ----

if (RUN.FILE) {

file1 = FILESET
  
### will remove later ###
file1 = "UCD_Lehmann_0011"# 1106

if (paste0(file1, ".jpg") %in% list.files("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages")) {
  file1 = file.path("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages", paste0(file1, ".jpg"))
}  else {
  sample_files = readRDS("~/Documents/DSI/wine-price-extraction/dsi/Data/sample_files.RDS")
  folder = sample_files[which(sample_files$file == file1),"Sample"]
  file1 = file.path("~/Documents/DSI/OCR_SherryLehmann/Sample", folder, paste0(file1, ".jpg"))
}
###

if (!is.null(DATA.INPUT.FOLDER)) {
  DATA1 = readRDS(file.path(DATA.INPUT.FOLDER, paste0(nth(strsplit(FILESET, split = c("/|\\."))[[1]], -2), "_data1.RDS")))
}

price_table_extraction(file1,
                       data1 = DATA1,
                       output.folder = OUTPUT.FOLDER, 
                       data.output.folder = DATA.OUTPUT.FOLDER,
                       save.data = SAVE.DATA,
                       save.deskewed = FALSE,
                       pix.threshold = NULL, pix.newValue = NULL, #pix.threshold = 200, pix.newValue = 0
                       column.header = c("bottle", "case", "quart", "fifth", "half", "of", "24"),
                       res1 = 600,
                       image.check = FALSE, 
                       show.ggplot = TRUE) 

}

# 2. Run on a fileset ----

if (!RUN.FILE) {

  fileset = FILESET
  fileset = list.files(FILESET, full.names = T, pattern = "\\.") #only files, not folders
  PIX.THRESHOLD = NULL; PIX.NEWVALUE = NULL
  
  output = lapply(fileset, function(img1) {
    
    height1 = dim(readJPEG(img1))[1] #note we'll use the image attribute here later
    px1 = deskew(pixConvertTo8(pixRead(img1)))
    api1 = tesseract(px1)
    if( GetSourceYResolution(api1)==0 ) {SetSourceResolution(api1, 600)}
    
    # Should we use pre-run get boxes?
    if (!is.null(DATA.INPUT.FOLDER)) {
      DATA1 = readRDS(file.path(DATA.INPUT.FOLDER, paste0(nth(strsplit(img1, split = c("/|\\."))[[1]], -2), "_data1.RDS")))
    } else {
      # this is done here instead of in price table extraction so that we know whether to continue
      DATA1 = GetBoxes(px1, pageSegMode = 6, engineMode = 3)
    }
    
    # Use getBoxes output in data1 to classify if a page (probably) has readable price tables

    if (median(DATA1$confidence) > 30 & sum(isPrice(DATA1$text, dollar = FALSE)) >= 3) {
      output1 = try({ # names will tell us if it was successful, but output is stored expernally
        names( 
          price_table_extraction(img1, 
                                      data1 = DATA1,
                                      output.folder = OUTPUT.FOLDER, 
                                      data.output.folder = DATA.OUTPUT.FOLDER,
                                      save.data = SAVE.DATA,
                                      save.deskewed = FALSE,
                                      pix.threshold = PIX.THRESHOLD, pix.newValue = PIX.NEWVALUE, #pix.threshold = 200, pix.newValue = 0
                                      column.header = c("bottle", "case", "quart", "fifth", "half", "of", "24"),
                                      res1 = 600,
                                      image.check = FALSE, 
                                      show.ggplot = FALSE)
        )
      })
    } else {output1 = NULL}
  
    if (is.null(output1)) {cat("\nNO PRICE TABLES DETECTED IN", img1, "\n\n")}
    output1 = c(output1, "median_conf" = median(DATA1$confidence), "n_price" = sum(isPrice(DATA1$text, dollar = FALSE)))
   
    return(output1)
  })
  
  names(output) = fileset
  
}
