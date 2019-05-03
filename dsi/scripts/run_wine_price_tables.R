# See R/wine_price_tables.R for overview of the workflow and To Do's associated with price_table_extraction function called below
# Jane Carlen

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

OUTPUT.FOLDER = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data"
DATA.INPUT.FOLDER = "~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed"
DATA.OUTPUT.FOLDER = "~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed"
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
  
  fileset = str_extract(list.files("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages"), ".*[^\\.jpg]")
  # files in truth not in sample folder: fileset = c("UCD_Lehmann_0008", "UCD_Lehmann_0027", "UCD_Lehmann_0267", "UCD_Lehmann_1470  ", "UCD_Lehmann_1994", "UCD_Lehmann_1544", "UCD_Lehmann_1835")
  
  # Other possible filesets:
  
  # pages with lots of marks
  # marks_top_files = read.csv("wine-price-extraction/dsi/Data/marks_top_files.csv", stringsAsFactors = F)
  # 0939.jpg needs higher threshold, eg  200
  
  # pages we have truth for
  # truth_files = unique(unlist(sapply(list.files("~/Downloads/price_id_truth_RDS_files", recursive = T), str_extract, pattern =   "UCD_Lehmann_[0-9]{4}"))) #downloaded then deleted
  # truth_files = truth_files[-grep(truth_files, pattern = "2759")] #remove bad one

  fileset = subset(fileset, str_detect(fileset, "UCD_Lehmann_[0-9]{4}"))
  output = vector("list", length(fileset))
  names(output) = fileset

  i = 0; pix.threshold = NULL; pix.newValue = NULL
  
  if (!file.exists(img1)) {
    if (!exists("sample_files")) {sample_files = readRDS("~/Documents/DSI/wine-price-extraction/dsi/Data/sample_files.RDS")}
    folder = sample_files[which(sample_files$file == file1),"Sample"]
    img1 = paste("~/Documents/DSI/OCR_SherryLehmann/Sample/", folder, "/", file1, ".jpg", sep = "")
  }
  
  for (file1 in fileset) {
  i = i+1; cat(file1, i, "\n")
  img1 = paste("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/", file1, ".jpg", sep = "")
  if (!file.exists(img1)) {
    if (!exists("sample_files")) {
      sample_files = readRDS("~/Documents/DSI/wine-price-extraction/dsi/Data/sample_files.RDS")
    }
    folder = sample_files[which(sample_files$file == file1),"Sample"]
    img1 = paste("~/Documents/DSI/OCR_SherryLehmann/Sample/", folder, "/", file1, ".jpg", sep = "")
  }
  height1 = dim(readJPEG(img1))[1] #note we'll use the image attribute here later
  
  px1 = deskew(pixConvertTo8(pixRead(img1)))
  api1 = tesseract(px1)
  if(GetSourceYResolution(api1)==0) {SetSourceResolution(api1, 600)}
  #data1 = fullBoxes[[paste0(file1,".jpg")]] # don't use this because not deskewed
  if(file.exists(paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))) {
    data1 = readRDS(paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))
  } else {
    # this seems to return the same results as applying GetBoxes to px1, but not warning message
    data1 = GetBoxes(api1, pageSegMode = 6, engineMode = 3)}
  if (median(data1$confidence) > 30 & sum(isPrice(data1$text, dollar = FALSE)) >= 3) {
    try({output[[i]] = price_table_extraction(file1, image.check = FALSE, data = data1, show.ggplot = FALSE, save.deskewed = TRUE)})
  }
  if (is.null(output[[i]])) {
    output[[i]]= c("median_conf" = median(data1$confidence), "n_price" = sum(isPrice(data1$text, dollar = FALSE)))
  }
}

  which(sapply(output, function(x) is.null(names(x[[1]]))))
  which(sapply(output, function(x) length(names(x[[1]])))!=5)

}