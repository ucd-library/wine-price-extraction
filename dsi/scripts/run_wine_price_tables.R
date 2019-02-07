# See R/wine_price_tables.R for overview of the workflow and To Do's associated with price_table_extraction function called below
# Jane Carlen

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

source("wine-price-extraction/dsi/R/wine_price_tables_functions.R") #redunant
source("wine-price-extraction/dsi/R/helper.R") #redundant
source("wine_price_tables.R")

####################################################################################################
# RUN ####
####################################################################################################

# 1. Example ----

file1 = "UCD_Lehmann_0027" #0455, 3392 
#checked 0069, 3943, 0066, 0011, 0237, 0190, 1452 (hard), 1802, 0644 (mixed ID types), 1176 (needs new color threshold?)
# 1591 (dollar sign used)
data1 = readRDS(paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))

#img1 = paste("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/", file1, ".jpg", sep = ""); px1 = deskew(pixConvertTo8(pixRead(img1)), binaryThreshold = 50);  height1 = dim(readJPEG(img1))[1] #note we'll use the image attribute here later
#plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1)

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

