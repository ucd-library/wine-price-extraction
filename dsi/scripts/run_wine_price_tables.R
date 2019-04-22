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


wd = "~/Documents/DSI" #path to wine-price-extraction repo
setwd(wd)

source("wine-price-extraction/dsi/R/wine_price_tables_functions.R") #redundant
source("wine-price-extraction/dsi/R/helper.R") #redundant
source("wine-price-extraction/dsi/R/wine_price_tables.R")

####################################################################################################
# RUN ####
####################################################################################################

# 1. Example ----

file1 = "UCD_Lehmann_0190" #0551, 3392 #0260 & 3001 (hard, lots of un-aligned tables), 0208 & 0194 (hard, year columns)
 #checked 0069, 3943, 0066, 0011, 0237, 0190, 1452 (hard), 1802, 0644 (mixed ID types), 1176, 0015, 0939 (needs new color threshold?)
# 1591 (dollar sign used), 1544 (repeat col), 
# 2535 (needs new pix.threshold 100) -> if prices but not price tables, try with  pix.treshold 100 and 2000
# 2504 (many rotated)
# too few words in name 1835!, 0008, 1470

data1 = readRDS(paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))


#img1 = paste("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/", file1, ".jpg", sep = ""); px1 = deskew(pixConvertTo8(pixRead(img1)), binaryThreshold = 50);  height1 = dim(readJPEG(img1))[1] #note we'll use the image attribute here later
#plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1)

price_table_extraction(file1, image.check = FALSE, save.root = wd, res1 = 600, show.ggplot = TRUE, data = data1, 
                       save.deskewed = FALSE) #pix.threshold = 200, pix.newValue = 0


# 2. Run on a fileset ----
fileset = str_extract(list.files("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages"), ".*[^\\.jpg]")
# files in truth not in sample folder: 0008, 0027, 0267, 1470, 1994, 1544, 1835

fileset = subset(fileset, str_detect(fileset, "UCD_Lehmann_[0-9]{4}"))
output = vector("list", length(fileset))
names(output) = fileset

i = 0; pix.threshold = NULL; pix.newValue = NULL
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
    try({output[[i]] = price_table_extraction(file1, image.check = FALSE, data = data1, show.ggplot = FALSE, save.deskewed = FALSE)})
  }
  if (is.null(output[[i]])) {
    output[[i]]= c("median_conf" = median(data1$confidence), "n_price" = sum(isPrice(data1$text, dollar = FALSE)))
  }
}

which(sapply(output, function(x) length(x[[1]]))==1)

# 3. Run on a second fileset (pages with lots of marks) ----

#0939.jpg needs higher threshold, eg  200
marks_top_files = read.csv("wine-price-extraction/dsi/Data/marks_top_files.csv", stringsAsFactors = F)

fileset2 = gsub(marks_top_files$file.jpg, pattern = "\\.jpg", replacement = "")
output2 = vector("list", length(fileset2))
names(output2) = fileset2


# # Run with save deskew for files we have truth for
# library(XML)
# library(stringr)
# truth_files = unique(unlist(sapply(list.files("~/Downloads/price_id_truth_RDS_files", recursive = T), str_extract, pattern = "UCD_Lehmann_[0-9]{4}"))) #downloaded then deleted
# truth_files = truth_files[-grep(truth_files, pattern = "2759")] #remove bad one
# output_truth = vector("list", length(truth_files))

i = 0; pix.threshold = NULL; pix.newValue = NULL
for (file1 in truth_files[41:48]) {
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
  
  if (file.exists(paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))) {
    data1 = readRDS(paste0("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed/",file1,"_data1.RDS"))
  } else {
    # this seems to return the same results as applying GetBoxes to px1, but no warning message
    # don't do if we need to change pix threshold
    if (! file1 %in% c("UCD_Lehmann_0939")) {data1 = GetBoxes(api1, pageSegMode = 6, engineMode = 3)}
  }
  
  # Checks that it's probably a price table page and then runs
  if (median(data1$confidence) > 30 & sum(isPrice(data1$text, dollar = FALSE)) >= 3) {
    
    # different settings if resoluation has to be adjusted
    if (! file1 %in% c("UCD_Lehmann_0939")) {
      try({output2[[i]] = price_table_extraction(file1, image.check = FALSE, data1 = data1, show.ggplot = FALSE, save.deskewed = TRUE)})
    } else {
      try({output2[[i]] = price_table_extraction(file1, image.check = FALSE, show.ggplot = FALSE, pix.threshold = 200, pix.newValue = 0, save.deskewed = TRUE)})
    }
  }
  
  if (is.null(output2[[i]])) {
    output2[[i]]= c("median_conf" = median(data1$confidence), "n_price" = sum(isPrice(data1$text, dollar = FALSE)))
  }
}

which(sapply(output2, function(x) {length(x[[1]])})==1)

