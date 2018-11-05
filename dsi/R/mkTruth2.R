# Jane Carlen
#
# GOAL: Polish the truth in "Truth1" which was created with mkTruth.R
# Truth1 is available in the DSL drive and here: http://dsi.ucdavis.edu/WineCatalogs/Truth1.tar.gz 
# You must set your "pathtotruth" and "pathtorepo" directory below accordingly
# 
# Output: 
# Files will output to a "Truth2" folder at the same level as Truth 1
# Standardize column names
# 1, 2, ... left to right
# Possible names: No, Case, Bottle, 
# Skipping description for now, can circle back
######################################################################################

# 0. Setup ####

pathtorepo = "~/Documents/DSI/WineCatalogs_forked_repo"
pathtotruth = "~/Documents/DSI/OCR_SherryLehmann/Truth1/"
  
setwd(pathtorepo)
cat_files = readRDS("Data/cat_files.RDS")

# 1. Pick an image ####

#filenum = sample(str_extract(list.files(pathtotruth), "[0-9]{4}"), 1) #picks randomly
filenum = "0008"
filename = paste0(pathtotruth, filenum, ".csv", sep="")

# which catalog is it in
which_cat = which(unlist(lapply(cat_files, function(x) {
  sum(grepl(paste(".*", filenum, sep=""), x$file.jpg))
}))>0)

# any other truth files in that catalog ?
# str_extract(as.character(cat_files[[which_cat]]$file.jpg), "[0-9]{4}") %in% str_extract(list.files(path = "Truth1"), "[0-9]{4}")

# 2. Look at the image
pageID = cat_files[[which_cat]][which(grepl(filenum, cat_files[[which_cat]]$file)), "page_id"]
catID = cat_files[[which_cat]][which(grepl(filenum, cat_files[[which_cat]]$file)), "catalog_id"]
browseURL(paste0("https://ptv.library.ucdavis.edu/#", catID, "/", pageID, sep=""))

# 3. fix the image & save -- ADD MORE CODE HERE FOR ALL IMAGES

###### 0008
truth1 = as.list(read.csv(filename, stringsAsFactors = F))
truth1$No. = c(truth1$No.[1:8], "A41", "A43", "A45", truth1$No.[11:14], "A61", truth1$No.[16:18])
truth1$Case = sort(c(truth1$Case, 55.75))
truth1$No..1 = c(truth1$No..1[1:8], "B41", "B43", truth1$No..1[10:18])
truth1$Case.1[c(2,7,9)] = c("35.75", "--", "41.75")
truth1$Case.1 = c(truth1$Case.1[1:9], "49.75", truth1$Case.1[10:18])
truth1 = as.list(data.frame(No1 = truth1$No., Case1 = truth1$Case, No2 = truth1$No..1, Case2 = truth1$Case.1))
# Save output to Truth2
saveRDS(truth1, paste0(pathtotruth,"..","/Truth2/",filenum,".RDS", sep = "")) #save as RDS in case of uneven row numbers for diff columns (csv won't work)

###### You can keep adding labels for fixed files...


######################################################################################
# IGNORE
##### fixed one more from the same catalog as 0008 (from scratch)
# filenum = "0027"
# truth2 = list()
# truth2$No1 = c("329", "668", "571", "250", "244", "338", "509", "224", "308", "315", "285", "259")
# truth2$Bottle1 = c("1.99", "2.49", "2.49", "3.49", "3.49", "3.69", "3.79", "3.79", "3.99", "3.99", "3.99", "3.99")
# truth2$Case1 = c("21.50", "26.90", "26.90", "37.70", "37.70", "39.85", "40.95", "40.95", "43.10", "43.10", "43.10", "43.10")
# truth2$No2 =  c("276", "440", "759", "5010", "403", "5033", "394", "372", "798", "1079")
# truth2$Bottle2 = c("4.49", "4.49", "4.79", "4.79", "5.79", "5.89", "5.99", "6.79", "12.50", "16.50")
# truth2$Case2 = c("48.50","48.50", "51.75", "51.75", "62.55", "63.60", "64.70", "74.75", "135.00", "178.00")
# saveRDS(truth2, paste0(pathtotruth,"..","/Truth2/",filenum,".RDS", sep = "")) #save as RDS in case of uneven row numbers for diff columns (csv won't work)

