# File to convert available information to usable training data
# Jane Carlen
# Created: 9-27-18
#
########################################################################################################################
#
# Notes:
# Images for Sample 30 are missing
#   ERROR: http://dsi.ucdavis.edu/WineCatalogs/Sample30.tar.bz2 -- not found!
#   The tesseract output is there though
# Discontinuing this approach because I see no easy, automated way to align mark positions with tesseract output boxes
#    mark boxes only have an xy coordinate, not a box size
#
########################################################################################################################
# 0. Setup ####
dir1 = "~/Documents/DSI/OCR_SherryLehmann/"
setwd(dir1)
tessdata_train = "~/Documents/DSI/ocrd-train-master/data/train"

library(Rtesseract)
library(readxl)
library(dplyr)
library(R.utils)
library(magick)
library(stringr)
library(ggplot2)

########################################################################################################################

# 1. Links file to the sample folder it's in (sample_files) -- did help ####
#    Links mark information from spreadsheets to images (marks_image) -- didn't help ####

marks = read_xlsx("Sherry Lehmann Crosswalk Page to Catalog.xlsx", sheet = "marks")
pages = read_xlsx("Sherry Lehmann Crosswalk Page to Catalog.xlsx", sheet = "pages")
page_xwalk = read_xlsx("Sherry Lehmann Crosswalk Page to Catalog.xlsx", sheet = "page_xwalk")
catalogs = read_xlsx("Sherry Lehmann Crosswalk Page to Catalog.xlsx", sheet = "catalogs")
catalog_crosswalk = read_xlsx("Sherry Lehmann Crosswalk Page to Catalog.xlsx", sheet = "catalog_crosswalk")
marks = left_join(marks, pages, by = "page_id")
marks = left_join(marks, catalogs, by = "catalog_id")


########################################################################################################################
##   Downloand images to re-organize by catalog: ####
# sample_files = paste("http://dsi.ucdavis.edu/WineCatalogs/Sample", 1:41, ".tar.bz2", sep = "")
# dest_file = paste("~/Downloads/Sample", 1:41, ".tar.bz2", sep = "")
# i = 1
# for (elem in sample_files) {
#     download.file(elem, dest_file[i])
#     i = i + 1
# }
## then I moved and decompressed them from the finder

##   Re-organize downloaded files by where they appeared (sample_files connects img to sample) ####
# where's 30 yo?

dest_folders = paste("~/Documents/DSI/OCR_SherryLehmann/Sample/Sample", c(1:29,31:41), sep = "")
list_files = lapply(dest_folders, list.files, pattern = ".*\\.jpg")
sample_files = data.frame(Sample = rep(paste("Sample", c(1:29,31:41), sep = ""), times = sapply(list_files, length)),
                          Sample_order =  unlist(sapply(sapply(list_files, length), seq)),
                          file = unlist(list_files))
sample_files$file.jpg = sample_files$file
sample_files$file = str_extract(as.character(sample_files$file), ".*(?=\\.)")
image_cat = left_join(sample_files, page_xwalk, by = "file")
sum(is.na(image_cat$catalog_id))
image_cat = image_cat %>% arrange(catalog_id)

marks_image = left_join(marks, image_cat, by = c("page_id" = "page_id"))
marks_image = select(marks_image, -contains(c("completed")))
marks_image = select(marks_image, -contains("created"))
marks_image = select(marks_image, -contains("editable"))
marks_image = select(marks_image, -contains("updated"))
marks_image = select(marks_image, -contains("user"))
marks_image = as.data.frame(marks_image)

sample_files[which(sample_files$file == "UCD_Lehmann_1835"),]
sample_files$Sample = as.character(sample_files$Sample)

#saveRDS(sample_files, "sample_files.RDS")

##   Can use marks_image to the find the sample that the image is in. eg ####
# (But note the order of images in the Sample.rds files is NOT the same as in the image folders?)

Sample26 = readRDS("Sample/Sample_tessGetBoxes/Sample26.rds")
#names(Sample26) = paste(names(Sample26), " ", 1:length(Sample26), sep = "")
names(Sample26) = as.vector(sapply(names(Sample26), function(x) strsplit(x, "\\.")[[1]][1]))
Sample26 = do.call(rbind, Sample26)
Sample26$file = str_extract(rownames(Sample26), ".*(?=\\.)")
#have to index from file by name

##   Catalogs with the top 10 most marks ####
mark_pages10 = subset(marks, marks$catalog_id %in% names(sort(table(marks$catalog_id), decreasing = T)[1:10]))


########################################################################################################################

# 2. Truth (Duncan's training data) ####

# Helpful for evaluating whole tables. Values are disconnected from positions so not helpful for individual word accuracy.

# see mkTruth2.R

########################################################################################################################

# 3. Make training files (function) ####

# make corresponding image (.tiff -> tif) and text truth (gt.txt) files for training
# I took ones with high-confidence and spot checked that they were right

make_box_tiff <- function(img1 = NULL, sample1 = NULL, boxes = NULL, basedir = "Sample", fixdir = NULL, conf = 90, conf.upper = NULL, level = "word",
                          write = T, pattern = NULL, deskew = F) {
  
  img1.path = paste0(basedir, "/", sample1, "/", img1, ".jpg",sep = "", collapse = "")
  
  test1.api = tesseract(img1.path)
  if (is.null(boxes)) {
    test1.boxes = GetBoxes(test1.api, level = level)
  } else {test1.boxes = boxes}
  
  hist(test1.boxes$confidence, breaks = 20)
  
  if (level == "textline") {bump = 2} else {bump = 0} #so single character with \n getx caught if type is textline
  test1.confboxes = dplyr::filter(test1.boxes, confidence > conf[1] & (nchar(test1.boxes$text) >= (2+bump) )) #found some issues with length-1 boxes, e.g. graphics being interpreted as vertical slashes and dots as i's
  
  if (!is.null(conf.upper)) {
    test1.confboxes = dplyr::filter(test1.confboxes, confidence < conf.upper) }
  
  if (!is.null(pattern)) {
    test1.confboxes = dplyr::filter(test1.confboxes, grepl(pattern, test1.confboxes$text))
  }
  
  #create directories to save into if necessary
  if (!R.utils::isDirectory(paste0(basedir, "/", sample1, "/tiff"))) {
    mkdirs(paste0(basedir, "/", sample1, "/tiff"))
  }
  if (!R.utils::isDirectory(paste0(basedir, "/", sample1, "/text"))) {
    mkdirs(paste0(basedir, "/", sample1, "/text"))
  }
  #write .tiff and corresponding .gt.txt files
  if(write) {
    
    # if we need to put the files in a tmp location (probably to fix them before adding to training data)
    if(!is.null(fixdir)) {
      basedir = fixdir
      if (!R.utils::isDirectory(paste0(basedir, "/", sample1, "/tiff"))) {
        mkdirs(paste0(basedir, "/", sample1, "/tiff"))
      }
      if (!R.utils::isDirectory(paste0(basedir, "/", sample1, "/text"))) {
        mkdirs(paste0(basedir, "/", sample1, "/text"))
      }
    } 
    
    if (deskew) {
      test1.api.crop = tesseract(deskew(img1.path))
      pixWrite(deskew(img1.path), file = "img1temp", format = "JPG")
      img1.read = image_read("img1temp")
      #workoround so that cropped boxes are from deskewed, makes them B&W which seems OK
    } else { 
      img1.read = image_read(img1.path)
      test1.api.crop = tesseract(img1.path)
    }
    
    sapply(1:nrow(test1.confboxes), function(y) {
      x = test1.confboxes[y,]

      SetRectangle(test1.api.crop, dims = c(x$left, x$bottom, x$right-x$left, x$top-x$bottom))
      test1.box = GetBoxes(test1.api.crop, level = level)
      tmp.box = image_crop(img1.read, geometry_area(x$right-x$left, x$top-x$bottom, x$left, x$bottom),
                           repage = TRUE)
      #save tiff
      tiff1.path = paste0(basedir, "/", sample1, "/tiff/", img1,
                          "_", x$left, "_", x$bottom, "_", x$right, "_", x$top, ".tiff",
                          sep = "", collapse = "")

      image_write(tmp.box, tiff1.path, format = "tiff")
      #save text
      text1.path = paste0(basedir, "/", sample1, "/text/", img1,
                        "_", x$left, "_", x$bottom, "_", x$right, "_", x$top, ".gt.txt",
                        sep = "", collapse = "")
      write(x$text, text1.path)
    })
  }
  return(test1.confboxes)
}

# I check about 20 of these and almost all were correct
# Saw some errors with one-character text, e.g. i and | so I omitted all one-character boxes to be safe

########################################################################################################################

# 4. Make training files (examples) ####

##   test on some images from the most-marked catalogs: ####
sort(table(mark_pages10$catalog_id), decreasing = TRUE)[1:3]

##    catalog 1 ####
which(marks_image$catalog_id.y == "affdd39b-2e6b-4f1a-b605-6dcdd522842d")
marks_image[.Last.value,]; table(.Last.value$file)

img1 = "UCD_Lehmann_0016" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 96.8)
make_box_tiff(img1, sample1, conf = 95, level = "textline")

img1 = "UCD_Lehmann_0040" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 96.8)
make_box_tiff(img1, sample1, conf = 95, level = "textline")

img1 = "UCD_Lehmann_0032" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 96.8)
make_box_tiff(img1, sample1, conf = 95, level = "textline")

img1 = "UCD_Lehmann_0009" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 97)
make_box_tiff(img1, sample1, conf = 95, level = "textline")

##    catalog 2 ####
which(marks_image$catalog_id.y == "00dd420b-02ee-4ead-aa01-c939f189542f")
marks_image[.Last.value,]; table(.Last.value$file)

img1 = "UCD_Lehmann_0419" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 96.8)

img1 = "UCD_Lehmann_0422" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 96.8)

img1 = "UCD_Lehmann_0431" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 96.8)

##    catalog 3 ####
which(marks_image$catalog_id.y == "61593f99-fc2f-42cc-8748-5c79ca7a44ae")
marks_image[.Last.value,]; table(.Last.value$file)

img1 = "UCD_Lehmann_2264" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 96.8)

img1 = "UCD_Lehmann_2266" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 96.8)

img1 = "UCD_Lehmann_2267" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 96.8)

### move results to training folders ####
#make sure no other tiff or text files in the way
file.copy(from = list.files(path = ".", pattern = ".*\\.tiff$", recursive = T),
          to = gsub(x = list.files(path = ".", pattern = "\\.tiff", recursive = T), "\\.tiff", ".tif"))
file.copy(list.files(path = ".", pattern = ".*\\.tif$", recursive = T), tessdata_train)

file.copy(list.files(path = ".", pattern = ".*\\.gt.txt$", recursive = T), tessdata_train)

# noticed in terminal the new data makes it better on prices but worse on spaces now

########################################################################################################################

# 5. Make training files that need fixing -- catch hard cases ####

#confs between 0 and 10 seem to catch lots of non-text

img1 = "UCD_Lehmann_0016" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 10, conf.upper = 70, write = T, fixdir = "Fix", pattern = "8")

img1 = "UCD_Lehmann_0419" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 10, conf.upper = 70, write = T, fixdir = "Fix", pattern = "3|8")

img1 = "UCD_Lehmann_2264" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 10, conf.upper = 80, write = T, fixdir = "Fix", pattern = "3|8")

img1 = "UCD_Lehmann_2266" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 10, conf.upper = 90, write = T, fixdir = "Fix", pattern = "3|8")

img1 = "UCD_Lehmann_2267" ; sample1 = as.character(sample_files[img1==sample_files$file, "Sample"]); print(sample1)
make_box_tiff(img1, sample1, conf = 10, conf.upper = 90, write = T, fixdir = "Fix", pattern = "3|8")

#make sure no other tiff or text files in the way
file.copy(from = paste0("Fix/", list.files(path = "Fix", pattern = "\\.tiff$", recursive = T)),
          to = paste0("Fix/",gsub(x = list.files(path = "Fix", pattern = "\\.tiff", recursive = T), "\\.tiff", ".tif")))

file.copy(paste0("Fix/", list.files(path = "Fix", pattern = ".*\\.tif$", recursive = T)), tessdata_train)

file.copy(paste0("Fix/", list.files(path = "Fix", pattern = ".*\\.gt.txt$", recursive = T)), tessdata_train)

#Last step was to manually fix these examples so they would represent truth


########################################################################################################################
# 6. Try training on a catalog's easy cases to then get the rest of the catalog ####

# 6a. Setup ####

#assumes we're in OCR_ directory

sample_files = readRDS("sample_files.RDS")

# cat_files = inner_join(sample_files, page_xwalk, by = "file") %>%
#   select(c("catalog_id", "Sample", "file.jpg")) %>% arrange(catalog_id) %>% 
#   left_join(catalogs[,c("catalog_id", "title")], by = "catalog_id") %>%
#   left_join(sample_files) %>%
#   left_join(page_xwalk)
# cat_files = split(cat_files, cat_files$title)

#saveRDS(cat_files, "~/Documents/DSI/WineCatalogs_forked_repo/R/cat_files.RDS")

cat_files = readRDS("cat_files.RDS")

# 6b. Pick a file (and catalog) ####

filenum = "0008"
which_cat = which(unlist(lapply(cat_files, function(x) {
  sum(grepl(paste(".*", filenum, sep=""), x$file.jpg))
}))>0)

# look at text distribution from this catalog:
#cat_textTypes = textTypes(cat_files, catalog = which_cat)
#cat_files[which_cat][[1]][is.na(cat_textTypes$price) | cat_textTypes$price < 4,]

# 6c. GetBoxes on all catalog images (results in cat.text) ####

cat.images = cat_files[which_cat][[1]]
cat.text = vector("list", nrow(cat.images))
for(i in 1:nrow(cat.images)) {
    print(i)
    y = cat.images[i,]
    cat.text[[i]] = GetBoxes(deskew(paste("Sample", y$Sample, y$file.jpg, sep = "/")))
    #if deskew causes crashes
    #GetBoxes(paste("Sample", y$Sample, y$file.jpg, sep = "/"))
}

#saveRDS(cat.text, paste("cat", which_cat, ".RDS", sep = ""))

# 6d. Explore output from the catalog (text types, confidence distributions) ####

# what types of text are in the files?
cat.textTypes = lapply(cat.text, function(x) {
  data.frame(as.list(table(sapply(x$text, isPrice, maybe = TRUE))))
})
cat.textTypes = bind_rows(cat.textTypes)
names(cat.textTypes) = sapply(names(cat.textTypes), switch, "FALSE." = "other_text","number" = "number", "TRUE." = "price",
                           "number." = "number*", "X.number" = "*number")

# If median confidence is below 25 (or 30?) it's probably an image
# If there are no prices (NA)
#     but mean conficence is > 30-35 it's probably a text page 
#     but mean conficence is < 25-30 it's probably a picture page
# Seems to work on a page with rotate boxes too, tried 1009 in sample 26

# create data frame of all boxes from the entire catalog
cat.textDF = data.frame(bind_rows(cat.text), 
                        file = rep(cat.images$file.jpg, sapply(cat.text, nrow)),
                        Nprice = rep(cat.textTypes$price, sapply(cat.text, nrow))) %>%
                        group_by(file) %>% mutate(median = median(confidence)) %>% ungroup() %>%
                        mutate(title = paste(str_extract(file, "[0-9]{4}"), str_pad(Nprice, 2, "left"), round(median,1), sep = "_"))

write.csv(cat.textDF, file = paste("cat_textDF_before_", which_cat, ".csv", sep =""), row.names = F)

# plot ####
ggplot(cat.textDF, aes(x = confidence)) + geom_histogram() +
  facet_wrap(~title) + 
  ggtitle("Distribution of confidences by image", subtitle =  "Title = image number _ Nprice _ Median confidence")

# 6e. Extract high-confidence word/image pairs from this catalog ####

highconf = filter(cat.textDF, median > 25 & !is.na(Nprice) & confidence > 96.5)
highconf = left_join(highconf, sample_files[,c("file.jpg", "Sample")], by = c("file" = "file.jpg"))
highconf = split(highconf, as.factor(as.character(highconf$file)))
lapply(highconf, function(x) {
  img1 = str_extract(x$file[1], "UCD_Lehmann_[0-9]{4}")
  sample1 = x$Sample[1]
  make_box_tiff(img1, sample1, boxes = x, conf = 96.5,
                fixdir = paste("Fix_", which_cat, sep = ""),
                deskew = TRUE)
})
              
# 6f. Move to training ####

#make sure no other tiff or text files in the way
fixdir = paste("Fix_", which_cat, sep = "")

file.copy(from = paste0(fixdir,"/", list.files(path = fixdir, pattern = "\\.tiff$", recursive = T)),
          to = paste0(fixdir,"/", gsub(x = list.files(path = fixdir, pattern = "\\.tiff", recursive = T), "\\.tiff", ".tif")))

file.copy(paste0(fixdir,"/", list.files(path = fixdir, pattern = ".*\\.tif$", recursive = T)), tessdata_train)

file.copy(paste0(fixdir,"/", list.files(path = fixdir, pattern = ".*\\.gt.txt$", recursive = T)), tessdata_train)


# 6g. Evaluate newly trained model ####

# Check improvement(?) on truth set (see mkTruth2.R)

# Check overall confidence distribution
cat.text.retrained = vector("list", nrow(cat.images))
for(i in 1:nrow(cat.images)) {
  print(i)
  y = cat.images[i,]
  cat.text.retrained[[i]] = GetBoxes(deskew(paste("Sample", y$Sample, y$file.jpg, sep = "/")), lang = "foo")
}

cat.textTypes.retrained = lapply(cat.text.retrained, function(x) {
  data.frame(as.list(table(sapply(x$text, isPrice, maybe = TRUE))))
})
cat.textTypes.retrained = bind_rows(cat.textTypes.retrained)
names(cat.textTypes.retrained) = sapply(names(cat.textTypes.retrained), switch, "FALSE." = "other_text","number" = "number", "TRUE." = "price",
                              "number." = "number*", "X.number" = "*number")


cat.textDF.retrained = data.frame(bind_rows(cat.text.retrained), 
                        file = rep(cat.images$file.jpg, sapply(cat.text.retrained, nrow)),
                        Nprice = rep(cat.textTypes.retrained$price, sapply(cat.text.retrained, nrow))) %>%
                        group_by(file) %>% mutate(median = median(confidence)) %>% ungroup() %>%
                        mutate(title = paste(str_extract(file, "[0-9]{4}"), str_pad(Nprice, 2, "left"), round(median,1), sep = "_"))

ggplot(cat.textDF.retrained, aes(x = confidence)) + geom_histogram() +
  facet_wrap(~title) + 
  ggtitle("Distribution of confidences by image (Retrained)", subtitle =  "Title = image number _ Nprice _ Median confidence")


# Check overall confidence distribution
cat.text.retrained2 = vector("list", nrow(cat.images))
for(i in 1:nrow(cat.images)) {
  print(i)
  y = cat.images[i,]
  cat.text.retrained2[[i]] = GetBoxes(deskew(paste("Sample", y$Sample, y$file.jpg, sep = "/")), lang = "eng+foo")
}

cat.textTypes.retrained2 = lapply(cat.text.retrained2, function(x) {
  data.frame(as.list(table(sapply(x$text, isPrice, maybe = TRUE))))
})
cat.textTypes.retrained2 = bind_rows(cat.textTypes.retrained2)
names(cat.textTypes.retrained2) = sapply(names(cat.textTypes.retrained2), switch, "FALSE." = "other_text","number" = "number", "TRUE." = "price",
                                        "number." = "number*", "X.number" = "*number")


cat.textDF.retrained2 = data.frame(bind_rows(cat.text.retrained2), 
                                  file = rep(cat.images$file.jpg, sapply(cat.text.retrained2, nrow)),
                                  Nprice = rep(cat.textTypes.retrained2$price, sapply(cat.text.retrained2, nrow))) %>%
  group_by(file) %>% mutate(median = median(confidence)) %>% ungroup() %>%
  mutate(title = paste(str_extract(file, "[0-9]{4}"), str_pad(Nprice, 2, "left"), round(median,1), sep = "_"))

ggplot(cat.textDF.retrained2, aes(x = confidence)) + geom_histogram() +
  facet_wrap(~title) + 
  ggtitle("Distribution of confidences by image (Retrained2)", subtitle =  "Title = image number _ Nprice _ Median confidence")
