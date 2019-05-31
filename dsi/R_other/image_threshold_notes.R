# Jane Carlen
# May 2019

# Checked that my image sizes line up with the ones in this spreadsheet
# Setting threshold value in pixThresholdToValue doesn't seem to affect deskew
# threshold in pixThresholdToBinary can affect deskew angle

# 0. Setup
library(dplyr)
library(Rtesseract)
library(jpeg)

setwd("~/Documents/DSI/wine-price-extraction/")

# Checked that my image sizes line up with the ones in this spreadsheet - jc
image_size = read.csv("csv/image_size.csv")
image_size %>% arrange(x) %>% head(50)
image_size %>% filter(x != 4000) %>% head(10)

#load
#img1 =  "~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/UCD_Lehmann_0267.jpg"
#img1 =  "~/Documents/DSI/OCR_SherryLehmann/Sample/Sample1/UCD_Lehmann_0015.jpg"
img1 =  "~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/UCD_Lehmann_1176.jpg" #150
#img1 =  "~/Documents/DSI/OCR_SherryLehmann/Sample/Sample27/UCD_Lehmann_0939.jpg" #200
#img1 =  "~/Documents/DSI/OCR_SherryLehmann/Sample/Sample37/UCD_Lehmann_4073.jpg"

jpg1 = readJPEG(img1)

# figuring out color thresholding ----
# Need a method for figuring out if color is in the text (and should -> black, assuming light background), or in the background and should go to white, assuming black text. (E.g. 1176 vs. 0015)

#convert to gray
#https://rdrr.io/cran/DescTools/man/ColToGrey.html 0.3*red + 0.59*green + 0.11*blue.
tmp = 255*(.3*as.vector(jpg1[,,1]) + .59 * as.vector(jpg1[,,2]) + .11 * as.vector(jpg1[,,3])) 

tmp.k = kmeans(tmp, 4)
tmp.k$cluster
sort(tmp.k$centers)
round(tmp.k$size/sum(tmp.k$size), 2)

px1 = pixRead(img1)
px1 = pixConvertTo8(px1)
plot(px1)
sapply(c(100, 150, (sort(tmp.k$centers) + dplyr::lag(sort(tmp.k$centers)))/2), function(x) {
  pixFindSkew(pixThresholdToBinary(px1, x))
})

#px1 = pixThresholdToValue(px1, 90, 255) #0015 #how to determine if we want to set a backgorund color to white e.g. 0015, vs. a foreground/text color to black, e.g .1176 #use amount? ask Duncan what exists in Rteseract
px1 = pixThresholdToValue(px1, 175, 0)  #1176

px1 = deskew(px1)
plot(px1)

# original
px1 = pixRead(img1)
px1 = deskew(px1, binaryThreshold = 150)
plot(px1)
###

### tesseract seems to understand skew even when angle wasn't detected correctly
# but my code likes straight lines so deskeweing is good anyway
api0 = tesseract(px1, pageSegMode = 6, engineMode = 3)
if(GetSourceYResolution(api0)==0) {SetSourceResolution(api0, 600)}
data0 = GetBoxes(api0, pageSegMode = 6, engineMode = 3)
sum(isPrice(data0$text))
data0 %>% filter(isPrice(text))
plot(api0, cropToBoxes = F, img = px1)

# other stuff -----

# original
dim(readJPEG(img1))

api0 = tesseract(img1, pageSegMode = 6, engineMode = 3)
if(GetSourceYResolution(api0)==0) {SetSourceResolution(api0, 600)}
data0 = GetBoxes(api0, pageSegMode = 6, engineMode = 3)

#deskwewed

plot(deskew(px1, binaryThreshold = 90))
plot(deskew(px1, binaryThreshold = 150))

pixFindSkew(pixThresholdToBinary(pixConvertTo8(px1), 50))
pixFindSkew(pixThresholdToBinary(pixConvertTo8(px1), 150))
pixFindSkew(pixThresholdToBinary(pixConvertTo8(px1), 200))

px1 = deskew(px1, binaryThreshold = 150)

api1 = tesseract(px1, pageSegMode = 6, engineMode = 3)
if(GetSourceYResolution(api1)==0) {SetSourceResolution(api1, 600)}
data1 = GetBoxes(api1, pageSegMode = 6, engineMode = 3)

# compare

data0 %>% arrange(top) %>% filter(nchar(text)> 4) %>% head(10)
data1 %>% arrange(top) %>% filter(nchar(text)> 4) %>% head(10)
image.check = TRUE
if(image.check) plot(tesseract(px1), cropToBoxes = F, img = px1)
if(image.check) plot(tesseract(px2), cropToBoxes = F, img = px2)

# working on back-rotating price ----
# CENTRAL ROTATION: the origin for rotation is not 0,0, but the middle of the page, it seems
# https://tpgit.github.io/Leptonica/skew_8c_source.html - documentation of leptonica deskew, I think second number in pixFindSkew is a type of conficence score
# (DEFAULT_BINARY_THRESHOLD = 130;?)

img1 = img1 = "~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/UCD_Lehmann_0644.jpg"
px1 = pixRead(img1)

# just do a central point: 
# plot(x = c(0, 4000), y = c(0, 6000), type = "n")
# abline(v = 0); abline(h = 0)
output = readRDS("~/Documents/DSI/wine-price-extraction/dsi/Data/price_table_output/UCD_Lehmann_0644.RDS")
tmp.prices = output$page.cols$prices[[3]]
angle1 = -output$page.cols$angle[1] *pi/180
points0 = as.matrix(tmp.prices %>% mutate(deskew_center_x = (left+right)/2,
                                            deskew_center_y = (bottom + top)/2) %>% 
                                     select(deskew_center_x, deskew_center_y))

# see helper.R for rotatePoints function
points1 = rotatePoints(points0, output$page.cols$angle[1] , output$page.cols$height_orig, output$page.cols$width_orig, flip.y = FALSE)
plot(x = c(0, output$page.cols$width_orig), y = c(0,  output$page.cols$height_orig), type = "n")
points(points0, col = "red", pch = ".", cex = 3)
points(points1, col = "green", pch = ".", cex = 3)

# with image. flip.y is TRUE
plot(px1)
points1 = rotatePoints(points0, output$page.cols$angle[1] , output$page.cols$height_orig, output$page.cols$width_orig, flip.y = TRUE)
points(points1, col = "green", pch = ".", cex = 3)


