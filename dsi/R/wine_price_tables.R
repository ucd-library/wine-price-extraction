# Extracting accurate prices from tables
# TO DO
# incorporate horizontal lines in finding prices
# smarter buffer
# get bottle and case info
# case if column is exactly vertical?
# make column recognition incorporate text justification
# cluster tops and bottoms if multiple charts on page

# workflow:
  # 1. Deskew and get boxes -- use existing Rtess functions. helpful to deskew first.
  # 2. Get initial column info -> find more possible prices -> get final column info -- bounds and slopes and also justification of column text and character height
  # 3. Extract text from columns 
  # 4. Extract final price guesses from column lines

####################################################################################################
# BUILD ####
####################################################################################################

wd = "~/Documents/DSI/OCR_SherryLehmann/"
setwd(wd)

# 0. Setup ####
library(Rtesseract)
library(tidyverse)
library(stringr)
library(jpeg)
library(cluster)

FullBoxes = readRDS("FullBoxes.rds") #download from http://dsi.ucdavis.edu/WineCatalogs/FullBoxes.rds

source("~/Documents/DSI/WineCatalogs_forked_repo/R/helper.R")

# Functions
# 2. Get iterative column info  ####

cleanBoxes <- function(data1, img1 = NULL, show.plot = FALSE) { #need img1 until image size attribute implemented

    prices =  data1[isPrice(data1$text),] 
    prices$center = (prices$left + prices$right)/2
    data1$price = isPrice(data1$text)
    data1$type = isPrice(data1$text, maybe = TRUE)
    charheight = median(prices$top - prices$bottom)
    
    #2a. initial column info ####
    
    cols1 = priceCols(prices, minGap = charheight)
    k = cols1[["k"]]
    just = cols1[["just"]]
    clust1 = cols1[["column_info"]]
    prices$cluster = clust1$clustering
    colData = prices %>% group_by(cluster) %>%
    summarize(col_left = min(left), col_right = max(right),
              col_bottom = min(prices$bottom), col_top = max(prices$top))
    colData$slope = rep(0,k)
    max.clust = which.max(clust1$clusinfo[,1]) #largest cluster

    #2b. Find more prices. Chose to do this to get most complete possible table. ####

    height1 = dim(readJPEG(img1))[1] #note we'll use the image attribute here later

    for (i in 1:k) {
      #column alignment
      lm.tmp = lm(height1 - top ~ prices[[just]], data = prices,
              subset = (clust1$clustering == i) )
      #print(lm.tmp)
      if(show.plot) {abline(lm.tmp)}
      if(i == max.clust) {slope = as.numeric(lm.tmp$coefficients[2])} #use slope from largest cluster
      colData$slope[i] = as.numeric(lm.tmp$coefficients[2])
      
      #find what's on the line
      near = abs((data1$top - data1$right*lm.tmp$coefficients[2] - lm.tmp$coefficients[1]
      )/lm.tmp$coefficients[2]) < charheight
      if(show.plot) {
        points(data1$top ~ data1[[just]], col = 1 + as.numeric(near))
      }
      look = data1$price == FALSE & near == TRUE
      data1$text.new = "FALSE"
      data1$text.new[look & data1$type == "number"] =  sapply(data1[look & data1$type == "number","text"], numToPrice)
      cat("grabbed", sum(data1$text.new!="FALSE"), "new prices\n") 
      data1$price[data1$text.new!="FALSE"] = TRUE
      data1$text[data1$text.new!="FALSE"] = data1$text.new[data1$text.new!="FALSE"]
    }
    
    #what do we have now?
    prices2 = filter(data1, price==TRUE)
    cat("progress: ", c(nrow(prices), "prices, then", nrow(prices2)), "\n")
    
    #2c. final column info ####
    
    cols2 = priceCols(prices2, minGap = charheight)
    k = cols2[["k"]]
    just = cols2[["just"]]
    clust2 = cols2[["column_info"]]
    prices2$cluster = clust2$clustering
    colData = prices2 %>% group_by(cluster) %>%
      summarize(col_left = min(left), col_right = max(right),
                col_bottom = min(prices2$bottom), col_top = max(prices2$top))
    colData$slope = rep(0,k)
    max.clust = which.max(clust2$clusinfo[,1]) #largest cluster

    return(list(newprices = prices2,
              columns = colData,
              progress = c(nrow(prices), nrow(prices2)),
              charheight = charheight,
              just = just))
}  
  
#    Get column info -- Find price columns, position and justification of text ####
priceCols <- function(prices, minGap) {
  prices$center = (prices$left + prices$right)/2
  clust = list(
    L = sapply(1:max(1, min(10, (nrow(prices)-2))), function(x) {
      pam1 = pam(prices$left, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min.size = min(pam1$clusinfo[,1]) #minimum cluster size
      clust.dist = abs(outer(c(pam1$medoids), c(pam1$medoids), "-"))
      min.dist = min(clust.dist[clust.dist > 0])
      return(c(obj1, min.size, min.dist))
    }),
    R = sapply(1:max(1, min(10, (nrow(prices)-2))), function(x) {
      pam1 = pam(prices$right, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min.size = min(pam1$clusinfo[,1]) #minimum cluster size
      clust.dist = abs(outer(c(pam1$medoids), c(pam1$medoids), "-"))
      min.dist = min(clust.dist[clust.dist > 0])
      c(obj1, min.size, min.dist)
    }),
    Ce = sapply(1:max(1, min(10, (nrow(prices)-2))), function(x) {
      pam1 = pam(prices$center, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min.size = min(pam1$clusinfo[,1]) #minimum cluster size
      clust.dist = abs(outer(c(pam1$medoids), c(pam1$medoids), "-"))
      min.dist = min(clust.dist[clust.dist > 0])
      c(obj1, min.size, min.dist)
    })
  )
  
  #left or right align?
  just = with(clust, max(apply(rbind(clust[["L"]][1,],
                                     clust[["R"]][1,],
                                     clust[["Ce"]][1,]),
                               2, which.min)))
  k = which.min(clust[[just]][1, clust[[just]][2,]>1 & clust[[just]][3,] > minGap])
  just = switch(just, "left", "right", "center")
  
  column1 = prices[[just]]
  clust1 = pam(column1, k)
  return(list(column_info = clust1, k = k, just = just))
}

# 3. Extract text from columns ####
extractFromCols <- function(x, colData, px, buffer = 5) {
  colDataRow = colData[x,]
  #px = pixRead(img1)
  #prot = pixRotate(px, -atan(1/colDataRow$slope))
  tpx = tesseract(px)
  SetRectangle(tpx, dims = c(colDataRow$col_left - buffer, max(0, colDataRow$col_bottom - buffer), #bottom is "TOP" in help. ugh.
                            colDataRow$col_right - colDataRow$col_left + buffer,
                            colDataRow$col_top - colDataRow$col_bottom + buffer))
  
  GetBoxes(tpx, level = "textline")
}

####################################################################################################
# RUN ####
####################################################################################################

# preprocess
img1 = "~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/UCD_Lehmann_0011.jpg"
#img1 = "~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/UCD_Lehmann_0069.jpg"
api = tesseract(img1)

################## check 1
gb1 = GetBoxes(api)
prices1 = gb1[isPrice(gb1$text),] 
plot(api, cropToBoxes = F, bbox = prices1)
# checked after removing low-confidence prices -- removed real prices, so don't do
# prices = dplyr::filter(prices, confidence > 50)
# plot(api, cropToBoxes = T, bbox = prices)
################## 

# 1
px1 = deskew(pixConvertTo8(pixRead(img1)))
data1 = GetBoxes(px1)
#data1[isPrice(data1$text),]

# 2
boxes = cleanBoxes(data1, img1)

################### check 2
boxes.prices = boxes[["newprices"]]
plot(tesseract(px1), cropToBoxes = F, bbox = boxes.prices, img = px1)
################## 

# 3
boxes.cols = lapply(1:nrow(boxes$columns), extractFromCols, boxes$columns,
                  px1, buffer = boxes$charheight/3)

# 4
prices.cols = lapply(boxes.cols, 
                     function(x) {
                       x$text = gsub("\n", "", x$text)
                       x = x[isPrice(x$text, maybe = TRUE) != "FALSE",]
                       x$text.new = sapply(x$text, numToPrice)
                       x$price = x$text.new != "FALSE"
                       return(x)
                     })

prices = lapply(prices.cols, dplyr::filter, price)

################### check 3
plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", prices), img = px1, confidence = FALSE)
plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", prices), img = px1)
################## 
