# Extracting accurate prices from tables

# TO DO
# addMissing can be streamlined, fix so it doesn't break if run twice, finish missings ids once i see an example
# smarter buffer
# get bottle and case info
# case if column is exactly vertical?
# make column recognition incorporate text justification
# cluster tops and bottoms if multiple charts on page

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
library(changepoint)

source("~/Documents/DSI/wine-price-extraction/dsi/R/helper.R")

# Functions ####
# 2. Get iterative column info  ####

# returns found prices, price column locations, number of prices found, character height,
# column justification, id column locations (left edge), and found ids
# image attribute only used for height

pageCols <- function(data1, img = NULL, img.height = NULL, show.plot = FALSE) { #need img until image size attribute implemented
  
    if(is.null(img) & is.null(img.height)) {break("Need image or image height")}
    prices =  data1[isPrice(data1$text),] 
    prices$center = (prices$left + prices$right)/2
    data1$price = isPrice(data1$text)
    data1$type = isPrice(data1$text, maybe = TRUE)
    charheight = median(prices$top - prices$bottom)
    
    #2a. initial column info ####
    
    # prices
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

    if (is.null(img.height)) height1 = dim(readJPEG(img))[1] #note we'll use the image attribute here later

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
    
    #2c. final price column info ####
    
    cols2 = priceCols(prices2, minGap = charheight)
    k = cols2[["k"]]
    just = cols2[["just"]]
    clust2 = cols2[["column_info"]]
    prices2$cluster = clust2$clustering
    colData = prices2 %>% group_by(cluster) %>%
      summarize(col_left = min(left), col_right = max(right),
                col_bottom = min(prices2$bottom), col_top = max(prices2$top),
                entries = n())
    colData$slope = rep(0,k)
    max.clust = which.max(clust2$clusinfo[,1]) #largest cluster
    colData = colData %>% arrange(col_left)
    
    #2d. id column info ####
    # numbers
    if (sum(data1$type=="ID") <= max(2, round(max.clust/3))) {
      tmp.numbers = filter(data1, type == "number", grepl(text, pattern="^[0-9]*$")) %>% arrange(left)
      tmp.numbers.cum = sapply(tmp.numbers$left, function(x) {sum(x > tmp.numbers$left)})
      idtype = "number"
    } else {
      tmp.numbers = filter(data1, type == "ID") %>% arrange(left)
      tmp.numbers.cum = sapply(tmp.numbers$left, function(x) {sum(x > tmp.numbers$left)})
      idtype = "ID"
    }
    
    #plot(tmp.numbers$left, tmp.numbers.cum, type="l")
    tmp.cpt = tmp.numbers$left[cpt.mean(tmp.numbers.cum, method='PELT')@cpts]
    tmp.cpt = tmp.cpt[-which(diff(tmp.cpt) < charheight/4)] #filter duplicates
   
    #are there at least _ numbers near each cpt?
    id_cols = tmp.cpt[(rowSums(abs(outer(tmp.cpt, tmp.numbers$left,  "-")) < charheight/4)) > 
                        max(table(prices2$cluster))/2]
    if(length(id_cols) > 0) {
    ids = filter(tmp.numbers, type == idtype,
                 abs(apply(abs(outer(tmp.numbers$left, id_cols, "-")), 1, min)) < charheight,
                 bottom > min(prices$bottom) - 4*charheight) %>% arrange(bottom)
    ids$table = sapply(ids$left, function(x) {which.min(abs(x-id_cols))}) #assume one id col per table
    
    id_cols = ids %>% group_by(table) %>% summarize(col_left = min(left),
                                                     col_right = max(right),
                                                     col_top = max(top),
                                                     col_bottom = min(bottom),
                                                     entries = n()) %>% arrange(col_left)
    
    } else {
      id_cols = NULL
      ids = NULL
    }
    
    #2e. return ####
    return(list(prices = lapply(1:max(prices2$cluster), function(x) filter(prices2, cluster==x)),
              price_cols = colData,
              n_price_cols = max(colData$cluster),
              
              progress = c(nrow(prices), nrow(prices2)),
              charheight = charheight,
              just = just,
              
              id_cols = id_cols, #approximate left locations of the number columns
              ids = ids, #the catalog numbers that go with the names
              n_id_cols = nrow(id_cols),
              idtype = idtype
              ))
}  
  
#    priceCols  Called by pageCols -- Find price columns, position and justification of text ####
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

# 3. Extract text from columns, find more prices and IDs ####
boxesFromCols <- function(index, colData, px, buffer = 5) {
  colDataRow = colData[index,]
  #px = pixRead(img)
  #prot = pixRotate(px, -atan(1/colDataRow$slope))
  tpx = tesseract(px)
  SetRectangle(tpx, dims = c(colDataRow$col_left - buffer, max(0, colDataRow$col_bottom - buffer), #bottom is "TOP" in help. ugh.
                            colDataRow$col_right - colDataRow$col_left + 2*buffer, #since left also has a buffer
                            colDataRow$col_top - colDataRow$col_bottom + buffer))
  
  gb = GetBoxes(tpx, level = "textline")
  gb$text = gsub("\n", "", gb$text)
  return(gb)
}

# 4. Find table locations using changepoint method. Check against IDs (if possible); group columns by table ####

pageTables <- function(data1, prices, page.cols, cpt.var = .05, buffer = page.cols$charheight/3) {

  # first organize columns into tables ####
  n.pricecols = length(prices)
  v.prices = lapply(prices, function(x) {c(mean(x$left), mean(x$right))})
  v.prices = v.prices[order(sapply(v.prices, first))]
  v.widths = diff(unlist(v.prices)) #column width, break, column width, break, etc.
  
  c = 2; i = 1; index = 1
  tables = vector(mode = "list", n.pricecols)
  tables[[1]] = v.prices[[1]]
  while (index < length(v.widths)) {
    if (v.widths[index+1] < v.widths[index]) {
      #new column in table
      tables[[i]] = rbind(tables[[i]], v.prices[[c]])
      index = index + 2
      c = c+1
    } else {
      #new table
      i = i + 1
      tables[[i]] = rbind(tables[[i]], v.prices[[c]])
      #v.labels[c] = paste(paste("table", i, collapse="_"), paste("col", j, collapse="_"), sep = ", ")
      index = index + 2
      c = c + 1
    }
  }
  n.table = i
  tables = lapply(1:n.table, function(x) {tables[[x]]})
 
  # find table left boundary ####
  # find page boundaries
  #by position
  data1.order = data1[order(data1$left),]
  r = max(data1.order$right)
  data1.cum = sapply(1:r, function(x) {sum(data1.order$left <= x)})
  data1.lag = diff(data1.cum)
  data1.lag[1] = 0
  v.regions = cpt.var(data1.lag, method='PELT')
  cpts = v.regions@cpts
  vars = v.regions@param.est$variance
  #we can use this to find one that's after a space, tmp@param.est$variance should be close to zero
  for (t in 1:n.table) {
    if (!is.null(nrow(tables[[t]])) && nrow(tables[[t]]) > 1) {
      attr(tables[[t]], "left") = max(cpts[(cpts < (tables[[t]][1,1] - (tables[[t]][1,2] - tables[[t]][2,1]))) & (vars < cpt.var)]) - buffer
    } else {
      attr(tables[[t]], "left") = max(cpts[cpts < tables[[t]][1] - buffer])
    }
  }
  return(list(breaks = cpts, breaks.var = vars, tables = tables, charheight = page.cols$charheight))
}

# find which table on the page each column is in
whichTable <- function(page.tables, page.cols) {
  mid = rowMeans(page.cols$price_cols[,c("col_left", "col_right")])
  apply(sapply(page.tables[["tables"]], function(t) {
    mid > min(t) & mid < max(t)
  }), 1, which)
}


# 5. Check again for missing prices and now IDs too ####
##  if no ids, just look for mismatches between #prices in columns in tables ----
##  if ids, also look for mismatches between #ids and #prices in tables ---- 

addMissing <- function(page.cols, buffer = page.cols$charheight) {
  
  # assumes everything's ordered correctly
  page.cols$price_cols$n_ids = page.cols$id_cols$entries[page.cols$price_cols$table]
  
  page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% 
    mutate(table.size = max(entries),
           missing.by.price = entries < table.size,
           missing.by.id = entries < n_ids,
           missing.id = n_ids < entries)
  
  # by within-table price column mismatch ####
  
  # list of tables we think have missing prices based on price column length mismatches
  incomplete.by.price = page.cols$price_cols %>% group_by(table) %>% summarize(sum(missing.by.price)) %>% 
    filter(`sum(missing.by.price)` > 0) %>% .[,"table"]
  
  if (nrow(incomplete.by.price) > 0) {
    cat(incomplete.by.price, "might be missing prices")
    for (elem in incomplete.by.price) {
      tmp.cols.missing = page.cols$prices[page.cols$price_cols$table == elem & page.cols$price_cols$missing]
      tmp.cols.full = page.cols$prices[page.cols$price_cols$table == elem & !page.cols$price_cols$missing]
      full.diffs = rowMeans(rbind(sapply(tmp.cols.full, function(x) {diff(x$top)})))
      full.top = rowMeans(rbind(sapply(tmp.cols.full, function(x) {x$top})))
      full.bot = rowMeans(rbind(sapply(tmp.cols.full, function(x) {x$bottom})))
      missing.spots = lapply(tmp.missing, function(x) 1 + which(diff(x$bottom) > median(full.diffs)*1.5))
      missing.boxes = lapply(1:length(tmp.cols.missing), function(x) {
        data.frame(
          left = rep(median(tmp.cols.missing[[x]]$left), length(missing.spots[x])),
          bottom = full.bot[missing.spots[[x]]],
          width = rep(median(tmp.cols.missing[[x]]$right) - median(tmp.cols.missing[[x]]$left),
                      length(missing.spots[x])),
          height =  full.top[missing.spots[[x]]] - full.bot[missing.spots[[x]]]
        )})
      boxes = lapply(missing.boxes, checkBoxes, px1, height = height1, buffer = buffer/3)
      boxes = lapply(boxes, filter, price)
      boxes = lapply(boxes, function(x) {
        x$text.new = str_extract(x$text, "[0-9].*")
        if (sum(is.na(x$text)) > 0) {cat("Missing entries not all prices", x$text)}
        return(x)
      })
      for (i in 1:length(tmp.cols.missing)) {
        tmp.cols.missing[[i]] = rbind(tmp.cols.missing[[i]], boxes[[i]][,c(1:6,8,7)]) %>% arrange(top)
      }
      page.cols$prices[page.cols$price_cols$table == elem & page.cols$price_cols$missing] = tmp.cols.missing
    }
  }
  
  # by prices less than ids ####
  
  # note this is a data frame rather than a list of table numbers
  incomplete.by.id = filter(page.cols$price_cols, missing.by.id)
  
  if (nrow(incomplete.by.id) > 0) {
    cat(nrow(incomplete.by.id), "columns might be missing prices")
    for (row in 1:nrow(incomplete.by.id)) {
      #match the ids and prices
      tmp.table = as.numeric(incomplete.by.id[row,"table"])
      tmp.column = as.numeric(incomplete.by.id[row,"column"])
      tmp.ids = filter(page.cols$ids, table == tmp.table)
      tmp.i = which(page.cols$price_cols$table == tmp.table)[tmp.column]
      tmp.prices = page.cols$prices[[tmp.i]]
      
      tmp.lr = as.numeric(page.tables$tables[[tmp.table]])
      if (is.null(dim(tmp.lr))) {left = tmp.lr[1]; right = tmp.lr[2]} else {
        left = tmp.lr[tmp.column, 1]; right = tmp.lr[tmp.column, 2]
      }
      
      #find missing
      tmp.missing = which(apply(abs(outer(tmp.ids$top, tmp.prices$top, "-")), 1, min) > buffer)
      n.missing = length(tmp.missing)
      missing.boxes = data.frame(
        left = rep(left, n.missing),
        bottom = tmp.ids[tmp.missing, "bottom"],
        width = rep(right, n.missing) - rep(left, n.missing),
        height = tmp.ids[tmp.missing, "top"] - tmp.ids[tmp.missing, "bottom"]
      )
      boxes = checkBoxes(missing.boxes, px1, height = height1, buffer = buffer/3)
      #boxes = filter(boxes, price), don't filter out, leave in with price False
      if (sum(is.na(str_extract(boxes$text, "[0-9].*"))) > 0) {cat("Missing entries detected", boxes$text)}
      
      boxes$text.new = boxes$text
      page.cols$prices[[tmp.i]] = rbind(page.cols$prices[[tmp.i]], boxes[,c(1:6,8,7)])
      
    }
  }
  
  incomplete.id = filter(page.cols$price_cols, missing.id)
  
  if (nrow(incomplete.by.id) > 0) {
    #fill this in later
  }
  
  return(page.cols)  
}

# re-check a box for a price
checkBoxes <- function(df, px, buffer = 10, level = "textline", height, checker = isPrice) {
  tpx = tesseract(px)
  
  tmp.list = apply(df, 1, function(x) {
    SetRectangle(tpx, dims = c(x[1] - buffer, x[2], x[3] + buffer, x[4] + buffer))
    gb = GetBoxes(tpx, level = level)
    gb$text = gsub("\n", "", gb$text)
    if (!is.null(checker)) {
      gb$price = checker(gb$text)
    }
    return(gb)})
  
  do.call("rbind", tmp.list)
}

# 6. Extract the names with words and images ####
nameBoxes <- function(data1, page.cols = NULL, prices, buffer = page.tables$charheight/3, page.tables = NULL) { #buffer on order of 1/2 small char size, 1/4 larege
  if (is.null(page.cols) & is.null(page.tables)) {break("Need at least one of page.cols (if IDs present) or page.tables")}
  if (!is.null(page.cols$id_cols)) {
    table.boxes = vector(mode = "list", length = page.cols$n_id_cols)
    table.boxes.words = vector(mode = "list", length = page.cols$n_id_cols)
    for (i in 1:page.cols$n_id_cols) {
      table1 = filter(page.cols$ids, table == i)
      table2 = subset(prices, page.cols$price_cols$table==i)[[1]]
      l = table1$left - buffer
      r = rep(min(filter(page.cols$price_cols, table == i)$col_left), length(l)) + buffer
      b = table1$bottom - buffer
      t = table2$top + buffer
      table.boxes[[i]] = data.frame(left = l, bottom = b, right = r, top = t)
      table.boxes.words[[i]] =  lapply(1:nrow(table.boxes[[i]]), function(x) {
        filter(data1, (left >= l[x] & right <= (r[x] + buffer*5)) & #extra buffer on the right
              (bottom >= b[x] & top <= t[x] + buffer))
      })
    }
  } else {
    tables = page.tables[["tables"]]
    prices.tables = rep(1:length(tables), times = sapply(tables, nrow)) #indexing by table
    table.boxes = vector(mode = "list", length = length(tables))
    table.boxes.words = vector(mode = "list", length = length(tables))
    for (i in 1:length(tables)) {
      l = attr(tables[[i]], "left")
      r = tables[[i]][1,1] #right is is LEFT of prices
      colsintable = subset(prices, prices.tables==i)
      b = sort(do.call("rbind", colsintable)$bottom)
      t = sort(do.call("rbind", colsintable)$top)
      r1 = prices[[i]][1]
      
      tmp.boxes = filter(data1, left >= l, left <= min(colsintable[[1]]$left), top <= max(t), bottom > min(b) - buffer*8)
      char.types = charTypes(tmp.boxes)
      char.sizes = char.types$means
      
      tmp.boxes$char.sizes = char.types$means[char.types$membership]
      if (buffer == "dynamic") {buffer= min(char.sizes)/2}
      
      b = b[(b - lead(b, 1)) < -buffer | is.na(b - lead(b, 1))]
      t = t[(t - lead(t, 1)) < -buffer | is.na(t - lead(t, 1))]
      if (length(b) != length(t)) {break("in nameBoxes: price data incomplete")}
      
      #if text above the price is in the bigger character type then include it the name
      table.boxes.words[[i]] = vector(mode = "list", length = length(t))
      for (j in 1:length(t)) {
        tmp.name = filter(tmp.boxes, left < r & (top <= (t[j]+buffer) & bottom >= (b[j]-2*max(char.sizes))) )
        tmp.name = filter(tmp.name, right < mean(tables[[i]][1,])) #remove anything glued to a price
        if (nrow(tmp.name)==0) {next} else {
          if (mean(tmp.name$char.sizes == max(char.sizes)) >= .4) {
            if (j == 1) {b[j] = min(tmp.name$bottom)} 
            if (j > 1) {b[j] = max(min(tmp.name$bottom - min(char.sizes)), t[j-1])}
            if (max(tmp.name$right) > r) {r = max(tmp.name$right)}
          }
        } #include another row in name, make sure below other price
        table.boxes.words[[i]][[j]] = tmp.name
      }
      tmp.lines = max(sapply(colsintable, nrow))
      table.boxes[[i]] = data.frame(left = rep(l, tmp.lines), bottom = b, right = rep(r, tmp.lines), top = t)
    }
  }
  names(table.boxes) = paste("table", 1:length(table.boxes), sep = "_")
  names(table.boxes.words) = paste("table", 1:length(table.boxes.words), sep = "_")
  return(list(locations = table.boxes, words = table.boxes.words))
}

# 7. Return final results - IDs, prices, names (with locations, words and image) ####


####################################################################################################
# RUN ####
####################################################################################################

image.check = FALSE

# preprocess ####
file1 = "UCD_Lehmann_1106"
img1 = paste("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/", file1, ".jpg", sep = "")
#img1 = "~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/UCD_Lehmann_0069.jpg"
api = tesseract(img1)
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
data1 = GetBoxes(px1)
#data1[isPrice(data1$text),]

# 2 ####
page.cols = pageCols(data1, img.height = height1)

############# img check 2 ####
if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1)

# 3 ####

tmp.boxes.from.cols_price = lapply(1:page.cols$n_price_cols, boxesFromCols, page.cols$price_cols, px1, buffer = page.cols$charheight/3)
tmp.prices.cols = lapply(tmp.boxes.from.cols_price, 
                     function(x) {
                       x = x[isPrice(x$text, maybe = TRUE) != "FALSE",]
                       x$text.new = sapply(x$text, numToPrice)
                       x$price = x$text.new != "FALSE"
                       return(x)
                     })

############# table check 1 ####

cat("Did this process add or take away PRICES? (all >= 0 good):", sapply(tmp.prices.cols, nrow) - page.cols$price_cols$entries)

if (!is.null(page.cols$ids)) {
  tmp.boxes.from.cols_id = lapply(1:page.cols$n_id_cols, boxesFromCols, page.cols$id_cols, px1, buffer = page.cols$charheight/3)
  tmp.ids.cols = lapply(tmp.boxes.from.cols_id, 
                     function(x) {
                       x = filter(x, isPrice(x$text, maybe = TRUE) == page.cols$idtype,
                                  !grepl(pattern = "[A-Za-z]{2}", x$text))})
  cat("Did this process add or take away IDs (>= 0 good)?", sapply(tmp.ids.cols, nrow) - page.cols$id_cols$entries)
}

#extract good data and order left to right
page.cols$prices = lapply(tmp.prices.cols, dplyr::filter, price)[order(unlist(lapply(tmp.prices.cols, function(x) mean(x$left))))]

#update column information
page.cols$price_cols$entries = sapply(page.cols$prices, nrow)
page.cols$id_cols$entries  = sapply(tmp.ids.cols, nrow)
page.cols$id_cols$col_left = sapply(tmp.ids.cols, function(x) {min(x$left)})
page.cols$id_cols$col_right =sapply(tmp.ids.cols, function(x) {min(x$right)})

for (elem in seq_along(tmp.ids.cols)) {
  tmp.ids.cols[[elem]]$type = page.cols$idtype
  tmp.ids.cols[[elem]]$price = FALSE
  tmp.ids.cols[[elem]]$text.new = FALSE
  tmp.ids.cols[[elem]]$table = elem
}
page.cols$ids = do.call("rbind", tmp.ids.cols)
  
############# img check 3 ####
if(image.check) plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", page.cols$prices), img = px1, confidence = FALSE)
#plot(tesseract(px1), cropToBoxes = F, bbox = do.call("rbind", prices), img = px1)
############# table check 2 ####
if (!is.null(page.cols$ids)) {
  cat("We guess there's", page.cols$n_id_cols, "table(s)\n")
  tmp.maybe_missing = rep(page.cols$id_cols$entries, each = page.cols$n_price_cols/page.cols$n_id_cols) - sapply(page.cols$prices, nrow)
  if (max(tmp.maybe_missing) > 0) {
    cat("Column(s)", which(tmp.maybe_missing>0), "may be missing", tmp.maybe_missing[tmp.maybe_missing>0], "entry")
  }
}
# 4 ####


# Find table locations using changepoint method 
page.tables = pageTables(data1, page.cols$prices, page.cols, cpt.var = 0.05, buffer = page.cols$charheight/3)


# Check that left edge based on ID columns, if present, equals changepoint method (page.tables) left-edge
# Default to method based on ID columns
if (!is.null(page.cols$ids)) {
  tmp.left.diffs = page.cols$id_cols$col_left - round(unlist(lapply(page.tables$tables, attr, "left")))
  cat("Difference in left-edge detected using ID col method vs. changepoint method",
  tmp.left.diffs, "\n")
  cat("This is",  100*(tmp.left.diffs)/page.cols$charheight,  "percent of estimated character height")
  if(max(100*(tmp.left.diffs)/page.cols$charheight) > 100) {cat("Error?? Greater than character's difference")}
}

# Add table and column var to price column table
page.cols$price_cols$table = whichTable(page.tables, page.cols)
page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% mutate(column = rank(cluster))

# 5 ####

page.cols = addMissing(page.cols, buffer = page.cols$charheight)

# 6 ####

name.boxes = nameBoxes(data1, page.cols, page.cols$prices, buffer = page.tables$charheight/2)

# 7 ####

# ids (if there) and prices
final.prices = vector("list", max(page.cols$price_cols$table))
final.prices = lapply(1:max(page.cols$price_cols$table), function(x) {
  final.prices[[x]] = list(
    if (!is.null(page.cols$ids)) {ids = unlist(page.cols$ids %>% filter(table == x) %>% .$text)},
    prices = lapply(subset(page.cols$prices, page.cols$price_cols$table == x), function(y) y$text.new)
  )
})
names(final.prices) = paste("table", 1:max(page.cols$price_cols$table), sep = "_")

# all
final.data = list(prices = final.prices, name.locations = name.boxes[["locations"]],
                  name.words = name.boxes[["words"]])

saveRDS(final.data, paste("~/Documents/DSI/wine-price-extraction/dsi/Data/", file1, ".RDS", sep = ""))

################## check 4 / share image####
#make fake confidence for box colors
tmp.boxes = do.call("rbind", name.boxes[[1]])
png(paste("~/Documents/DSI/wine-price-extraction/dsi/Data/", file1, ".png", sep=""), width = 4000, height = 6000)
plot(tesseract(px1), img = px1)
dev.off()
png(paste("~/Documents/DSI/wine-price-extraction/dsi/Data/", file1, "_boxes.png", sep=""), width = 1000, height = 1500)
plot(tesseract(px1), cropToBoxes = F, bbox = tmp.boxes, img = px1, confidence = FALSE)
dev.off()


