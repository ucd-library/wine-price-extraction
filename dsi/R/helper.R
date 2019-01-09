# figure out if things are prices
# generally allows two characters of fudge
# dollar sets whether to allow dollars signs before prices.
  # for tables default should be false

isPrice = #Jane's version - different than Duncan's isPrice
  
  function(x, dollar = FALSE, maybe = FALSE, years = c(1800,2000), warn = FALSE) {
    
    x = as.character(x)
    
    sapply(x, function(x) {
      
      if ( grepl("^([0-9]+\\.[0-9]{2}$)", x)) { return(TRUE) }
      
      else if ( grepl("^([0-9]+\\.[0-9]{2})\\ ([0-9]+\\.[0-9]{2})", x)) {
        ifelse(maybe, "two prices", FALSE)
      }
      
      else if( grepl("^\\$([0-9]+)\\.[0-9]{2}.{0,2}$", x) | grepl("^\\$([0-9]+).{0,2}$", x)) {
        if (warn) (paste0("Is this (", x, ") a price with a dollar sign? ", collapse = " "))
        ifelse(dollar, TRUE, FALSE)
      }
      
      else if( grepl("^([0-9]+)\\.[0-9]{2}", x )) {
        if (warn) (paste0("Is this (", x, ") a price followed by something else? ", collapse = " "))
        ifelse(maybe,  "price*", FALSE)
      }

      else if( grepl(".*([0-9]+)\\.[0-9]{2}", x )) {
        if (warn) warning(paste0("Is this (", x, ") a price preceded by something else? ", collapse = " "))
        ifelse(maybe, "*price", FALSE) #useful for looking at lines
      }
      
      else if( grepl("^\\$([0-9]+)\\.[0-9]{2}", x) | grepl("^\\$([0-9]+)", x)) {
        if (warn) warning(paste0("Is this (", x, ") a price with a dollar sign followed by something else? ", collapse = " "))
        ifelse(dollar, TRUE, FALSE)
      }
      
      else if( grepl("^([0-9])", x)) {
        year = 0 #default
        if (length(years) == 2) {
          first.dig.low = strtrim(years[1], 1)
          first.dig.high = strtrim(years[2], 1)
          year = as.numeric(str_extract(x, paste0("[", first.dig.low, "|", first.dig.high,"]", "[0-9]{3}")))
          if (!is.na(year) & ((year >= years[1] & year <= years[2]) & warn)) warning(paste0("This  (", x, ") seems to be a year, not a price ", collapse = " "))
        }
        ifelse(maybe & is.na(year), if (grepl("^([0-9]){3}" , x)) "number" else ("FALSE") , FALSE) #our IDS have at least 3 numbers
      }
      
      else if( grepl("^[nN].{0,1}[0-9]+$", x)) {
        if (warn) (paste0("This  (", x, ") seems to be an ID ", collapse = " "))
        ifelse(maybe, "ID", FALSE)
      }
      
      else{return(FALSE)}
    })
  }


numToPrice = function(numAsChar, dollar = FALSE) {
  #dollar False to not allow numbers will dollar sign in front 
  
  #option 1, just have to remove extra stuff at end, e.g. a period -- at most two
  if( grepl("^([0-9]+)\\.[0-9]{2}.{0,2}$", numAsChar)) return(str_extract(numAsChar, "^([0-9]+)\\.[0-9]{2}"))
  
  #option 2, number with wrong puncuation
  if( grepl("^([0-9]+)[\\ |,][0-9]{2}", numAsChar)) {
    return(gsub("\\ ", "\\.", str_extract(numAsChar, "^([0-9]+)[\\ |,][0-9]{2}")))
  }
  
  #option 3, number with stuff before -- allow at most lots
  if (dollar) {
    if( grepl("^.([0-9]+)\\.[0-9]{2}", numAsChar)) return(str_extract(numAsChar, "([0-9]+)\\.[0-9]{2}"))
  } else {
    if( grepl("\\$[0-9]+", numAsChar)) return(FALSE)
    else {
      if( grepl("([0-9]+)\\.[0-9]{2}", numAsChar)) return(str_extract(numAsChar, "([0-9]+)\\.[0-9]{2}"))
    }  
  }
  
  #option 4, number without punctuation (remove any puncuation at end -- allow at most 3 after)
  
  if (!grepl("[0-9]{3,10}.{0,2}$", numAsChar)) { return(FALSE) }
    else {
    numAsChar = str_extract(numAsChar, "[0-9]{3,10}")     
    num = strsplit(numAsChar, split="")[[1]]
    if(!tail(num,1) %in% c("0","5","9") & paste0(head(num,2), collapse = "") %in% c("18","19")) {
      warning(paste0(numAsChar, " is probably a year"))
      return(FALSE)
    }
    price = paste(paste0(num[1:(length(num)-2)], collapse = ""), ".",
                paste0(tail(num,2), collapse = ""), sep="")
    return(price)
  }
}
  

# Sort a catalog of images by instances of prices, numbers and other text
# will inform if a page has a price table

textTypes <- function(cat_files, catalog = NULL){
  
  if(is.null(catalog)) {catalog = sample(1:length(cat_files), 1)}
  cat1 = cat_files[catalog][[1]]
  cat("catalog number", catalog, "with", nrow(cat1), "files")
  
  text.tmp = lapply(paste(cat1$Sample, cat1$file.jpg, sep = "/"), function(x) {
    text1 = GetText(x)
    table1 = table(sapply(text1, isPrice, maybe = TRUE))
    print(prop.table(table1))
    return(data.frame(as.list(table1))) #if only one type skip -- wouldn't be a table page
  })
  
  text.types= bind_rows(text.tmp)
  names(text.types) = sapply(names(text.types), switch, "FALSE." = "other_text","number" = "number", "TRUE." = "price",
                             "number." = "number*", "X.number" = "*number")
  return(text.types)
}

# For precendence between detected prices
compareTypes <- function(pair) {
  if (pair[1] != "TRUE" & pair[2] == "TRUE") {return(2)}
  if (pair[1] == "FALSE" & "number" %in% pair[2] ) {return(2)}
  else {return(1)}
}

charWidth <- function(boxes) {
  (boxes$right-boxes$left)/nchar(boxes$text)
}

# Types of text by character width
charTypes <- function(boxes, types = 2, conf.min = 50) { #using k-means
  boxes$charwidth = charWidth(boxes)
  boxes.use = filter(boxes, confidence > conf.min) # only use somewhat high conf boxes for finding char types
  boxes.kmeans = kmeans(boxes.use$charwidth, centers = types)
  cluster = sapply(boxes$charwidth, function(x){
    which.min(abs(x - boxes.kmeans$centers))})
  return(list(membership = cluster, means = boxes.kmeans$centers))
}

# for extracting new information from identified column
boxesFromCols <- function(index, colData, px, buffer = 5, psm = 3) {
  colDataRow = colData[index,]
  tpx = tesseract(px, pageSegMode = 3)
  SetRectangle(tpx, dims = c(colDataRow$col.left - buffer, max(0, colDataRow$col.bottom - buffer), #bottom is "TOP" in help. ugh.
                             colDataRow$col.right - colDataRow$col.left + 2*buffer, #since left also has a buffer
                             colDataRow$col.top - colDataRow$col.bottom + buffer))
  
  gb = GetBoxes(tpx, level = "textline")
  gb$text = gsub("\n", "", gb$text)
  return(gb)
}

# Function to see if prices should be clustered  by checking for different character sizes matching locational clusters
splitCol <- function(prices, type = "h", px = px1, buffer = page.cols$charheight/3, height = height1,
                     new.index = 1, min.presplit = 4, min.postsplit = 1, max.impurity = 0) {
  #h for horizonatl, #v for vertical
  if (nrow(prices) < min.presplit) {return(NULL)}
  
  # if a column has entries with two prices, split the column by a horizontal divide
  if (type == "h") {
    two.prices = isPrice(prices$text, maybe = TRUE)=="two prices"
    if (sum(two.prices)>0) {
      split.prices = checkBoxes(makeCheckBox(prices[two.prices,]), px1, buffer = buffer, height = height1, level = "word", psm =  3)
      split.prices.left1 = quantile(split.prices$left, .25)
      split.prices.right1 = quantile(split.prices$right, .25)
      split.prices.left2 = quantile(split.prices$left, .75)
      split.prices$type = isPrice(split.prices$text, maybe = TRUE, dollar = FALSE)
      split.prices$text.new = extractPrice(split.prices$text, dollar = FALSE)
      split.prices$cluster = prices$cluster[1]
      # add the new prices from splitting old entries
      prices = rbind(prices[-which(two.prices),], split.prices[,names(prices)]) %>% arrange(top)
      prices$cluster[abs(prices$left - split.prices.left1) < abs(prices$left - split.prices.left2)] = new.index
    }
  return(prices = split(prices))
  }
  
  if (type == "v") {
  
    prices = prices %>% arrange(top)
    pricewidths = charWidth(prices) 
    compare.clusterings = sapply(1:min(floor(length(pricewidths)/3), length(unique(pricewidths))), function(x) {
      possible.cluster = kmeans(pricewidths, centers = x, nstart = 100)
      c(possible.cluster$tot.withinss, min(possible.cluster$size))
    })
    # stop looking if clustering returns all clusters <=  minimum allowed size
    max.possible.vclusters = min(floor(length(pricewidths)/3),  min(which(compare.clusterings[2,] <= min.postsplit))-1)
    
    #see if character size clusters match location clusters
    if (max.possible.vclusters > 1) {
      cluster.purity = sapply(2:max.possible.vclusters, function(x) {
        charsize.cluster = pam(pricewidths, k = x) #switched to mediod clustering so more robust, less randomness
        if (min(charsize.cluster$clusinfo[,1]) <= min.postsplit) {return(0)} else { #min(charsize.cluster$clusinfo[,1]) is cluster sizes
            charsize.order = rank(sapply(1:x, function(x) min(which(charsize.cluster$cluster == x))))
            perfect.cluster = rep(charsize.order, times = charsize.cluster$clusinfo[,1])
            RecordLinkage::levenshteinSim(paste(charsize.cluster$cluster, collapse=""),
                                        paste(perfect.cluster, collapse=""))
          }
      })
      #print(cluster.purity)
      if (sum(cluster.purity==1) > 0) {
        charsize.cluster = kmeans(pricewidths, centers = 1+max(which(cluster.purity==1)), nstart = 1000, iter.max = 1000)
        prices$cluster[charsize.cluster$cluster!=1] = new.index-2+charsize.cluster$cluster[charsize.cluster$cluster!=1]
      }
    }
    return(prices)
      #which(cluster.impurity==0)
  }
}

#where boxes is a result of a checkBoxes call on missing.boxes with rbind = FALSE
checkMissingBoxes <- function(boxes, missing.boxes, type = "price", cluster = cols.missing[[x]]$cluster[1], table = cols.missing[[x]]$table[1]) {
  
  if (type == "price") {pattern = "[0-9].*"}
  else if (type == "ID") {pattern = "[A-Z]-[0-9].*"}
  else {pattern = "[0-9].*"}
  
  lapply(1:length(boxes), function(i) {
    box = boxes[[i]]
    if (!is.null(boxes[[i]])) {
      box$text.new = str_extract(box$text, pattern)
      box$price = isPrice(box$text.new, dollar = FALSE, maybe = FALSE)
      box$type = isPrice(box$text.new, maybe = T, dollar = F)
      box$cluster = cluster
      box$table = table
      box = filter(box, type!="FALSE")
      if (nrow(box) > 1) {
        box = arrange(box, type=="FALSE")
        box = box[1,]; cat("Two prices where one should be?", box$text.new)
      }
    } 
    if (is.null(boxes[[i]]) || nrow(box) == 0) { # including newly emptied box
      box = makeCheckBox(missing.boxes[i,], type = "forBbox")
      box$text = ""
      box$confidence = 0
      box$price = FALSE
      box$text.new = ""
      box$type = "FALSE"
      box$cluster = cluster
      box$table = table
    }
    return(box) 
  })
}

#converts a data frame of left, bottom, right, top boxes to left, bottom, width, height for the x function
makeCheckBox <- function(df, type  = c("forCheckBox", "forBbox")) {
  if (type[1] == "forCheckBox") return(data.frame(left = df$left, bottom  = df$bottom, width = df$right - df$left, height = df$top - df$bottom))
  if (type[1] == "forBbox") return(data.frame(left = df$left, bottom  = df$bottom, right = df$left + df$width, top = df$bottom + df$height))
}

removeDuplicates <- function(table, buffer = 10, justify = "right") { #price or id table, should have left, bottom, right, top and confidence  
  look.columns = c("bottom", "top")
  if (justify == "right" | justify == "left") {look.columns = c(look.columns, justify)}
  look.for.duplicates = apply(table[, look.columns], 1, 
                              function(x) {apply(table[, look.columns], 1, 
                                  function(y) {max(abs(y-x))})})
  look.for.duplicates = data.frame(which(apply(look.for.duplicates, 1, "<",  buffer), arr.ind = T)) %>%  filter(., row > col)
  if(nrow(look.for.duplicates) > 0) {
    remove = apply(look.for.duplicates, 1, function(x) {
      x[which.min(table[x,"confidence"])]
    })
    table = table[-remove,]
  }
  table
}

extractPrice <- function(x, dollar = FALSE) 
  {
    if (dollar == FALSE) {
      extraction = str_extract(x, "[0-9]+\\.{0,1}[0-9]+.{0,1}$") #(?![0-9])
      if (length(extraction)==0) {return(FALSE)} else {return(extraction)}
    } else {
      extraction = str_extract(x, "\\$[0-9]+[.,][0-9]{2}")
      if (is.na(extraction)) {return(FALSE)} else {return(extraction)}
    }
}

addRows <- function(page.cols, buffer = page.cols$charheight/2) {
  # Assign row numbers by table
  all.prices = do.call("rbind", page.cols$prices)
  n.tables = n_distinct(page.cols$price_cols$table)
  
  all.prices = lapply(1:n.tables, function(i) {
    table.prices = filter(all.prices, table == i) %>% arrange(top, bottom)
    buffer2 = mean((table.prices %>% group_by(cluster) %>% mutate(diff = c(diff(top),NA)) %>% 
            summarize(med.diff = median(diff, na.rm = T)))[["med.diff"]])
    buffer = max(page.cols$charheight/2, buffer2/2)
    table.prices$row = c(1, cumsum(diff(sort(table.prices$top))>buffer)+1)
    table.prices
  })
  all.prices = do.call("rbind", all.prices)
  page.cols$prices = split(all.prices, all.prices$cluster)
  return(page.cols)
}

findHeader <- function(colData, column.header, buffer) {
  
  # look for column header (e.g. "Bottle")
  
  header = apply(colData, 1, function(x) {
    l = which(names(colData) == c("col.left"))
    r = which(names(colData) == c("col.right"))
    b = which(names(colData) == c("col.bottom"))
    # larger bottom buffer in case top line(s) of table missing
    text = filter(data1, left > x[l] - 2*buffer, left < x[r], bottom < x[b], top > x[b] - 4*buffer)
    # choose the lowest one on page if several
    if (nrow(text) > 0) {
      (filter(text, grepl(paste(column.header, collapse = "|"), text)) %>% arrange(-bottom) %>% select(text, bottom, top))[1,]
    }
  })
  header = lapply(header, function(x) {if (length(x)==0) {data.frame(text = NA, bottom = NA, top = NA)} else {x}})
  
  header = do.call("rbind", header)
  return(header)
}

#from duncan?
fixPrice =
  #
  # fixPrice("249")
  # fixPrice("12,49")
  function(x)
  {
    x = gsub(",", ".", x)
    gsub("^([0-9]+)([0-9]{2})$", "\\1.\\2", x)
  }

