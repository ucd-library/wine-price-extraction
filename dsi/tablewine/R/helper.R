# figure out if things are prices
# generally allows two characters of fudge
# dollar sets whether to allow dollars signs before prices.
  # for tables default should be false

isPrice = #Jane's version - different than Duncan's isPrice
  
  function(x, dollar = FALSE, maybe = FALSE, years = c(1800,1999), warn = FALSE) {
    
    x = as.character(x)
    
    sapply(x, function(x) {
      
      # price
      if ( grepl("^([0-9]+\\.[0-9]{2}$)", x)) {TRUE}
      
      # two prices glued together
      else if ( grepl("^([0-9]+\\.[0-9]{2})\\ ([0-9]+\\.[0-9]{2})", x)) {
        ifelse(maybe, "two prices", FALSE)
      }
     
      # price with comma instead of period
      else if( grepl("^[0-9]+,[0-9]{2}$", x)) {
        if (warn) (paste0("Is this (", x, ") a price with a comma instead of a period? ", collapse = " "))
        ifelse(maybe, "pr,ice", FALSE)
      }
      
      # price with dollar sign
      else if( grepl("^\\$([0-9]+)\\.[0-9]{2}.{0,2}$", x) | grepl("^\\$([0-9]+).{0,2}$", x)) {
        if (warn) (paste0("Is this (", x, ") a price with a dollar sign? ", collapse = " "))
        ifelse(dollar, TRUE, FALSE)
      }
      
      # price with stuff at end
      else if( grepl("^([0-9]+)\\.[0-9]{2}", x )) {
        if (warn) (paste0("Is this (", x, ") a price followed by something else? ", collapse = " "))
        ifelse(maybe,  "price*", FALSE)
      }

      # price with stuff at beginning
      else if( grepl(".*([0-9]+)\\.[0-9]{2}", x )) {
        if (warn) warning(paste0("Is this (", x, ") a price preceded by something else? ", collapse = " "))
        ifelse(maybe, "*price", FALSE) #useful for looking at lines
      }
      
      # price with dollar sign and stuff at end
      else if( grepl("^\\$([0-9]+)\\.[0-9]{2}", x) | grepl("^\\$([0-9]+)", x)) {
        if (warn) warning(paste0("Is this (", x, ") a price with a dollar sign followed by something else? ", collapse = " "))
        ifelse(dollar, TRUE, FALSE)
      }
      
      # price in cents with no 0
      # price with stuff at beginning
      else if( grepl("^\\.[0-9]{2}", x )) {
        if (warn) warning(paste0("Is this (", x, ") a price without a leading 0? ", collapse = " "))
        ifelse(maybe, ".price", FALSE) 
      }
      
      # number, at least two digits -- seperate out years
      else if(maybe & grepl("^([0-9]{2})", x)) {
        if (length(years) == 2) {
          first.dig.low = strtrim(years[1], 1)
          first.dig.high = strtrim(years[2], 1)
          year = as.numeric(stringr::str_extract(x, paste0("[", first.dig.low, "|", first.dig.high,"]", "[0-9]{3}")))
          ifelse (!is.na(year) & (year >= years[1] & year <= years[2]), "year", "number")
        } else {"number"}
      }
      
      #Alpha-numeric ID
      else if( grepl("^[nN].{0,1}[0-9]+$", x)) {
        if (warn) (paste0("This  (", x, ") seems to be an ID ", collapse = " "))
        ifelse(maybe, "ID", FALSE)
      }
      
      else{FALSE}
    })
  }

numToPrice = function(numAsChar, dollar = FALSE) {
  #dollar False to not allow numbers will dollar sign in front 
  #order of options is important (don't reorder)
  
  #option 1, just have to remove extra stuff at end, e.g. a period -- at most two
  if( grepl("^([0-9]+)\\.[0-9]{2}.{0,2}$", numAsChar)) return(stringr::str_extract(numAsChar, "^([0-9]+)\\.[0-9]{2}"))
  
  #option 2, 4+ digit price with comma -- remove comma, allow at most 9,999.99
  if (grepl("^[0-9]{1},[0-9]{3}\\.[0-9]{2}", numAsChar)) { return(gsub(",","",numAsChar))}
  
  #option 3, number with wrong puncuation
  if( grepl("^([0-9]+)[\\ |,][0-9]{2}", numAsChar)) {
    return(gsub("[\\ |,]", "\\.", stringr::str_extract(numAsChar, "^([0-9]+)[\\ |,][0-9]{2}")))
  }

  #option 4, number with stuff before -- allow at most lots
  if (dollar & grepl("^.([0-9]+)\\.[0-9]{2}", numAsChar)) return(stringr::str_extract(numAsChar, "([0-9]+)\\.[0-9]{2}"))

  if (!dollar) {
    if ( grepl("\\$[0-9]+", numAsChar)) return(FALSE)
    if ( grepl("([0-9]+)\\.[0-9]{2}", numAsChar)) return(stringr::str_extract(numAsChar, "([0-9]+)\\.[0-9]{2}"))
  }
  
  #option 5, number with no leading 0 -- add the 0
  if (grepl("^\\.[0-9]{2}", numAsChar)) { return(paste0("0",numAsChar))}
  
  #option 6 number without punctuation (remove any puncuation at end -- allow at most 2 after)
  if (grepl("[0-9]{3,10}.{0,2}$", numAsChar)) {
    numAsChar = stringr::str_extract(numAsChar, "[0-9]{3,10}")     
    num = strsplit(numAsChar, split="")[[1]]
    if (!tail(num,1) %in% c("0","5","9") & paste0(head(num,2), collapse = "") %in% c("18","19")) {
      warning(paste0(numAsChar, " is probably a year, excluded"))
      return(FALSE)
    }
    price = paste(paste0(num[1:(length(num)-2)], collapse = ""), ".",
                paste0(tail(num,2), collapse = ""), sep="")
    return(price)
  }
  
  return(FALSE)
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
  boxes.use = dplyr::filter(boxes, confidence >= conf.min) # only use somewhat high conf boxes for finding char types
  boxes.kmeans = kmeans(boxes.use$charwidth, centers = max(1, min(nrow(boxes.use)-1, types)))
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
  
  if (GetSourceYResolution(tpx)==0) {SetSourceResolution(tpx, 600)}
  gb = GetBoxes(tpx, level = "textline")
  gb$text = gsub("\n", "", gb$text)
  return(gb)
}

# Function to see if prices should be clustered  by checking for different character sizes matching locational clusters
splitCol <- function(prices, type = "h", px = px1, buffer = 13, height = height1,
                     new.index = 1, min.presplit = 4, min.postsplit = 1, max.impurity = 0, min.chardiff = 2) {
  #h for horizonatl, #v for vertical
  if (nrow(prices) < min.presplit) {return(prices)}
  
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
    
    tmp.prices = prices %>% mutate(text = prices$text)
    tmp.prices$text[prices$text.new!=FALSE] = prices$text.new[prices$text.new!=FALSE]
    pricewidths = charWidth(tmp.prices) 
    
    if ((max(pricewidths) - min(pricewidths)) < min.chardiff) {return(prices)}
    
    # first row of compare.clusterings is within ss, second is min cluster size for that number of gropus
    compare.clusterings = sapply(1:min(floor(length(pricewidths)/3), length(unique(pricewidths))), function(x) {
      possible.cluster = kmeans(pricewidths, centers = x, nstart = 100)
      pcc = possible.cluster$cluster
      pure = identical(pcc, rep(pcc[!duplicated(pcc)], times = table(pcc)[order(pcc[!duplicated(pcc)])]))
      # use minimum gap created:
      #min_gap = min((prices %>% group_by(possible.cluster$cluster) %>% arrange(top) %>% summarize(tmp.min = min(bottom), tmp.max = max(top)) %>% mutate(x = lead(tmp.min, default = Inf) - tmp.max))$x)
      c(possible.cluster$tot.withinss, min(possible.cluster$size), pure)
    })
    # stop looking if clustering returns all clusters <=  minimum allowed size
    if (any(compare.clusterings[2,] > min.postsplit & compare.clusterings[3,] == 1)) {
      max.possible.vclusters = max(which(compare.clusterings[2,] > min.postsplit & compare.clusterings[3,] == 1))
    } else {max.possible.vclusters = 1}
    
    #see if character size clusters match location clusters
    if (max.possible.vclusters > 1) {
      cluster.purity = sapply(2:max.possible.vclusters, function(x) {
        charsize.cluster = cluster::pam(pricewidths, k = x) #switched to mediod clustering so more robust, less randomness
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
      box$text.new = stringr::str_extract(box$text, pattern)
      box$price = isPrice(box$text.new, dollar = FALSE, maybe = FALSE)
      box$type = isPrice(box$text.new, maybe = T, dollar = F)
      box$cluster = cluster
      box$table = table
      box = dplyr::filter(box, type!="FALSE")
      if (nrow(box) > 1) {
        box = arrange(box, type=="FALSE")
        box = box[1,]; cat("Two prices where one should be? ", box$text.new)
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
  if (nrow(table) > 1) {
    look.columns = c("bottom", "top")
    if (justify == "right" | justify == "left") {look.columns = c(look.columns, justify)}
    look.for.duplicates = apply(table[, look.columns], 1, 
                              function(x) {apply(table[, look.columns], 1, 
                                  function(y) {max(abs(y-x))})})
    look.for.duplicates = data.frame(which(apply(look.for.duplicates, 1, "<",  buffer), arr.ind = T)) %>%  dplyr::filter(., row > col)
    if(nrow(look.for.duplicates) > 0) {
      remove = apply(look.for.duplicates, 1, function(x) {
        x[which.min(unlist(table[unlist(x),"confidence"]))]
      })
      table = table[-remove,]
    }
  }  
  
  table
}

extractPrice <- function(x, dollar = FALSE) 
  {
    if (dollar == FALSE) {
      extraction = stringr::str_replace(x, ",", "\\.") #first replace commas with periods
      extraction = stringr::str_extract(extraction, "[0-9|,]+\\.{0,1}[0-9]+.{0,1}$") #(?![0-9])
      if (length(extraction)==0) {extraction = stringr::str_extract(extraction, "[0-9]+\\.[0-9]{2}$")}
      if (length(extraction)==0) {return(FALSE)} else {return(extraction)}
    } else {
      extraction = stringr::str_extract(x, "\\$[0-9]+[.,][0-9]{2}")
      if (is.na(extraction)) {return(FALSE)} else {return(extraction)}
    }
    
}

addRows <- function(page.cols, buffer = page.cols$charheight/2, type = "price") {
  
  if (! type %in% c("price", "ids")) {stop("type to add rows to should be price or ids")}
  
  if (type == "price") {
    # Assign row numbers by table
    all.prices = do.call("rbind", page.cols$prices)
    n.tables = n_distinct(page.cols$price_cols$table)
  
    all.prices = lapply(1:n.tables, function(i) {
      table.prices = dplyr::filter(all.prices, table == i) %>% arrange(top, bottom)
      buffer = page.cols$charheight/2
      table.prices$row = c(1, cumsum(diff(sort(table.prices$bottom))>buffer | diff(sort(table.prices$bottom))>buffer)+1)
      table.prices
    })
    all.prices = do.call("rbind", all.prices)
    page.cols$prices = split(all.prices, all.prices$cluster)
  }
  
  if (type == "ids") {
    all.prices = do.call("rbind", page.cols$prices)
    list.table.ids = lapply(unique(page.cols$ids$table), function(i) {
      
      if (is.na(i)) {
        table.ids = dplyr::filter(page.cols$ids, is.na(table))
      } else {
        table.ids = dplyr::filter(page.cols$ids, table == i)
      }
      tmp.prices = dplyr::filter(all.prices, table == as.numeric(i)) %>% arrange(top, left)
      # price with midpoint (top + bottom)/2 that's closest to and below id bottom
      row.mids = tmp.prices %>% group_by(row) %>% summarize(mid = mean(top + bottom)/2)
      # find the closest lower-than row.mid for any id bottom and assign id row accordingly
      row.try = sapply(table.ids$bottom, function(x) {
          as.numeric(row.mids[min(which(x < row.mids$mid)),1])
        })
      
      if (sum(!is.na(row.try))==0) {
        warning("Something is wrong? All table ids are below table prices. ids rows not updated")
        table.ids = table.ids %>% arrange(top, left) %>% mutate(row = 1:nrow(.))
        return(table.ids)
      } else if (sum(is.na(row.try))>0) {
        warning(cat("Some ids, (", sum(is.na(row.try)), ") are below table prices.\n"))
        row.try[is.na(row.try)] = max(row.try, na.rm = T) + 1:sum(is.na(row.try))
        table.ids$row = row.try
        return(table.ids)
      } else {
        table.ids$row = row.try
        return(table.ids)
      }
    })
    page.cols$ids = do.call("rbind", list.table.ids)
  }
  
  return(page.cols)
}

findHeader <- function(colData, data1, column.header, buffer) {
  
  # look for column header (e.g. "Bottle")
  
  header = apply(colData, 1, function(x) {
    l = as.numeric(x[which(names(colData) == c("col.left"))])
    r = as.numeric(x[which(names(colData) == c("col.right"))])
    b = as.numeric(x[which(names(colData) == c("col.bottom"))])
    
    # larger bottom buffer in case top line(s) of table missing
    text1 = dplyr::filter(data1, left > l - 2*buffer, left < r, bottom < b, top > b - 6*buffer)
    
    if (nrow(text1) > 0) {
      # filter by levenshtein distance one or zero
      text1 = dplyr::filter(text1, sapply(tolower(text1$text), function(x) {min(RecordLinkage::levenshteinDist(x, tolower(column.header)))}) <= 1)
      # if the match is for a number is should be exact
      if (nrow(text1) > 0) {
        text1 = dplyr::filter(text1, ! (grepl(text, pattern = "^[0-9]+") & ! ( text %in% column.header ) ) )
      }
      # paste together if several (if bottle and case are both words we shoudl be able to figure out which is right using the prices)
      if (nrow(text1) > 0) {
        text2 = text1 %>% 
          summarize(bottom = min(bottom), top = max(top), text = paste(text, collapse = " "))
        #(text1 %>% arrange(-bottom))[1,c("text", "bottom", "top")] # for only bottom-most
      }
    }
  })
  header = lapply(header, function(x) {if (length(x)==0) {data.frame(text = NA, bottom = NA, top = NA)} else {x}})
  header = do.call("rbind", header)
  if (length(header)==0) {header = data.frame(text = rep(NA, nrow(colData)), bottom = rep(NA, nrow(colData)),
                                              top = rep(NA, nrow(colData)))}
  return(header)
}

# Detect outliers from a column with this current standard:
# In our example price_subset is a cluster of prices
tableOutlier <- function(price_subset, maxGap) {
    ( abs( scale(price_subset$left)) >= 2.5 | abs(price_subset$left - median(price_subset$left)) > maxGap ) & 
    ( abs( scale(price_subset$right)) >= 2.5 | abs(price_subset$right - median(price_subset$right)) > maxGap)
}

# find minimum differences between any two elements in a vector
minDiffs <- function(vector) {
  x = abs(outer(vector, vector, "-"))
  diag(x) = NA
  apply(x, 1, min, na.rm = T)
}


# function used to rotate points back to their positions in the original image
# NOTE: CENTRAL ROTATION: the origin for rotation is not 0,0, but the middle of the page, it seems
# https://tpgit.github.io/Leptonica/skew_8c_source.html - documentation of leptonica deskew
  # I think second number in pixFindSkew is a type of conficence score
  # (DEFAULT_BINARY_THRESHOLD = 130) for leptonica?

# flip.y accounts for when images are plotted with flipped y scales
# assumes "points" has two columns listing x and y

rotatePoints <- function(points, angle, height, width, flip.y = FALSE) {
  center = c(width/2, height/2)
  angle1 = -angle*pi/180
  rotation1 = matrix(c(cos(angle1), -sin(angle1), sin(angle1), cos(angle1)), nrow = 2) #clockwise
  if (flip.y) {
    points[,2] = height - points[,2]
  }
  
  points1 =  t((rotation1 %*% (t(points) - center)) + center)
  return(points1)
}

#from duncan -- might use

thresholdImage =
  function(f, threshold = 220)    
  {
    pix = pixRead(f)
    p8 = pixConvertTo8(pix)    
    pb = pixThresholdToBinary(p8, threshold)
  }

fixPrice =
  #
  # fixPrice("249")
  # fixPrice("12,49")
  function(x)
  {
    x = gsub(",", ".", x)
    gsub("^([0-9]+)([0-9]{2})$", "\\1.\\2", x)
  }

