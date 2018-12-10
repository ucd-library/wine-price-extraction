# Component function for wine_price_tables####
# Jane Carlen

source("wine-price-extraction/dsi/R/helper.R")

# 2. Get iterative column info  ####

# returns found prices, price column locations, number of prices found, character height,
# column justification, id column locations (left edge), and found ids
# image attribute only used for height
pageCols <- function(data1, img = NULL, img.height = NULL, show.plot = FALSE) { #need img until image size attribute implemented
  
  if(is.null(img) & is.null(img.height)) {stop("Need image or image height")}
  
  data1$price = isPrice(data1$text)
  data1$type = isPrice(data1$text, maybe = TRUE)
  charheight = median((filter(data1, price) %>% mutate(diff = top - bottom))$diff)
  data1 = filter(data1, !(left <  2*charheight)) #margin: don't want to catch prices from previous page
  
  prices =  data1[isPrice(data1$text),] 
  prices$center = (prices$left + prices$right)/2
  
  #2a. initial column info ####
  
  # prices
  cols1 = priceCols(prices, minGap = charheight)
  k = cols1[["k"]] #suspected number of price columns
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
  bottle.or.case = vector("character", k) # add column header "bottle" or "case" if possible
  bottle.or.case.bottom = vector("numeric", k) # add top line of "bottle" or "case" if possible
  
  for (i in 1:k) {
    #column alignment
    lm.tmp = lm(height1 - top ~ prices[[just]], data = prices, subset = (clust1$clustering == i)) 
    
    jitter.amt = .001
    while (is.na(lm.tmp$coefficients[[2]] )) {
          lm.tmp = lm(height1 - top ~ jitter(prices[[just]], amount = jitter.amt), data = prices,
                subset = (clust1$clustering == i)) 
          jitter.amt = jitter.amt*10
    }

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
    
    #look for column header "Bottle" or "Case"
    look1 = (near == TRUE) 
    #use levenshtein distance <= 1 or contains "Bottle" or "Case":
    Bottle = data1[look1,][adist(data1[look1, "text"], "Bottle") <= 1 | str_detect(data1[look1, "text"], "Bottle"), ] 
    Case = data1[look1,][adist(data1[look1, "text"], "Case") <= 1 | str_detect(data1[look1, "text"], "Case"), ]
    if (nrow(Bottle) >= 1 & nrow(Case) == 0 ) {bottle.or.case[i] = "Bottle"; bottle.or.case.bottom[i] = Bottle$bottom}
    if (nrow(Case) >= 1 & nrow(Bottle) == 0 ) {bottle.or.case[i] = "Case"; bottle.or.case.bottom[i] = Case$bottom}
    
    #look for more prices
    look2 = (look1 & data1$price == FALSE)
    data1$text.new = "FALSE"
    data1$text.new[look2 & data1$type == "number"] =  sapply(data1[look2 & data1$type == "number","text"], numToPrice)
    cat("grabbed", sum(data1$text.new!="FALSE"), "new prices\n") 
    data1$price[data1$text.new!="FALSE"] = TRUE
    data1$text[data1$text.new!="FALSE"] = data1$text.new[data1$text.new!="FALSE"]
  }
  bottle.or.case = bottle.or.case[order(colData$col_left)]
  bottle.or.case.bottom = bottle.or.case.bottom[order(colData$col_left)]
  
  #what do we have now?
  prices2 = filter(data1, price==TRUE)
  #recheck type
  prices2$type = isPrice(prices2$text, maybe = TRUE)
  cat("progress: ", c(nrow(prices), "prices, then", nrow(prices2)), "\n")
  
  #2c. final price column info ####
  
  cols2 = priceCols(prices2, minGap = charheight)
  k = cols2[["k"]]
  just = cols2[["just"]]
  clust2 = cols2[["column_info"]]
  prices2$cluster = clust2$clustering
  colData2 = prices2 %>% group_by(cluster) %>%
    summarize(col_left = min(left), col_right = max(right),
              col_bottom = min(prices2$bottom), col_top = max(prices2$top),
              entries = n())
  max.clust = which.max(clust2$clusinfo[,1]) #largest cluster
  colData2 = colData2 %>% arrange(col_left)
  colData2$bottle.or.case = bottle.or.case #bottle.or.case already ordered left to right
  colData2$bottle.or.case.bottom = bottle.or.case.bottom #bottle.or.case.bottom already ordered left to right
  
  # reformate and detect outliers by right edge
  tmp.prices = lapply(1:max(prices2$cluster), function(x) filter(prices2, cluster==x))
  tmp.prices = lapply(tmp.prices, function(x) {
    filter(x, abs(scale(x$right)) < 2)
  })
  tmp.prices = tmp.prices[order(sapply(tmp.prices, function(x) {median(x$left)}))]
  
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
    # make sure in a table
    ids = filter(tmp.numbers, type == idtype,
                 apply(abs(outer(tmp.numbers$left, id_cols, "-")), 1, min) < charheight,
                 bottom > min(prices$bottom) - 4*charheight) %>% arrange(bottom)
    
    # saw a weird case (1106.jpg) where one ID was being read twice.
    # Eliminate lower-confidence duplicate
    if(is.null(dim(ids))) ids = ids[[1]]
    ids = removeDuplicates(ids)
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
  
  cat(nrow(ids), "ids found")
  #2e. return ####
  
  return(list(prices = tmp.prices,
              price_cols = colData2,
              n_price_cols = max(colData2$cluster),
              
              progress = c(nrow(prices), nrow(prices2)),
              charheight = charheight,
              
              id_cols = id_cols, #approximate left locations of the number columns
              ids = ids, #the catalog numbers that go with the names
              n_id_cols = nrow(id_cols),
              idtype = idtype,
              justify = just))
}  

#Use after updating prices
updatePageCols <- function(page.cols, type = "price") {
  
  if (type == "price") {
    # updated entries
    page.cols$price_cols$entries = sapply(page.cols$prices, nrow)
    # track price-finding progress
    page.cols$progress = c(page.cols$progress, sum(page.cols$price_cols$entries))
    # expand columns boundaries if necessary
    page.cols$price_cols[,c("col_left","col_bottom")] = do.call("rbind",
                                                                lapply(page.cols$prices, function(x) { apply(x[,c("left", "bottom")], 2, min)}))
    page.cols$price_cols[,c("col_right","col_top")] = do.call("rbind",
                                                              lapply(page.cols$prices, function(x) {apply(x[,c("right", "top")], 2, max)}))
    if (hasName(page.cols$price_cols, "table.size")) {
      page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% mutate(table.size = max(entries)) %>% ungroup()
    }
  }
  
  if (type == "ids") {
    page.cols$id_cols$entries = as.vector(table(page.cols$ids$table)[order(  as.numeric(names(table(page.cols$ids$table))) )]) #assumes page.cols$id_cols ordered by table
    # expand columns boundaries if necessary
    page.cols$id_cols[,c("col_left","col_bottom")] = (  page.cols$ids %>% group_by(table) %>% summarize(left = min(left), bottom = min(bottom)) )[,-1]
    page.cols$id_cols[,c("col_right","col_top")] = (  page.cols$ids %>% group_by(table) %>% summarize(right = max(right), top = max(top)) )[,-1]
  }
  
  return(page.cols)
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

# check for new prices and return updated page.cols ####
addPrices <- function(page.cols) {
  
  tmp.boxes.from.cols_price = lapply(1:page.cols$n_price_cols, boxesFromCols,
                                     page.cols$price_cols,px1, buffer = page.cols$charheight/3)
  
  # only add new ones, only replace ones that got better
  page.cols$prices = lapply(1:page.cols$n_price_cols, function(x) {
    
    #evaluate possible new prices
    tmp.boxes.from.cols_price[[x]]$price = isPrice(tmp.boxes.from.cols_price[[x]]$text, maybe = F, dollar = F)
    tmp.boxes.from.cols_price[[x]]$type = isPrice(tmp.boxes.from.cols_price[[x]]$text, maybe = T, dollar = F)
    tmp.boxes.from.cols_price[[x]] = filter(tmp.boxes.from.cols_price[[x]], type !="FALSE", !str_detect(tmp.boxes.from.cols_price[[x]]$text, "\\$"))
    tmp.boxes.from.cols_price[[x]]$text.new = str_extract(tmp.boxes.from.cols_price[[x]]$text, "[0-9]+\\.{0,1}[0-9]+.{0,1}$")
    tmp.boxes.from.cols_price[[x]]$type = isPrice(tmp.boxes.from.cols_price[[x]]$text.new, maybe = T, dollar = F)
    tmp.boxes.from.cols_price[[x]] = filter(tmp.boxes.from.cols_price[[x]], type !="FALSE")
    
    #new prices? -- add later
    compare.col = abs(outer(tmp.boxes.from.cols_price[[x]]$bottom, page.cols$prices[[x]]$bottom, "-"))
    tmp.new = apply(compare.col, 1, min) > page.cols$charheight
    tmp.lost = apply(compare.col, 2, min) > page.cols$charheight # prices not found in new data
    new.prices = filter(tmp.boxes.from.cols_price[[x]], tmp.new)
    
    #better prices? - should have same number of rows as old
    tmp.boxes.from.cols_price[[x]] = filter(tmp.boxes.from.cols_price[[x]], !tmp.new)
    
    #what is the closest old price for each?
    tmp.compare = apply(cbind(page.cols$prices[[x]]$type[!tmp.lost], tmp.boxes.from.cols_price[[x]]$type), 1, compareTypes)
    page.cols$prices[[x]]$text.new[!tmp.lost][tmp.compare==1] = page.cols$prices[[x]]$text[!tmp.lost][tmp.compare==1]
    page.cols$prices[[x]]$text.new[!tmp.lost][tmp.compare==2] = tmp.boxes.from.cols_price[[x]]$text[tmp.compare==2]
    page.cols$prices[[x]]$text.new[tmp.lost] = page.cols$prices[[x]]$text[tmp.lost]
    
    #add new prices
    if (nrow(new.prices) > 0) {
      new.prices$cluster = median(page.cols$prices[[x]]$cluster)
      page.cols$prices[[x]] = rbind(page.cols$prices[[x]], new.prices[,names(page.cols$prices[[x]])]) %>% arrange(top)
    }
    
    #strip leading periods from new text
    page.cols$prices[[x]]$text.new = str_remove(page.cols$prices[[x]]$text.new, "^\\.+")
    return(page.cols$prices[[x]])
  })
  cat("Did this process add or take away PRICES? (all >= 0 good):", sapply(page.cols$prices, nrow) - page.cols$price_cols$entries)
  
  # update other stuff
  page.cols = updatePageCols(page.cols)
}

# check for new IDs and return updated page.cols ####
addIds <- function(page.cols) {
  
  tmp.boxes.from.cols_id = lapply(1:page.cols$n_id_cols, boxesFromCols, page.cols$id_cols, px1, buffer = page.cols$charheight/3)
  
  new.ids = do.call("rbind", lapply(1:page.cols$n_id_cols, function(x) {
    
    #are the things we found IDs?
    tmp.boxes.from.cols_id[[x]]$price = isPrice(tmp.boxes.from.cols_id[[x]]$text, maybe = F)
    tmp.boxes.from.cols_id[[x]]$type = isPrice(tmp.boxes.from.cols_id[[x]]$text, maybe = T)
    tmp.boxes.from.cols_id[[x]] = filter(tmp.boxes.from.cols_id[[x]], type == page.cols$idtype)
    #another heuristic to weed out non-IDs
    tmp.boxes.from.cols_id[[x]] = filter(tmp.boxes.from.cols_id[[x]], !grepl("[a-zA-Z-]{3}", tmp.boxes.from.cols_id[[x]]$text))
    
    old.ids = filter(page.cols$ids, table == x)
    compare.ids = abs(outer(tmp.boxes.from.cols_id[[x]]$bottom, old.ids$bottom, "-"))
    tmp.new = apply(compare.ids, 1, min) > page.cols$charheight
    #remove column if it's not a new or a best match for an old
    #shouldn't really do anything since we removed ID duplicates in pageCols
    keep.cols = unique(apply(compare.ids, 1, which.min)[!tmp.new])
    compare.ids = compare.ids[,keep.cols]
    tmp.lost = apply(compare.ids, 2, min) > page.cols$charheight
    new.ids = filter(tmp.boxes.from.cols_id[[x]], (apply(compare.ids, 1, min) > page.cols$charheight))
    
    #better ids? - should have same number of rows as old
    tmp.boxes.from.cols_id[[x]] = filter(tmp.boxes.from.cols_id[[x]], !tmp.new)
    
    # replace if original worse thn 90 and new at least 10 better
    tmp.replace = (old.ids$confidence[!tmp.lost])[keep.cols] < 90 &
      (old.ids$confidence[!tmp.lost][keep.cols] - tmp.boxes.from.cols_id[[x]]$confidence) < -10 #old - new
    old.ids$text[!tmp.lost][tmp.replace] =  tmp.boxes.from.cols_id[[x]]$text[tmp.replace]
    
    #add improved and new prices
    if (nrow(new.ids) > 0) {
      new.ids$text.new = new.ids$text
      new.ids$table = x
      new.ids = rbind(old.ids, new.ids[,names(page.cols$ids)]) %>% arrange(table, top)
    } else {new.ids = old.ids}
    return(new.ids)
  }))
  
  cat("Did this process add or take away IDs (>= 0 good)?",   new.ids  %>% group_by(table) %>% group_size() - page.cols$id_cols$entries)
  
  # update  stuff
  # ids
  page.cols = updatePageCols(page.cols, type = "ids")
  return(page.cols)
}

# for extracting new information from identified column
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

# For precendence between detected prices
compareTypes <- function(pair) {
  if (pair[1] != "TRUE" & pair[2] == "TRUE") {return(2)}
  if (pair[1] == "FALSE" & "number" %in% pair[2] ) {return(2)}
  else {return(1)}
}

# 4. Find table locations using changepoint method. Check against IDs (if possible); group columns by table ####

pageTables <- function(data1, prices, page.cols, buffer = page.cols$charheight/3) {
  
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
  # tables is a list of price columns organized by suspsected table
  tables = lapply(1:n.table, function(x) {tables[[x]]}) 
  
  # debug:
  # merge if left boundary of one table < 150% price column's distance from right boundary of previous,
  tables.unlist = lapply(tables, c)
  leftmost = sapply(tables.unlist, first) 
  rightmost  = sapply(tables.unlist, last) 
  median.price.col.width = median(page.cols$price_cols$col_right - page.cols$price_cols$col_left)
  to.join = (leftmost - lag(rightmost) < 1.5*median.price.col.width)
  to.join[1] = FALSE #easier than having an NA there
  if (sum(to.join) > 0) {
    tables2 = vector("list", sum(!to.join))
    bounds =  c(which(!to.join), length(tables) + 1)
    tables = lapply(seq_along(tables2), function(x) {
      do.call("rbind", subset(tables, seq_along(tables) %in% (bounds[x] : (bounds[x+1]-1))))
    })
    n.table = length(tables)
  }
  
  # find table left boundary ####
  # find page boundaries
  # by position
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
    if (t) {
      attr(tables[[t]], "left") = max(cpts[(cpts < (tables[[t]][1,1] - (tables[[t]][1,2] - tables[[t]][2,1])))]) - buffer
    } else {
      attr(tables[[t]], "left") = max(cpts[cpts < tables[[t]][1] - buffer])
    }
  }
  return(list(breaks = cpts, breaks.var = vars, tables = tables, charheight = page.cols$charheight))
}

# find which table on the page each column is in
whichTable <- function(page.tables, page.cols) {
  if (!is.null(page.cols$id_cols)) {
    sapply(page.cols$price_cols$col_left, function(x) {
      tmp.close = min( (x - page.cols$id_cols$col_left)[x > page.cols$id_cols$col_left] )
      which(x - page.cols$id_cols$col_left == tmp.close) 
    })  
  } else {
    mid = rowMeans(page.cols$price_cols[,c("col_left", "col_right")])
    apply(sapply(page.tables[["tables"]], function(t) {
      mid > min(t) & mid < max(t)
    }), 1, which)
  }
}

# 5. Check again for missing prices and now IDs too ####
##  if no ids, just look for mismatches between #prices in columns in tables 
##  if ids, also look for mismatches between #ids and #prices in tables 
##  Should do nothing if not mismatches found

addMissing <- function(page.cols, buffer = page.cols$charheight) {
  
  # assumes everything's ordered correctly
  if(!is.null(page.cols$ids)) {
    page.cols$price_cols$n_ids = page.cols$id_cols$entries[page.cols$price_cols$table]
  } else page.cols$price_cols$n_ids = 0
  
  # See what's missing
  page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% 
    mutate(table.size = max(entries),
           missing.by.price = entries < table.size,
           missing.by.id = entries < n_ids,
           missing.id = ifelse(sum(n_ids)==0, FALSE, n_ids < entries)) #if no IDS, can't have missing IDs
  
  # 1. by within-table price column mismatch ####
  
  # list of tables we think have missing prices based on price column length mismatches
  incomplete.by.price = page.cols$price_cols %>% group_by(table) %>% summarize(sum(missing.by.price)) %>% 
    filter(`sum(missing.by.price)` > 0) %>% .[,"table"]
  
  if (nrow(incomplete.by.price) > 0) {
    cat("Table", unlist(incomplete.by.price), "might be missing prices")
    for (elem in unlist(incomplete.by.price)) {
      tmp.cols.missing = page.cols$prices[page.cols$price_cols$table == elem & page.cols$price_cols$missing.by.price]
      tmp.cols.full = page.cols$prices[page.cols$price_cols$table == elem & !page.cols$price_cols$missing.by.price]
      full.diffs = rowMeans(rbind(sapply(tmp.cols.full, function(x) {diff(x$top)}))) #find row gaps
      full.top = rowMeans(rbind(sapply(tmp.cols.full, function(x) {x$top})))
      full.bot = rowMeans(rbind(sapply(tmp.cols.full, function(x) {x$bottom})))
      missing.spots = lapply(tmp.cols.missing, function(x) 1 + which(diff(x$bottom) > median(full.diffs)*1.5))
      if(length(missing.spots[[1]])>0) {
        missing.boxes = lapply(1:length(tmp.cols.missing), function(x) {
          data.frame(
            left = rep(median(tmp.cols.missing[[x]]$left), length(missing.spots[x])),
            bottom = full.bot[missing.spots[[x]]],
            width = rep(median(tmp.cols.missing[[x]]$right) - median(tmp.cols.missing[[x]]$left),
                        length(missing.spots[x])),
            height =  full.top[missing.spots[[x]]] - full.bot[missing.spots[[x]]]
          )})
        boxes = lapply(missing.boxes, checkBoxes, px1, height = height1, buffer = buffer)
        boxes = lapply(boxes, function(x) {
          x$text.new = str_extract(x$text, "[0-9].*")
          if (sum(is.na(x$text)) > 0) {cat("Missing entries not all prices", x$text)}
          return(x)
        })
        boxes = lapply(boxes, function(x) {x$price = isPrice(x$text.new, dollar = FALSE); return(x)})
        boxes = lapply(boxes, function(x) filter(x, !is.na(x$text.new))) #this would imply no numbers in the text
        for (i in 1:length(tmp.cols.missing)) {
          boxes[[i]]$type = isPrice(boxes[[i]]$text, maybe = T, dollar = F)
          boxes[[i]]$cluster = tmp.cols.missing[[i]]$cluster[1] #all cluster values in that table should be the same
          tmp.cols.missing[[i]] = rbind(tmp.cols.missing[[i]], boxes[[i]][,names(tmp.cols.missing[[i]])]) %>% arrange(top)
        }
        page.cols$prices[page.cols$price_cols$table == elem & page.cols$price_cols$missing.by.price] = tmp.cols.missing
        page.cols = updatePageCols(page.cols, type = "price")
      }
    }
  }
  
  # 2. by prices less than or misaligend with ids ####
  # are prices and ids misaligned?
  misaligned = sapply(1:length(page.cols$prices), function(x) {
    t.price.tmp = (page.cols$prices)[[x]]$top
    t.id.tmp = filter(page.cols$ids, table == as.numeric(page.cols$price_cols[x,"table"]))$top
    if (quantile(abs(t.price.tmp - t.id.tmp), .25) > (buffer * 4)) {
      cat("misaligned", x, "\n")
      return(TRUE)
    } else {return(FALSE)}
  })
  
  page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% 
    mutate(table.size = max(entries),
           missing.by.price = entries < table.size,
           missing.by.id = (entries < n_ids),
           missing.id = n_ids < entries) %>% ungroup() %>%
    mutate(missing.by.id = missing.by.id | misaligned)
  
  incomplete.by.id = filter(page.cols$price_cols, missing.by.id) # note this is a data frame rather than a list of table numbers
  
  if (nrow(incomplete.by.id) > 0) {
    cat(nrow(incomplete.by.id), "columns might be missing prices")
    for (row in 1:nrow(incomplete.by.id)) {
      #match the ids and prices
      tmp.table = as.numeric(incomplete.by.id[row,"table"])
      tmp.column = as.numeric(incomplete.by.id[row,"column"])
      tmp.ids = filter(page.cols$ids, table == tmp.table)
      tmp.i = which(page.cols$price_cols$table == tmp.table)[tmp.column]
      tmp.prices = page.cols$prices[[tmp.i]]
      
      tmp.lr = page.tables$tables[[tmp.table]]
      if (is.null(dim(tmp.lr))) {left = tmp.lr[1]; right = tmp.lr[2]} else {
        left = tmp.lr[tmp.column, 1]; right = tmp.lr[tmp.column, 2]
      }
      
      #find missing
      ids.missing.prices = which(apply(abs(outer(tmp.ids$top, tmp.prices$top, "-")), 1, min) > 2*buffer) #might need x if three-line name
      n.missing = length(ids.missing.prices)
      missing.boxes = data.frame(
        left = rep(left, n.missing),
        bottom = tmp.ids[ids.missing.prices, "bottom"],
        width = rep(right, n.missing) - rep(left, n.missing),
        height = tmp.ids[ids.missing.prices, "top"] - tmp.ids[ids.missing.prices, "bottom"]
      )
      boxes = checkBoxes(missing.boxes, px1, height = height1, buffer = buffer)
      #boxes = filter(boxes, price), since we know there's an ID, don't filter out, leave in with price False
      if (sum(!is.na(str_extract(boxes$text, "[0-9].*"))) > 0) {cat("Missing entries detected", boxes$text)}
      
      if (!is.null(boxes)) {
        boxes$text.new = boxes$text
        boxes$type = isPrice(boxes$text, maybe = T, dollar = F)
        boxes$cluster = page.cols$prices[[tmp.i]]$cluster[1] #all cluster values in that table should be the same
        page.cols$prices[[tmp.i]] = rbind(page.cols$prices[[tmp.i]], boxes[,names(page.cols$prices[[tmp.i]])]) %>% arrange(top)
      }
    }
    page.cols = updatePageCols(page.cols, type = "price")
  }
  
  
  # 3. by ids less than prices ####
  page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% mutate(missing.id = n_ids < entries)
  incomplete.id = filter(page.cols$price_cols, missing.id)
  
  if (nrow(incomplete.id) > 0) {
     for (row in 1:nrow(incomplete.by.id)) {
        #match the ids and prices
        tmp.table = as.numeric(incomplete.id[row,"table"])
        tmp.ids = filter(page.cols$ids, table == tmp.table)
        tmp.cluster = as.numeric(filter(page.cols$price_cols, table == tmp.table, column == 1)$cluster)
        tmp.prices = page.cols$prices[[which(sapply(page.cols$prices, function(x) {x$cluster[1]==tmp.cluster}))]]
        
        prices.missing.ids = which(apply(abs(outer(tmp.prices$top, tmp.ids$top, "-")), 1, min) > buffer * 4) #might need more buffer
        n.missing = length(prices.missing.ids)
        
        if(n.missing > 0) {
            missing.boxes = data.frame(
              left = rep(median(tmp.ids$left), n.missing),
              bottom = tmp.prices[prices.missing.ids,"bottom"],
              width = rep( quantile(tmp.ids$right, .9) - quantile(tmp.ids$left, .1), n.missing),
              height = rep(2*page.cols$charheight, n.missing))
          }
          
        boxes = checkBoxes(missing.boxes, px1, height = height1, buffer = buffer)
        boxes = filter(boxes, grepl("[0-9].*", boxes$text), nchar(boxes$text) < 2*max(nchar(tmp.ids$text))) #some ids like N-1 only have one number
      
        if (!is.null(boxes) & nrow(boxes) > 0) {
          cat("Missing entries detected", boxes$text)
          boxes$text.new = boxes$text
          boxes$type = isPrice(boxes$text, maybe = T, dollar = F)
          boxes$table = tmp.table #all cluster values in that table should be the same
          page.cols$ids = rbind(page.cols$ids, boxes[,names(page.cols$ids)]) %>% arrange(table, top)
        }
      }
    page.cols = updatePageCols(page.cols, type = "ids")
  }
  return(page.cols)  
}

# Remove false positive prices by checking if they're numbers (not prices) and have weird left or right places
# Remove false positive ids by checking if their character width is way off --- don't remove so many that there are less ids than prices

removeExtra <- function(page.cols, buffer = page.cols$charheight, removeType = "prices", charwidth.cutoff = 2, bottleTypes = c("Bottle", "Case") ) { #charWidth.cutoff for removing false positive ids by charwidth outlier status
  
  if (! removeType %in% c("prices", "ids", "all")) stop("removeType must be 'prices', 'ids', or 'all'")
  
  #if we found good bottle and case labels for all columns we can presume anything way above is a false positive
  if (mean(page.cols$price_cols$bottle.or.case %in% bottleTypes)==1) {
    page.cols$prices = lapply(page.cols$prices, function(x) {
      tmp.col = which(page.cols$price_cols$cluster == x$cluster[1])
      filter(x, x$bottom > as.numeric(page.cols$price_cols[tmp.col,"bottle.or.case.bottom"] - buffer*5))
    })
    page.cols = updatePageCols(page.cols, type = "price")
    page.cols = updatePageCols(page.cols, type = "ids")
  }
  
  if (removeType == "prices" | removeType == "all") {
    page.cols$prices = lapply(page.cols$prices, removeDuplicates, buffer = buffer)
    
    page.cols$prices = lapply(page.cols$prices, function(x) {
      if (page.cols$justify == "center") {
        x = filter(x, ! (abs(scale(  (x$left + x$right)/2 )) > 2 & type == "number") )
        x
      } else {
        x = filter(x, ! (abs(scale(x[[page.cols$justify]])) > 2 & type == "number") )
        x
      }
    }) 
    page.cols = updatePageCols(page.cols)
  }
  
  if (removeType == "ids" | removeType == "all") {
    extra_id = page.cols$id_cols$entries - aggregate(entries ~ table, FUN = max, data = page.cols$price_cols)$entries
    charwidth = charWidth(page.cols$ids)
    charwidth_range = max(charwidth) - min(charwidth)
    charwidth.tmp = abs(scale(charwidth))
    #remove strong outliers, be more careful with weaker outliers
    if (charwidth_range > median(charwidth)/3 & max(charwidth.tmp) > 3) { #make sure enough range to actually have outliers
      page.cols$ids = page.cols$ids[-which(charwidth.tmp>3),]
      page.cols = updatePageCols(page.cols, type = "ids")
    }
    if (max(extra_id) > 0 & (charwidth_range > median(charwidth)/3)) { #there are more ids than entries in at least one table
      # try to make them align by removing the worst matches according to charachter width; must have some range in character widths
      rank_charwidth.tmp = rank(-charwidth.tmp, ties.method = 'first')
      to_remove = which( charwidth.tmp > charwidth.cutoff & rank_charwidth.tmp %in% 1:sum(extra_id[extra_id>0]) ) #if a number is specified, filter out those with the least likely character widths and somewhat unlikely (how unlikely determined by charwidth.cutoff)
      if (identical( as.vector(table((page.cols$ids$table)[-to_remove])), aggregate(entries ~ table, FUN = max, data = page.cols$price_cols)$entries)) {
        page.cols$ids = page.cols$ids[-to_remove,]
        page.cols = updatePageCols(page.cols, type = "ids")
      }
    }
  }
  return(page.cols)
}

# re-check a box for a price
checkBoxes <- function(df, px, buffer = 10, level = "textline", height, checker = isPrice) {
  tpx = tesseract(px)
  
  tmp.list = apply(df, 1, function(x) {
    SetRectangle(tpx, dims = c(max(x[1] - buffer,0), max(x[2] - buffer,0), min(height,x[3] + 2*buffer), min(height, x[4] + 2*buffer)))
    gb = GetBoxes(tpx, level = level)
    if (gb$left[1] > 0) { #otherwise means an error or nothing found
      gb$text = gsub("\n", "", gb$text)
      if (!is.null(checker)) {
        gb$price = checker(gb$text)
      }
      return(gb)
    } else {}}
  )
  if(!is.null(tmp.list)) {do.call("rbind", tmp.list)}
}
# df is left, bottom, width, height

# 6. Extract the names with words and images ####
nameBoxes <- function(data1, page.cols = NULL, prices, px = px1, buffer = page.tables$charheight/2, page.tables = NULL, charsize.cutoff = .4, psm = 3, text.level = "textline") { 
  #buffer on order of 1/2 small char size, 1/4 larege; charsize.cutoff determines by percentage same type size whether a line-above should be included in a name box
  #psm i(3) mportant for last stage with reocr
  if (is.null(page.cols) & is.null(page.tables)) {stop("Need at least one of page.cols (if IDs present) or page.tables")}
  if (!is.null(page.cols$id_cols)) {
    
    #initialize output
    table.boxes = vector(mode = "list", length = page.cols$n_id_cols)
    table.boxes.words = vector(mode = "list", length = page.cols$n_id_cols)
    table.boxes.words.reocr = vector(mode = "list", length = page.cols$n_id_cols)
    
    for (i in 1:page.cols$n_id_cols) {
      id_table = filter(page.cols$ids, table == i)
      price_table = subset(prices, page.cols$price_cols$table==i)[[1]]
      l = id_table$left - buffer
      r = rep(min(filter(page.cols$price_cols, table == i)$col_left), length(l)) + buffer
      b = id_table$bottom - buffer
      # we use the bottom (top) of the prices as the lower bound for the name box, but have to be careful if more prices than names
      t = price_table$top + buffer 
      t = price_table$top[apply(abs(outer(id_table$top, t, "-")), 1, which.min)] + buffer
      
      table.boxes[[i]] = data.frame(l, b, r, t)
      table.boxes.words[[i]] =  lapply(1:nrow(table.boxes[[i]]), function(x) {
        dplyr::filter(data1, left >= l[x] & left <= r[x], 
                      right < ( min(filter(page.cols$price_cols, table == i)$col_right) - 2*buffer ),
                      bottom >= b[x], top <= t[x])
      })
      r =  sapply(table.boxes.words[[i]], function(x) max(x$right)) + buffer #update r
      table.boxes[[i]] = data.frame(l, b, r, t) #update r
      
      # see if top overlaps with next row -- if so update for re-ocring
      overlap_below = lapply(t, function(x) {filter(data1, bottom < x, (x - bottom) < buffer/2, top > (bottom + buffer), left > min(l), right < median(r) )})
      update_t = which(sapply(overlap_below, nrow) > 2)
      cat("Found", length(update_t), "overlap(s) with row below when checking name boxes")
      if (length(update_t) > 0) {
        t_new = sapply(overlap_below[update_t], function(x) min(x$bottom))
        update_t = update_t[t_new > price_table$top[update_t]] # new t should be below old on the page]
        t[update_t] = t_new
        overlap_below = lapply(t, function(x) {filter(data1, bottom < x, (x - bottom) < buffer/2, top > (bottom + buffer), left > l, right < r )})
        update_t = sapply(overlap_below, nrow) > 2
        cat("Now", sum(update_t), "overlaps with row below when checking name boxes")
        table.boxes[[i]] = data.frame(l, b, r, t) #update r
      }
      
      #reocr
      table.boxes.words.reocr[[i]] = lapply(1:nrow(table.boxes[[i]]), function(x) {
        tpx = tesseract(px, pageSegMode = psm)
        SetRectangle(tpx, l[x], b[x], r[x]-l[x], t[x]-b[x]) #buffers already in
        gb = GetBoxes(tpx, level = text.level)
        if(text.level  == "textline") {gb$text = gsub("\n", "", gb$text)}
        gb
      })
    }
  } else {
    tables = page.tables[["tables"]]
    prices.tables = rep(1:length(tables), times = sapply(tables, nrow)) #indexing by table
    table.boxes = vector(mode = "list", length = length(tables))
    table.boxes.words = vector(mode = "list", length = length(tables))
    table.boxes.words.reocr = vector(mode = "list", length = length(tables))
    for (i in 1:length(tables)) {
      
      #get table left, right, row bottoms and row tops
      l = attr(tables[[i]], "left") - buffer
      r = tables[[i]][1,1] + buffer #right is LEFT of prices
      colsintable = subset(prices, prices.tables==i)
      b = sort(do.call("rbind", colsintable)$bottom) - buffer
      t = sort(do.call("rbind", colsintable)$top) + buffer
      
      tmp.boxes = filter(data1, left >= l, left <= min(colsintable[[1]]$left), top <= max(t) + buffer*8, bottom > min(b) - buffer*8)
      
      #remove absurdly large text
      charwidth = (tmp.boxes$right-tmp.boxes$left)/nchar(tmp.boxes$text)
      tmp.boxes = filter(tmp.boxes, abs(scale(charwidth)) < 10)
      
      char.types = charTypes(tmp.boxes, types = 2, conf.min = 50) #only uses somewhat high-conf examples for finding char types
      char.sizes = char.types$means
      
      tmp.boxes$char.sizes = char.types$means[char.types$membership]
      if (buffer == "dynamic") {buffer= min(char.sizes)/2}
      
      #filter out duplicates
      b = b[(b - lead(b, 1)) < -page.cols$charheight | is.na(b - lead(b, 1))] - buffer
      t = t[(t - lead(t, 1)) < -page.cols$charheight | is.na(t - lead(t, 1))] + buffer
      
      if (length(b) != length(t)) {break("in nameBoxes: price data incomplete")}
      
      #if text above the price is in the bigger character type then include it the name
      table.boxes.words[[i]] = vector(mode = "list", length = length(t))
      for (j in 1:length(t)) {
        tmp.name = filter( tmp.boxes, left < r & (top <= t[j] & bottom >= b[j]-2*max(char.sizes)) ) #sets how high to look, assumes at most two-row names
        tmp.name = filter( tmp.name, right < mean(tables[[i]][1,])) #remove anything glued to a price
        if (nrow(tmp.name)==0) {next} else {
          # update b
          if (mean(tmp.name$char.sizes == max(char.sizes)) >= charsize.cutoff) { #could add another condition here for text width
            if (j == 1) {b[j] = min(tmp.name$bottom) - buffer} 
            if (j > 1) {b[j] = max(min(tmp.name$bottom - min(char.sizes)), t[j-1]) - buffer}
            if (max(tmp.name$right) > r) {r = max(tmp.name$right)}
          }
          tmp.name = filter( tmp.boxes, left < r & (top <= t[j] & bottom >= b[j]) )
          tmp.name = filter( tmp.name, right < mean(tables[[i]][1,]))
        } #include another row in name, make sure below other price
        table.boxes.words[[i]][[j]] = tmp.name
      }
      tmp.lines = max(sapply(colsintable, nrow))
      table.boxes[[i]] = data.frame(left = rep(l, tmp.lines), bottom = b, right = rep(r, tmp.lines), top = t)
      
      table.boxes.words.reocr[[i]] = lapply(seq_along(b), function(x) {
        tpx = tesseract(px, pageSegMode = psm)
        SetRectangle(tpx, l, b[x], r-l, t[x]-b[x] + buffer) 
        gb = GetBoxes(tpx, level = text.level)
        if (text.level == "textline") {gb$text = gsub("\n", "", gb$text)}
        gb
      })
      
    }
  }
  names(table.boxes) = paste("table", 1:length(table.boxes), sep = "_")
  names(table.boxes.words) = paste("table", 1:length(table.boxes.words), sep = "_")
  names(table.boxes.words.reocr) = paste("table", 1:length(table.boxes.words), sep = "_")
  return(list(locations = table.boxes, words_old = table.boxes.words, words = table.boxes.words.reocr))
}

# 7. Return final results - IDs, prices, names (with locations, words and image) ####
