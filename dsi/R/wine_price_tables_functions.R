# Component function for wine_price_tables####
# Jane Carlen

source("wine-price-extraction/dsi/R/helper.R")

# 1. Get iterative column info  ####

# returns found prices, price column locations, number of prices found, character height,
# column justification, id column locations (left edge), and found ids
# image attribute only used for height
pageCols <- function(data1, img = NULL, img.height = NULL, show.plot = FALSE, column.header = c("Bottle", "Case", "Quart")) { #need img until image size attribute implemented
  
  if(is.null(img) & is.null(img.height)) {stop("Need image or image height")}
  
  data1$price = isPrice(data1$text)
  data1$type = isPrice(data1$text, maybe = TRUE)
  data1$center = (data1$left + data1$right)/2
  charheight = median((filter(data1, price) %>% mutate(diff = top - bottom))$diff)
  data1 = filter(data1, !(left <  2*charheight)) #margin: don't want to catch prices from previous page
  
  prices =  data1[isPrice(data1$text),] 
  
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
  max.clust = which.max(clust1$clusinfo[,1]) #largest cluster
 
  #2b. Find more prices. Chose to do this to get most complete possible table. ####
  
  if (is.null(img.height)) height1 = dim(readJPEG(img))[1] #note we'll use the image attribute here later
  column.header = vector("character", k) # add column header "bottle" or "case" if possible
  column.header.bottom = vector("numeric", k) # add top line of "bottle" or "case" if possible
  
  slope = img.height #default to vertical slope
  for (i in 1:k) {
    
      #if at least two entries, consider non-vertical slope update
      if (sum(clust1$clustering == i) >= 2) {
        #incorporate column alignment
        prices.lm.tmp = subset(prices, clust1$clustering == i)
        try({lm.left = lm(img.height - top ~ left, data = prices.lm.tmp)})
        try({lm.right = lm(img.height - top ~ right, data = prices.lm.tmp)})
        try({lm.center = lm(img.height - top ~ center, data = prices.lm.tmp)})
        try({lm.slopes = sapply(list(lm.left, lm.right, lm.center), function(x) {(x$coefficients)[[2]]})})
        slope = max(lm.slopes)
      }
      if ( !is.finite(slope) | slope == img.height) {
         slope = img.height #probably don't need
         near = abs(data1[[just]] - median(prices.lm.tmp[[just]])) < charheight
         near.column.header = abs(data1[[just]] - median(prices.lm.tmp[[just]])) < 2*charheight #more leeway for works
         if(show.plot) {plot(prices.lm.tmp[[just]], prices.lm.tmp$top, xlim = c(0,4000), ylim = c(0,img.height)); abline(v = median(prices.lm.tmp[[just]]), col = "red")}
      } else {
        lm.best = list(lm.left, lm.right, lm.center)[[which.max(lm.slopes)]]
        yintercept = lm.best$coefficients[[1]]
        near = abs((data1$top - data1[[just]]*slope - yintercept)/slope) < charheight #slope puts it on the right scale if slope is very large (near vertical)
        near.column.header = abs((data1$top - data1[[just]]*slope - yintercept)/slope) < charheight*2
        if(show.plot) {plot(prices.lm.tmp[[just]], prices.lm.tmp$top, xlim = c(0,4000), ylim = c(0,img.height)); abline(lm.best, col = "red")}
      }
      
      #look for column header (e.g. "Bottle")
      look1 = (near.column.header == TRUE) 

      header.matches = sapply(column.header, function(x) {
        data1[look1,][adist(data1[look1, "text"], x) <= 1 | str_detect(data1[look1, "text"], x), ] 
      })

      #if (nrow(Bottle) >= 1 & nrow(Case) == 0 ) {column.header[i] = "Bottle"; column.header.bottom[i] = Bottle$bottom}
      #if (nrow(Case) >= 1 & nrow(Bottle) == 0 ) {column.header[i] = "Case"; column.header.bottom[i] = Case$bottom}
    
      #look for more prices
      look2 = (near == TRUE & data1$price == FALSE)
      data1$text.new = "FALSE"
      data1$text.new[look2 & data1$type == "number"] =  sapply(data1[look2 & data1$type == "number","text"], numToPrice)
      cat("grabbed", sum(data1$text.new!="FALSE"), "new prices\n") 
      data1$price[data1$text.new!="FALSE"] = TRUE
      data1$text[data1$text.new!="FALSE"] = data1$text.new[data1$text.new!="FALSE"]
  }
    
  
  #what do we have now?
  prices2 = filter(data1, price==TRUE) %>% select(-matches("center"))
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
  column.header = column.header[order(colData2$col_left)]
  column.header.bottom = column.header.bottom[order(colData2$col_left)]
  colData2$column.header = column.header #column.header already ordered left to right
  colData2$column.header.bottom = column.header.bottom #column.header.bottom already ordered left to right
  
  # reformate and detect outliers by right edge
  tmp.prices = lapply(1:max(prices2$cluster), function(x) filter(prices2, cluster==x))
  tmp.prices = lapply(tmp.prices, function(x) {
    filter(x, abs(scale(x$right)) < 2)
  })
  tmp.prices = tmp.prices[order(sapply(tmp.prices, function(x) {median(x$left)}))]
  
  #2d. id column info ####
  # numbers
  if (sum(data1$type=="ID") <= max(2, round(max.clust/3))) {
    tmp.numbers = filter(data1, type == "number", grepl(text, pattern="^[0-9]*$")) %>% arrange(left) %>% select(-matches("center"))
    tmp.numbers.cum = sapply(tmp.numbers$left, function(x) {sum(x > tmp.numbers$left)})
    idtype = "number"
  } else {
    tmp.numbers = filter(data1, type == "ID") %>% arrange(left) %>% select(-matches("center"))
    tmp.numbers.cum = sapply(tmp.numbers$left, function(x) {sum(x > tmp.numbers$left)})
    idtype = "ID"
  }
  
  #plot(tmp.numbers$left, tmp.numbers.cum, type="l")
  tmp.cpt = tmp.numbers$left[cpt.mean(tmp.numbers.cum, method='PELT')@cpts]
  if ( length(  which(diff(tmp.cpt) < charheight/3)  ) > 0 ) {
    tmp.cpt = tmp.cpt[-which(diff(tmp.cpt) < charheight/3)] #filter duplicates
  }
  
  #are there at least _ numbers near each cpt?
  id_cols = tmp.cpt[(rowSums(abs(outer(tmp.cpt, tmp.numbers$left,  "-")) < charheight/3)) >
                      max(table(prices2$cluster))/2]
  
  if(length(id_cols) > 0) {
    
    # make sure in a table
    ids = filter(tmp.numbers, type == idtype,
                 apply(abs(outer(tmp.numbers$left, id_cols, "-")), 1, min) < charheight,
                 bottom > min(prices$bottom) - 4*charheight) %>% arrange(bottom)
    
    # saw a weird case (1106.jpg) where one ID was being read twice.
    # Eliminate lower-confidence duplicate
    if(is.null(dim(ids))) ids = ids[[1]]
    ids = removeDuplicates(ids, justify = "none")
    ids$table = sapply(ids$left, function(x) {which.min(abs(x-id_cols))}) #assume one id col per table
    
    #remove if too close to a price column (may happen if periods aren't being read well)
    # check left-right same
    id_cols_overlap  = which(abs(outer(id_cols, colData2$col_left, "-")) < charheight, arr.ind = T)
  
    # check top-bottom overlap
    if (length(id_cols_overlap) > 0)   {
      for (row in 1:nrow(id_cols_overlap)) {
        if (length(id_cols > 0)) {
          # is top (bottom) of ids above (less than) bottom (top) of prices?
          c1 = min(filter(ids, table == id_cols_overlap[row])$bottom) < #top of ids above
          colData2[as.numeric(id_cols_overlap[row,2]),c("col_top")] #bottom of prices
          # is bottom (top) of ids below top (bottom) of prices?
          c2 = max(filter(ids, table == id_cols_overlap[row])$top) > #bottom of ids below
            colData2[as.numeric(id_cols_overlap[row,2]),c("col_bottom")] #top of prices
          if (c1 | c2) {
            id_cols = id_cols[-id_cols_overlap[row]]
            cat("Removed a column of ids which we think was actually prices")
            ids = filter(ids, !table == id_cols_overlap[row])
          }
        }
      }
    }
  }
    
  # if still IDs, save them
  if (length(id_cols) > 0) {
        ids$table = as.numeric(as.factor(ids$table))
        id_cols = ids %>% group_by(table) %>% summarize(col_left = min(left),
                                                    col_right = max(right),
                                                    col_top = max(top),
                                                    col_bottom = min(bottom),
                                                    entries = n()) %>% arrange(col_left)
        
    
        cat(nrow(ids), "ids found")    
  } else {id_cols = NULL; ids = NULL}
  
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
    page.cols$id_cols[,c("col_left","col_bottom")] = (  page.cols$ids %>% group_by(table) %>% summarize(left = min(left), bottom = min(bottom)) %>% ungroup())[,-1]
    page.cols$id_cols[,c("col_right","col_top")] = (  page.cols$ids %>% group_by(table) %>% summarize(right = max(right), top = max(top)) %>% ungroup())[,-1]
  }
  
  return(page.cols)
}

#    priceCols  Called by pageCols -- Find price columns, position and justification of text ####
priceCols <- function(prices, minGap) {
  prices$center = (prices$left + prices$right)/2
  clust = list(
    L = sapply(1:max(1, min(10, (nrow(prices)-2))), function(x) { # range of possible columns in the table
      pam1 = pam(prices$left, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min.size = min(pam1$clusinfo[,1]) #minimum cluster size
      clust.dist = abs(outer(c(pam1$medoids), c(pam1$medoids), "-"))
      suppressWarnings({min.dist = min(clust.dist[clust.dist > 0])})
      return(c(obj1, min.size, min.dist))
    }),
    R = sapply(1:max(1, min(10, (nrow(prices)-2))), function(x) {
      pam1 = pam(prices$right, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min.size = min(pam1$clusinfo[,1]) #minimum cluster size
      clust.dist = abs(outer(c(pam1$medoids), c(pam1$medoids), "-"))
      suppressWarnings({min.dist = min(clust.dist[clust.dist > 0])})
      c(obj1, min.size, min.dist)
    }),
    Ce = sapply(1:max(1, min(10, (nrow(prices)-2))), function(x) {
      pam1 = pam(prices$center, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min.size = min(pam1$clusinfo[,1]) #minimum cluster size
      clust.dist = abs(outer(c(pam1$medoids), c(pam1$medoids), "-"))
      suppressWarnings({min.dist = min(clust.dist[clust.dist > 0])})
      c(obj1, min.size, min.dist)
    })
  )
  
  #left or right align?
  just = with(clust, apply(rbind(clust[["L"]][1,],
                                     clust[["R"]][1,],
                                     clust[["Ce"]][1,]),
                               2, which.min) %>% table %>% which.max %>% as.vector)
  k = which.min(clust[[just]][1, clust[[just]][2,]>1 & clust[[just]][3,] > minGap])
  just = switch(just, "left", "right", "center")
  
  column1 = prices[[just]]
  clust1 = pam(column1, k)
  return(list(column_info = clust1, k = k, just = just))
}

# 2. Extract text from columns, find more prices and IDs ####

# check for new prices and return updated page.cols ####
addPrices <- function(page.cols, px) {
  
  tmp.boxes.from.cols_price = lapply(1:page.cols$n_price_cols, boxesFromCols,
                                     page.cols$price_cols, px, buffer = page.cols$charheight/3)
  
  # only add new ones, only replace ones that got better
  page.cols$prices = lapply(1:page.cols$n_price_cols, function(x) {
    
    #evaluate possible new prices
    tmp.boxes.from.cols_price[[x]]$price = isPrice(tmp.boxes.from.cols_price[[x]]$text, maybe = F, dollar = F)
    tmp.boxes.from.cols_price[[x]]$type = isPrice(tmp.boxes.from.cols_price[[x]]$text, maybe = T, dollar = F)
    tmp.boxes.from.cols_price[[x]]$text.new = extractPrice(tmp.boxes.from.cols_price[[x]]$text)
    tmp.boxes.from.cols_price[[x]]$type = isPrice(tmp.boxes.from.cols_price[[x]]$text.new, maybe = T, dollar = F)
    tmp.boxes.from.cols_price[[x]] = filter(tmp.boxes.from.cols_price[[x]], type !="FALSE", !str_detect(tmp.boxes.from.cols_price[[x]]$text, "\\$"))
    
    if (nrow(tmp.boxes.from.cols_price[[x]])>0) {
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
    }
    else {return(page.cols$prices[[x]])}
  }) 
  cat("Did addPrices process add or take away PRICES? (all >= 0 good):", sapply(page.cols$prices, nrow) - page.cols$price_cols$entries)
  
  # update other stuff
  page.cols = updatePageCols(page.cols)
}

# check for new IDs and return updated page.cols ####
addIds <- function(page.cols, px) {
  
  tmp.boxes.from.cols_id = lapply(1:page.cols$n_id_cols, boxesFromCols, page.cols$id_cols, px, buffer = page.cols$charheight/3)
  
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
  
  cat("Did addIds process add or take away IDs (>= 0 good)?\n",   new.ids  %>% group_by(table) %>% group_size() - page.cols$id_cols$entries)
  
  # update  stuff
  # ids
  page.cols = updatePageCols(page.cols, type = "ids")
  return(page.cols)
}

# for extracting new information from identified column
boxesFromCols <- function(index, colData, px, buffer = 5, psm = 3) {
  colDataRow = colData[index,]
  tpx = tesseract(px, pageSegMode = 3)
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

# 3. Find table locations using changepoint method. Check against IDs (if possible); group columns by table ####

pageTables <- function(data1, prices, page.cols, buffer = page.cols$charheight/3) {
  
  # first organize columns into tables ####
  n.pricecols = length(prices)
  lapply(prices, charWidth)
  # first organize columns into tables ####
  
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
  
  #reformat if an entry is a vector and has no dimension
  tables = lapply(tables, function(x) {if (is.null(dim(x[[1]]))) {matrix(x, nrow = 1)}}) 
  
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
  
  # we can use this to find one that's after a space, @param.est$variance should be close to zero
  # tables is a list with  one entry for each table, one row in  it for each column
  
  min.col.width = quantile(page.cols$price_cols$col_right - page.cols$price_cols$col_left, .5)/2 
  tables = lapply(tables, function(t) {
    if ( !is.null(dim(t)) && nrow(t)>1 ) { #compare right of first table to left of next table
      c1 = cpts < t[1,1] - (t[1,2] - t[2,1]) # max right
      c2 = t[1,1] - cpts > min.col.width # min width
      c3 = TRUE # <- add a check here to eliminate changepoint before a space that separates names from price cols
      attr(t, "left") = max(cpts[ c1 & c2 & c3 ]) - buffer
      return(t)
    } else {
      attr(t, "left") = max(cpts[cpts < t[1] - min.col.width])
      return(t)
    }
  })

  return(list(breaks = cpts, breaks.var = vars, tables = tables))
}

# find which table on the page each column is in
whichTable <- function(page.tables, page.cols) {
  if (!is.null(page.cols$id_cols)) {
    sapply(page.cols$price_cols$col_left, function(x) {
      tmp.close = min( (x - page.cols$id_cols$col_left)[x > page.cols$id_cols$col_left] )
      ifelse (is.finite(tmp.close), which(x - page.cols$id_cols$col_left == tmp.close), max(page.cols$ids$table)+1) # make up a table?
    })  
  } else {
    mid = sapply(page.cols$prices, function(x) {median(x$left+x$right)/2})
    apply(sapply(page.tables[["tables"]], function(t) {
      mid > min(t) & mid < max(t)
    }), 1, which)
  }
}

whichRows <- function(page.cols) {
  lapply(page.cols$prices, function(x) {
    
  })
}
# 4. Check again for missing prices and now IDs too ####
##  if no ids, just look for mismatches between #prices in columns in tables 
##  if ids, also look for mismatches between #ids and #prices in tables 
##  Should do nothing if not mismatches found

addMissing <- function(page.cols, page.tables, buffer = page.cols$charheight, img.height, px) {
  
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
  tables.incomplete.by.price = page.cols$price_cols %>% group_by(table) %>% summarize(sum(missing.by.price)) %>% 
    filter(`sum(missing.by.price)` > 0) %>% .[,"table"]
 
  if (nrow(tables.incomplete.by.price) > 0) {
    cat("\nTable", unlist(tables.incomplete.by.price), "might be missing prices")

    for (table in unlist(tables.incomplete.by.price)) {
      
      cols.missing = page.cols$prices[page.cols$price_cols$table == table & page.cols$price_cols$missing.by.price]
      cols.full = page.cols$prices[page.cols$price_cols$table == table & !page.cols$price_cols$missing.by.price]
      #find row gaps
      full.diffs = rowMeans(rbind(sapply(cols.full, function(x) {diff(x$top)}))) 
      full.top = rowMeans(rbind(sapply(cols.full, function(x) {x$top})))
      full.bot = rowMeans(rbind(sapply(cols.full, function(x) {x$bottom})))
      missing.spots = lapply(cols.missing, function(x) 1 + which(diff(x$bottom) > median(full.diffs)*1.5))
      
      if(length(missing.spots)>0) {
        cols.missing = lapply(1:length(cols.missing), function(x) {
          if (length(missing.spots[[x]]) > 0) {
            missing.boxes = data.frame(
              left = rep(median(cols.missing[[x]]$left), length(missing.spots[x])),
              bottom = full.bot[missing.spots[[x]]],
              width = rep(median(cols.missing[[x]]$right) - median(cols.missing[[x]]$left),
                          length(missing.spots[x])),
              height =  full.top[missing.spots[[x]]] - full.bot[missing.spots[[x]]]
            )
            boxes = checkBoxes(missing.boxes, px, height = img.height, buffer = buffer)
            boxes$text.new = str_extract(boxes$text, "[0-9].*")
            if (sum(is.na(boxes$text.new)) > 0) {cat("Missing entries not all prices, such as", boxes$text)}
            boxes$price = isPrice(boxes$text.new, dollar = FALSE, maybe = FALSE)
            boxes$type = isPrice(boxes$text, maybe = T, dollar = F)
            boxes$cluster = cols.missing[[x]]$cluster[1] #all cluster values in that table should be the same
            return(rbind(cols.missing[[x]], boxes[boxes$type!="FALSE", names(cols.missing[[x]])]) %>% arrange(top))
          }
        })
      }
      page.cols$prices[page.cols$price_cols$table == table & page.cols$price_cols$missing.by.price] = cols.missing
      page.cols = updatePageCols(page.cols, type = "price")
    }    

  }
  
  # 2. by prices less than or misaligend with ids ####
  # are prices and ids misaligned?
  
  if (!is.null(page.cols$ids)) {
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
        boxes = checkBoxes(missing.boxes, px, height = img.height, buffer = buffer)
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
  }
  
  # 3. by ids less than prices ####
  
  if (!is.null(page.cols$ids)) {
    
    page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% mutate(missing.id = n_ids < entries)
    incomplete.id = filter(page.cols$price_cols, missing.id)
    
    if (nrow(incomplete.id) > 0) {
      for (row in 1:nrow(incomplete.id)) {
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
            height = rep(2*page.cols$charheight, n.missing)
            )
          boxes = checkBoxes(missing.boxes, px, height = img.height, buffer = buffer)
          try({boxes = filter(boxes, grepl("[0-9].*", boxes$text), nchar(boxes$text) < 2*max(nchar(tmp.ids$text)))}) #some ids like N-1 only have one number
        
          if (!is.null(boxes) && nrow(boxes) > 0) {
            cat("Missing entries detected", boxes$text)
            boxes$text.new = boxes$text
            boxes$type = isPrice(boxes$text, maybe = T, dollar = F)
            boxes$table = tmp.table #all cluster values in that table should be the same
            page.cols$ids = rbind(page.cols$ids, boxes[,names(page.cols$ids)]) %>% arrange(table, top)
          }
        }
      }
      page.cols = updatePageCols(page.cols, type = "ids")
    }
  }
  return(page.cols)  
}

# Remove false positive prices by checking if they're numbers (not prices) and have weird left or right places
# Remove false positive ids by checking if their character width is way off --- don't remove so many that there are less ids than prices

removeExtra <- function(page.cols, buffer = page.cols$charheight, removeType = "prices", charwidth.cutoff = 2, column.header = c("Bottle", "Case", "Quart") ) { #charWidth.cutoff for removing false positive ids by charwidth outlier status
  
  if (! removeType %in% c("prices", "ids", "all")) stop("removeType must be 'prices', 'ids', or 'all'")
  if ( is.null(page.cols$ids) & removeType == "all" ) {removeType = "prices"}
    
  #if we found good bottle and case labels for all columns we can presume anything way above is a false positive
  if (mean(page.cols$price_cols$column.header %in% column.header)==1) {
    page.cols$prices = lapply(page.cols$prices, function(x) {
      tmp.col = which(page.cols$price_cols$cluster == x$cluster[1])
      filter(x, x$bottom > as.numeric(page.cols$price_cols[tmp.col,"column.header.bottom"] - buffer*5))
    })
    page.cols = updatePageCols(page.cols, type = "price")
    if (!is.null(page.cols$ids)) page.cols = updatePageCols(page.cols, type = "ids")
  }
  
  if (removeType == "prices" | removeType == "all") {
    page.cols$prices = lapply(page.cols$prices, removeDuplicates, buffer = buffer, justify = page.cols$justify)
    
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
    page.cols$ids = removeDuplicates(page.cols$ids, buffer = buffer, justify = page.cols$justify) %>% arrange(table, top)
    
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
checkBoxes <- function(df, px, buffer = 10, level = "textline", height, checker = isPrice, psm = 3) {
  tpx = tesseract(px, pageSegMode = psm)
  
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

# 5. Extract the names with words and images ####
nameBoxes <- function(data1, page.cols = NULL, prices = page.cols$prices, px , buffer = page.cols$charheight/2, page.tables = NULL, charsize.cutoff = .4, psm = 3, text.level = "textline") { 
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
      t = price_table$top #better match without buffer
      t = price_table$top[apply(abs(outer(id_table$top, t, "-")), 1, which.min)] + buffer
      
      table.boxes[[i]] = data.frame(l, b, r, t)
      table.boxes.words[[i]] =  lapply(1:nrow(table.boxes[[i]]), function(x) {
        dplyr::filter(data1, left >= l[x] & left <= r[x], 
                      right < ( min(filter(page.cols$price_cols, table == i)$col_right) - 2*buffer ),
                      bottom >= b[x], top <= t[x])
      })
      r.tmp =  sapply(table.boxes.words[[i]], function(x) max(x$right)) + buffer #update r
      r[is.finite(r.tmp)] = r.tmp[is.finite(r.tmp)]
      t[t<b] = b[t<b] + 2*page.cols$charheight # if stuck to row above, use default name height
      table.boxes[[i]] = data.frame(l, b, r, t) #update r
      
      
      # see if top overlaps with next row -- if so update for re-ocring
      overlap_below = lapply(t, function(x) {filter(data1, bottom < x, (x - bottom) < buffer/2, top > (bottom + buffer), left > min(l), right < median(r) )})
      update_t = which(sapply(overlap_below, nrow) > 2)
      cat("Found", length(update_t), "overlap(s) with row below when checking name boxes\n")
      if (length(update_t) > 0) {
        t_new = sapply(overlap_below[update_t], function(x) min(x$bottom))
        update_t = update_t[t_new > price_table$top[update_t]] # new t should be below old on the page]
        t[update_t] = t_new
        overlap_below = lapply(t, function(x) {filter(data1, bottom < x, (x - bottom) < buffer/2, top > (bottom + buffer), left > l, right < r )})
        update_t = sapply(overlap_below, nrow) > 2
        cat("Now", sum(update_t), "overlaps with row below when checking name boxes\n")
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
      
      tmp.boxes = filter(data1, right >= l, left <= min(colsintable[[1]]$left), top <= max(t) + buffer*8, bottom > min(b) - buffer*8)
      
      #remove absurdly large text
      charwidth = charWidth(tmp.boxes)
      tmp.boxes = filter(tmp.boxes, abs(scale(charwidth)) < 10)
      
      char.types = charTypes(tmp.boxes, types = 2, conf.min = quantile(tmp.boxes$confidence, .5)) #only uses somewhat high-conf examples for finding char types
      char.sizes = char.types$means
      
      tmp.boxes$char.sizes = char.types$means[char.types$membership]
      if (buffer == "dynamic") {buffer= min(char.sizes)/2}
      
      #filter out duplicates -- add row justification here later to choose bottom or top
      keep1 = (b - lead(b, 1)) < -page.cols$charheight | is.na(b - lead(b, 1)) & (t - lead(t, 1)) < -page.cols$charheight | is.na(t - lead(t, 1))
      b = b[keep1]
      t = t[keep1] 
      if (length(b) != length(t)) {break("in nameBoxes: price data incomplete")}
      
      l = rep(l, length(b))
      r = rep(r, length(b))
      
      #if text above the price is in the bigger character type then include it the name
      table.boxes.words[[i]] = vector(mode = "list", length = length(t))
      for (j in 1:length(b)) {
        tmp.name = filter( tmp.boxes, left < r[j] & (top <= t[j] & bottom >= b[j]-2*max(char.sizes)) ) #sets how high to look, assumes at most two-row names
        tmp.name = filter( tmp.name, right < mean(tables[[i]][1,])) #remove anything glued to a price
        if (nrow(tmp.name)==0) {next} else {
          # update b
          if (mean(tmp.name$char.sizes == max(char.sizes)) >= charsize.cutoff) { #could add another condition here for text width
            if (j == 1) {b[j] = min(tmp.name$bottom) - buffer} 
            if (j > 1) {b[j] = max(min(tmp.name$bottom - min(char.sizes)), t[j-1]) - buffer}
          }
          tmp.name = filter( tmp.boxes, left < r[j], bottom >= b[j], right < mean(tables[[i]][1,]),  top <= t[j])
          # update l - can only get bigger
          if (l[j] > min(tmp.name$left) & (l[j] - min(tmp.name$left) < page.cols$charheight*4)) {l[j] = min(tmp.name$left) - buffer}#-- put maximum move on this?
          # update r
          if (r[j] - max(tmp.name$right) < page.cols$charheight*4) {r[j] = max(tmp.name$right) + buffer}
        } #include another row in name, make sure below other price
        table.boxes.words[[i]][[j]] = tmp.name
      }

      # see if top overlaps with next row -- if so update for re-ocring
      # implemented differently than for IDS. Which is better?
      # find words that the current t value strikes through
      overlap_below = lapply(t, function(x) {
        x = filter(data1, bottom < x, x <top, left > min(l), right < median(r))
        if (nrow(x) > 2) {
          c1 = abs(charWidth(x) - min(char.sizes)) < abs(charWidth(x) - max(char.sizes)) #not the big text size
          c2 = charWidth(x) < 2*max(char.sizes) #remove weirdly large text
          x = filter(x, c1, c2)
          if (!is.null(x) & nrow(x) > 2) return(x) else {NULL}}
        })
      update_t = which(!sapply(overlap_below, is.null))
                       
      cat("Found", length(update_t), "overlap(s) with row below when checking name boxes\n")
      if (length(update_t) > 0) {
        t_new = sapply(overlap_below[update_t], function(x) min(x$bottom))
        update_t = update_t[t_new < t[update_t] & t_new > b[update_t] + buffer/2] # new t should be above old t and below bottom
        t_new = sapply(overlap_below[update_t], function(x) min(x$bottom))
        t[update_t] = t_new
        cat("Now", length(update_t), "overlaps with row below when checking name boxes\n")
      }
      
      table.boxes[[i]] = data.frame(left = rep(l, length(b)), bottom = b, right = rep(r, length(b)), top = t)

      # reocr
      table.boxes.words.reocr[[i]] = lapply(seq_along(b), function(x) {
        tpx = tesseract(px, pageSegMode = psm)
        SetRectangle(tpx, l[x], b[x], r[x]-l[x], t[x]-b[x] + buffer) 
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

# 6. Return final results - IDs, prices, names (with locations, words and image) ####
