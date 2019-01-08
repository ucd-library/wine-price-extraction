# Component function for wine_price_tables####
# Jane Carlen

source("wine-price-extraction/dsi/R/helper.R")

# 1. Get iterative column info  ####

# returns found prices, price column locations, number of prices found, character height,
# column justification, id column locations (left edge), and found ids
# image attribute only used for height
pageCols <- function(data1, img = NULL, img.height = NULL, show.plot = FALSE, column.header = c("Bottle", "Case", "Quart")) { #need img until image size attribute implemented
  
  #2. Create prices from data1 ####
  if(is.null(img) & is.null(img.height)) {return(break("Need image or image height"))}
  if (is.null(img.height)) height1 = dim(readJPEG(img))[1] #we'll use the image attribute here later
  
  data1$price = isPrice(data1$text)
  data1$type = isPrice(data1$text, maybe = TRUE)
  data1$center = (data1$left + data1$right)/2
  charheight = median((filter(data1, price) %>% mutate(diff = top - bottom))$diff)
  data1 = filter(data1, !(left <  2*charheight)) #margin: don't catch prices from previous page
  # Might move this to an external argument later, but since we don't know which pages have legit dollar prices it's here, dynamic for now
  if ( sum(isPrice(data1$text, dollar = T)) / (1 + sum(isPrice(data1$text, dollar = F))) > 2) {dollar = TRUE} else {dollar = FALSE}
  
  prices =  data1[isPrice(extractPrice(data1$text), dollar = dollar),] 
  prices = filter(prices, type != "FALSE") #if not dollar sign prices, dollar sign prices get removed here
  
  #cut down (just left for now) if glued to other stuff
  pricewidth = median(charWidth(data1[isPrice(data1$text),]))
  trimleft = prices[prices$type == "*price",]
  if (nrow(trimleft) > 0) {
    trimleft$left = trimleft$right - sapply(extractPrice(trimleft$text), nchar) * pricewidth+1
    trimleft$text = extractPrice(trimleft$text)
    prices[prices$type == "*price", ] = trimleft
  }
  

  #2a. initial column info ####
  
  # prices
  cols1 = priceCols(prices, minGap = charheight)
  k = cols1[["k"]] #suspected number of price columns
  just = cols1[["just"]]
  clust1 = cols1[["column_info"]]
  prices$cluster = clust1$clustering
  
  # split columns vertically - possible randomness here?
  for(i in 1:k) { # for loop due to cluster numbering 
    nclust = max(prices$cluster) + 1
    if (filter(prices, cluster == i) %>% select(just) %>% var() > charheight/4) {
      prices = rbind(filter(prices, cluster != i), splitCol(filter(prices, cluster == i), type = "v", new.index = nclust))
    }
  }
  
  # remove clusters of size 1 -- won't work for lm 
  prices$cluster_size = table(prices$cluster)[prices$cluster]
  prices = filter(prices, cluster_size > 1)
  prices$cluster = as.numeric(droplevels(factor(prices$cluster))) #shift cluster labels down if necessary
  sort(table(prices$cluster))
  
  colData = prices %>% group_by(cluster) %>%
    summarize(col.left = min(left), col.right = max(right),
              col.bottom = min(prices$bottom), col.top = max(prices$top))
  max.clust = which.max(table(prices$cluster)) #largest cluster
 
  #2b. Find more prices. Chose to do this to get most complete possible table. ####
  
  slope = img.height #default to vertical slope
  for (i in 1:max.clust) {
    
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
         if(show.plot) {plot(prices.lm.tmp[[just]], prices.lm.tmp$top, xlim = c(0,4000), ylim = c(0,img.height)); abline(v = median(prices.lm.tmp[[just]]), col = "red")}
      } else {
        lm.best = list(lm.left, lm.right, lm.center)[[which.max(lm.slopes)]]
        yintercept = lm.best$coefficients[[1]]
        near = abs((data1$top - data1[[just]]*slope - yintercept)/slope) < charheight #slope puts it on the right scale if slope is very large (near vertical)
        if(show.plot) {plot(prices.lm.tmp[[just]], prices.lm.tmp$top, xlim = c(0,4000), ylim = c(0,img.height)); abline(lm.best, col = "red")}
      }
      
      #look for more prices
      look2 = (near == TRUE & data1$price == FALSE)
      data1$text.new = "FALSE"
      if (sum (look2 & !data1$type %in% c("FALSE","TRUE")) > 0) {
         data1$text.new[look2 & !data1$type %in% c("FALSE","TRUE")] =  
            sapply(data1[look2 & !data1$type %in% c("FALSE","TRUE"),"text"], numToPrice)
         cat("grabbed", sum(data1$text.new!="FALSE"), "new prices\n") 
         data1$price[data1$text.new!="FALSE"] = TRUE
         data1$text[data1$text.new!="FALSE"] = data1$text.new[data1$text.new!="FALSE"]
      }
  }
    
  #2c. What do we have now? -- create prices2 ####
  prices2 = filter(data1, isPrice(extractPrice(data1$text))) %>% select(-matches("center"))
  
  #trim again
  trimleft = prices2[prices2$type == "*price",]
  if (nrow(trimleft) > 0) {
    trimleft$left = trimleft$right - sapply(extractPrice(trimleft$text), nchar) * pricewidth+1
    trimleft$text = extractPrice(trimleft$text)
    prices2[prices2$type == "*price", ] = trimleft
  }
  
  #recheck type
  prices2$type = isPrice(prices2$text, maybe = TRUE, dollar = dollar) 
  prices2 = filter(prices2, type != "FALSE")
  cat("progress: ", c(nrow(prices), "prices, then", nrow(prices2)), "\n")
  
  #2d. final price column info ####
  
  cols2 = priceCols(prices2, minGap = charheight)
  k = cols2[["k"]]
  just = cols2[["just"]]
  clust2 = cols2[["column_info"]]
  prices2$cluster = clust2$clustering
  
  # split columns vertically
  for(i in 1:k) { # for loop due to cluster numbering 
    nclust = max(prices2$cluster) + 1
    # only try a split if the left and right ranges are wide enough
    if (with(filter(prices2, cluster == i), (max(left) - min(left)) >= charheight/2 | (max(right) - min(right)) >= charheight/2)) {
          prices2 = rbind(filter(prices2, cluster != i), splitCol(filter(prices2, cluster == i), type = "v", new.index = nclust))
        }
  }
  
  # reformat and detect outliers by left and right edge and minimum gap from median
  prices2 = data.frame(prices2 %>% group_by(cluster) %>% 
                         filter(! ((abs(scale(left) > 2.5) & abs(left - median(left)) > charheight/2) &
                                 (abs(scale(right) > 2.5) & abs(right - median(right)) > charheight/2))) %>%
                                                         ungroup() %>% arrange(left))
  # relevel cluster if necessary
  prices2$cluster = as.numeric(as.factor(prices2$cluster))
  max.clust = which.max(table(prices2$cluster)) #largest cluster
    
  if(show.plot) {plot(tesseract(px1), cropToBoxes = F, bbox = prices2, img = px1)}

  colData2 = prices2 %>% group_by(cluster) %>%
    summarize(col.left = min(left), col.right = max(right),
              col.bottom = min(bottom), col.top = max(top),
              entries = n())
  
  #Try adding column headers:
  header = findHeader(colData2, column.header, buffer = charheight)
  colData2[, c("col.header","col.header.bottom","col.header.top")] = header
  colData2 = colData2  %>% arrange(cluster)
  
  #2e. id column info ####
  
  # numbers
  if (sum(data1$type=="ID") <= max(2, round(max.clust/3))) {
    tmp.numbers = filter(data1, type == "number", grepl(text, pattern="^[0-9]*$")) %>% arrange(left) %>% select(-matches("center"))
    idtype = "number"
  } else {
    tmp.numbers = filter(data1, type == "ID") %>% arrange(left) %>% select(-matches("center"))
    idtype = "ID"
  }
  
  #remove IDS in price columns
  for (i in 1:nrow(colData2)) {
    tmp.numbers = filter(tmp.numbers, ! (left > as.numeric(colData2[i,"col.left"]) & left < as.numeric(colData2[i,"col.right"])))
  }
  
  tmp.numbers.cum = sapply(1:max(tmp.numbers$left), function(x) {sum(x > tmp.numbers$left)})
  tmp.cpt = cpt.mean(tmp.numbers.cum, method='BinSeg', Q = nrow(colData2))@cpts
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
                 bottom > min(prices2$bottom) - 4*charheight) %>% arrange(bottom)
    
    # saw a weird case (1106.jpg) where one ID was being read twice.
    # Eliminate lower-confidence duplicate
    if(is.null(dim(ids))) ids = ids[[1]]
    ids = removeDuplicates(ids, justify = "none")
    ids$table = sapply(ids$left, function(x) {which.min(abs(x-id_cols))}) #assume one id col per table
    
    #remove if too close to a price column (may happen if periods aren't being read well)
    # check left-right same
    id_cols_overlap  = which(abs(outer(id_cols, colData2$col.left, "-")) < charheight, arr.ind = T)
  
    # check top-bottom overlap
    if (length(id_cols_overlap) > 0)   {
      for (row in 1:nrow(id_cols_overlap)) {
        if (length(id_cols > 0)) {
          # is top (bottom) of ids above (less than) bottom (top) of prices?
          c1 = min(filter(ids, table == id_cols_overlap[row])$bottom) < #top of ids above
          colData2[as.numeric(id_cols_overlap[row,2]),c("col.top")] #bottom of prices
          # is bottom (top) of ids below top (bottom) of prices?
          c2 = max(filter(ids, table == id_cols_overlap[row])$top) > #bottom of ids below
            colData2[as.numeric(id_cols_overlap[row,2]),c("col.bottom")] #top of prices
          if (c1 | c2) {
            id_cols = id_cols[-id_cols_overlap[row]]
            cat("Removed a column of ids which we think was actually prices\n")
            ids = filter(ids, !table == id_cols_overlap[row])
          }
        }
      }
    }
  }
    
  # if still IDs, save them
  if (length(id_cols) > 0) {
        ids$table = as.numeric(as.factor(ids$table))
        id_cols = ids %>% group_by(table) %>% summarize(col.left = min(left),
                                                    col.right = max(right),
                                                    col.top = max(top),
                                                    col.bottom = min(bottom),
                                                    entries = n()) %>% arrange(col.left)
        
    
        cat(nrow(ids), "ids found")    
  } else {id_cols = NULL; ids = NULL}
  
  #2e. return ####
  
  return(list(prices = split(prices2, prices2$cluster),
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
    
    if (exists("table", page.cols$price_cols)) {page.cols = addRows(page.cols)}
    # updated entries
    page.cols$price_cols$entries = sapply(page.cols$prices[page.cols$price_cols$cluster], nrow)
    # track price-finding progress
    page.cols$progress = c(page.cols$progress, sum(page.cols$price_cols$entries))
    # expand columns boundaries if necessary
    page.cols$prices = page.cols$prices[order(sapply(page.cols$prices, function(x) {x$cluster[1]}))] #next assumes this ordering
    page.cols$price_cols[,c("col.left","col.bottom")] = 
        do.call("rbind",lapply(page.cols$prices, function(x) { apply(x[,c("left", "bottom")], 2, min)}))[page.cols$price_cols$cluster,]
    page.cols$price_cols[,c("col.right","col.top")] = 
        do.call("rbind",  lapply(page.cols$prices, function(x) {apply(x[,c("right", "top")], 2, max)}))[page.cols$price_cols$cluster,]
    if (hasName(page.cols$price_cols, "table.size")) {
      page.cols$price_cols  = left_join(page.cols$price_cols, do.call("rbind", page.cols$prices) %>% 
                                          group_by(table) %>% 
                                          summarize(table.size = max(row)), by = "table", suffix = c(".x","")) %>% select(-contains("table.size.x"))
    }
  }
  
  if (type == "ids") {
    
    page.cols$ids$row = (page.cols$ids %>% group_by (table) %>% mutate(row = rank(bottom, ties.method = "first")))$row
    # updated entries
    page.cols$id_cols$entries = as.vector(table(page.cols$ids$table)[order(  as.numeric(names(table(page.cols$ids$table))) )]) #assumes page.cols$id_cols ordered by table
    # expand columns boundaries if necessary
    page.cols$id_cols[,c("col.left","col.bottom")] = (  page.cols$ids %>% group_by(table) %>% summarize(left = min(left), bottom = min(bottom)) %>% ungroup())[,-1]
    page.cols$id_cols[,c("col.right","col.top")] = (  page.cols$ids %>% group_by(table) %>% summarize(right = max(right), top = max(top)) %>% ungroup())[,-1]
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
  
  #function assumes everything's ordered by cluster number, so we make sure
  page.cols$prices = page.cols$prices[order(as.numeric(names(page.cols$prices)))] 
  page.cols$price_cols = page.cols$price_cols %>% arrange(cluster)
  
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
    if (sum(tmp.new) > 0) {
      #remove column if it's not a new or a best match for an old
      #shouldn't really do anything since we removed ID duplicates in pageCols
      keep.cols = unique(apply(compare.ids, 1, which.min)[!tmp.new])
      compare.ids = compare.ids[,keep.cols]
      tmp.lost = apply(compare.ids, 2, min) > page.cols$charheight
      new.ids = filter(tmp.boxes.from.cols_id[[x]], (apply(compare.ids, 1, min) > page.cols$charheight))
    
      #better ids? - should have same number of rows as oldx
      tmp.boxes.from.cols_id[[x]] = filter(tmp.boxes.from.cols_id[[x]], !tmp.new)
    
      # replace if original worse thn 90 and new at least 10 better
      tmp.replace = (old.ids$confidence[!tmp.lost])[keep.cols] < 90 &
        (old.ids$confidence[!tmp.lost][keep.cols] - tmp.boxes.from.cols_id[[x]]$confidence) < -10 #old - new
      
      if (sum(tmp.replace) > 0 & nrow(new.ids) > 0) {
        old.ids$text[!tmp.lost][tmp.replace] =  tmp.boxes.from.cols_id[[x]]$text[tmp.replace]
        new.ids$text.new = new.ids$text
        new.ids$table = x
        new.ids$row = 1
        new.ids = rbind(old.ids, new.ids[,names(page.cols$ids)]) %>% arrange(table, top)
        new.ids$row = 1:nrow(new.ids)
        return(new.ids)
      }
    } else {return(old.ids)}
  }))
  
  cat("Did addIds process add or take away IDs (>= 0 good)?",   new.ids  %>% group_by(table) %>% group_size() - page.cols$id_cols$entries, "\n")
  
  # update  stuff
  # ids
  page.cols = updatePageCols(page.cols, type = "ids")
  return(page.cols)
}

# 3. a) organize columns into tables + add table column to page.cols b) find table left edges using changepoint method. ####

pageTables <- function(data1, page.cols, buffer = page.cols$charheight/3) {
  
  page.cols$price_cols = page.cols$price_cols %>% arrange(col.bottom, col.top)
  
  # a) organize page into row ####
  
  page.cols$price_cols$table.row = c(1, #check column height over gap with next - max 30%
    cumsum((page.cols$price_cols$col.top - page.cols$price_cols$col.bottom)/ 
           (page.cols$price_cols$col.top - lead(page.cols$price_cols$col.bottom))  < .3) + 1)[1:length(page.cols$prices)] 
  
  page.cols$price_cols = page.cols$price_cols %>% arrange(table.row, col.left)
  
  # b ) organize columns into tables ####
  
  # order by cluster which is ordered by table.row, then left to rright
  
  # left as for loop, easier for debugging
  
  start.table = 1
  tmp.table = c()
  for (i in 1:max(page.cols$price_cols$table.row)) {
    tmp.cols = filter(page.cols$price_cols, table.row == i)
    tmp.prices = page.cols$prices[tmp.cols$cluster]
    v.prices = lapply(tmp.prices, function(x) {c(median(x$left), median(x$right))})
    v.widths = diff(unlist(v.prices)) #column width, break, column width, break, etc.
    tmp.table = c(tmp.table, start.table, 
                  cumsum( v.widths[seq(2, length(v.widths), by = 2)]/v.widths[seq(1, length(v.widths)-1, by = 2)] > 1.5 ) + start.table)
    start.table = max(tmp.table) +  1
  }

  page.cols$price_cols$table = tmp.table
  
  # c) find table locations using changepoint method ####
    # we can use this to find one that's after a space, @param.est$variance should be close to zero
    # tables is a list with  one entry for each table, one row in it for each column
  
  min.col.width = quantile(page.cols$price_cols$col.right - page.cols$price_cols$col.left, .5)/2 
  left.price_cols = page.cols$price_cols %>% arrange(table.row, table) %>% group_by(table.row)  %>%
    #to start use either the right point of the previous table (here we have only the leftmost column) or 0 for left margin
    mutate(cpt.left = lag(col.right, default = 0))  %>% ungroup() %>% group_by(table) %>%  
    filter(col.left == min(col.left))

  table.left = sapply(1:nrow(left.price_cols), function(x) {
    column = left.price_cols[x,]
    # look for table left boundary ####
    data1.order = data1 %>% filter(top < column$col.top + buffer, bottom > column$col.bottom - buffer, left < column$col.left, left > column$cpt.left) %>% arrange(left)
    data1.cum = sapply(1:column$col.left, function(x) {sum(data1.order$left <= x |data1.order$right <=x)})
    data1.lag = diff(data1.cum)
    data1.lag[1] = 0
    v.regions = cpt.var(data1.lag, method = "BinSeg", penalty = "None", Q = 1)
    #v.regions = cpt.var(data1.lag, method='PELT', penalty = )
    cpts = v.regions@cpts
    cpts = cpts[cpts < column$col.left - min.col.width] # max right
    
    if (length(cpts)>0) {
      table.left = max(0, max(cpts) - buffer)
    } else {table.left = column$cpt.left} # a safe guess
  return(table.left)
  })
  
  # left and right table bounds
  tmp.table.right = page.cols$price_cols %>% arrange(table) %>% group_by(table) %>% filter(col.right == max(col.right)) %>% .$col.right
  page.cols$price_cols = page.cols$price_cols  %>%
    mutate(table.left.cpt = rep(table.left, times = table(page.cols$price_cols$table)),
           table.right = rep(tmp.table.right, times = table(page.cols$price_cols$table)))
  
  # Add table and column var to price column table
  page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% mutate(column = rank(col.left))
  page.cols$prices = lapply(page.cols$prices, function(x) {
    x$table = filter(page.cols$price_cols, cluster == x$cluster[1])$table
    x$row = 1:nrow(x)
    x})
  
  #Add row information
  page.cols = addRows(page.cols)
  page.cols = updatePageCols(page.cols)
    
}

# 4. Check again for missing prices and now IDs too ####
##  if no ids, just look for mismatches between #prices in columns in tables 
##  if ids, also look for mismatches between #ids and #prices in tables 
##  Should do nothing if not mismatches found

addMissing <- function(page.cols, buffer = page.cols$charheight/2, img.height, px) {
  
  # assumes everything's ordered correctly
  if(!is.null(page.cols$ids)) {
    page.cols$price_cols$n_ids = page.cols$id_cols$entries[page.cols$price_cols$table]
  } else page.cols$price_cols$n_ids = 0
  
  # See what's missing
  page.cols$price_cols$table.size = left_join(page.cols$price_cols,  do.call("rbind", page.cols$prices) %>% 
                                     group_by(table) %>% 
                                     summarize(table.size = max(row)),
                                     by = "table", suffix = c(".x",""))$table.size
  
  page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% 
    mutate(missing.by.price = entries < table.size,
           missing.by.id = entries < n_ids,
           missing.id = ifelse(sum(n_ids)==0, FALSE, n_ids < entries)) #if no IDS, can't have missing IDs
  
  # 1. by within-table price column mismatch ####
  
  # list of tables we think have missing prices based on price column length mismatches
  tables.incomplete.by.price = page.cols$price_cols %>% group_by(table) %>% summarize(sum(missing.by.price)) %>% 
    filter(`sum(missing.by.price)` > 0) %>% .[,"table"]
 
  if (nrow(tables.incomplete.by.price) > 0) {
    cat("\nTable", unlist(tables.incomplete.by.price), "might be missing prices\n")

    for (t in unlist(tables.incomplete.by.price)) {
      
      table.prices = do.call("rbind", page.cols$prices) %>% filter(table == t)
      max.rows = max(table.prices$row)
      cols.missing = page.cols$prices[as.character(filter(ungroup(page.cols$price_cols), missing.by.price, table == t)$cluster)]
      cols.full = page.cols$prices[as.character(filter(ungroup(page.cols$price_cols), !missing.by.price, table == t)$cluster)]
      missing.spots = lapply(cols.missing, function(x) {which(! 1:max.rows %in% x$row)})
      
      if(length(missing.spots)>0) {
        cols.missing = lapply(1:length(cols.missing), function(x) {
            missing.boxes = data.frame(
              left = rep(quantile(cols.missing[[x]]$left, .2), length(missing.spots[[x]])),
              bottom = table.prices %>% filter(row %in% missing.spots[[x]]) %>% group_by(row) %>% 
                          summarize(bottom = median(bottom)) %>% ungroup() %>% select(bottom),
              width = rep(quantile(cols.missing[[x]]$right, .8), length(missing.spots[[x]])) -
                        rep(quantile(cols.missing[[x]]$left,.2), length(missing.spots[[x]])),
              height =  table.prices %>% filter(row %in% missing.spots[[x]]) %>% group_by(row) %>% 
                summarize(bottom = median(bottom), top = median(top), height = top - bottom) %>% ungroup() %>% select(height)
            )
            boxes = checkBoxes(missing.boxes, px, height = img.height, buffer = buffer, rbind = FALSE)
            if (is.null(boxes)) {boxes = vector(mode = "list", length = nrow(missing.boxes))}
            boxes = lapply(1:length(boxes), function(i) {
                if (!is.null(boxes[[i]])) {
                  box = boxes[[i]]
                  box$text.new = str_extract(box$text, "[0-9].*")
                  box$price = isPrice(box$text.new, dollar = FALSE, maybe = FALSE)
                  box$type = isPrice(box$text.new, maybe = T, dollar = F)
                  box$cluster = cols.missing[[x]]$cluster[1] #all cluster values in that table should be the same
                  box$table = cols.missing[[x]]$table[1]
                  box = filter(box, type!="FALSE")
                  if (nrow(box) > 1) {
                    box = arrange(box, type=="FALSE")
                    box = box[1,]; cat("Two prices where one should be?", box$text.new)
                  }
                } 
                if (is.null(boxes[[i]]) | nrow(box) == 0) { # including newly emptied box
                  box = makeCheckBox(missing.boxes[i,], type = "forBbox")
                  box$text = "-"
                  box$confidence = 0
                  box$price = FALSE
                  box$text.new = "-"
                  box$type = "FALSE"
                  box$cluster = cols.missing[[x]]$cluster[1] #all cluster values in that table should be the same
                  box$table = cols.missing[[x]]$table[1]
                }
               return(box) 
              })
            boxes = do.call("rbind", boxes)
            boxes$row = missing.spots[[x]]
            return(rbind(cols.missing[[x]], boxes[, names(cols.missing[[x]])]) %>% arrange(row))
        })
      }
      page.cols$prices[filter(page.cols$price_cols, table == t)$cluster] = c(cols.missing, cols.full)
      page.cols = updatePageCols(page.cols, type = "price")
    }    

  }
  
  # 2. by prices less than or misaligend with ids ####
  
  if (!is.null(page.cols$ids)) {
    all.prices = do.call("rbind", page.cols$prices)
    inner_join(page.cols$ids, all.prices, by = c("table","row"))[,c("top.x","top.y")]
    
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
        
        #find missing
        ids.missing.prices = which(apply(abs(outer(tmp.ids$top, tmp.prices$top, "-")), 1, min) > 2*buffer) #might need x if three-line name
        n.missing = length(ids.missing.prices)
        if (n.missing > 0) {
          missing.boxes = data.frame(
            left = rep(min(tmp.prices$left), n.missing),
            bottom = tmp.ids[ids.missing.prices, "bottom"],
            width = rep(max(tmp.prices$right) - min(tmp.prices$left), n.missing),
            height = tmp.ids[ids.missing.prices, "top"] - tmp.ids[ids.missing.prices, "bottom"]
          )
          boxes = checkBoxes(missing.boxes, px, height = img.height, buffer = buffer, psm = 6)
          #boxes = filter(boxes, price), since we know there's an ID, don't filter out, leave in with price False
          if (sum(!is.na(str_extract(boxes$text, "[0-9].*"))) > 0) {cat("Missing entries detected", boxes$text)}
        
          if (!is.null(boxes)) {
            boxes$text.new = boxes$text
            boxes$type = isPrice(boxes$text, maybe = T, dollar = F)
            boxes$cluster = page.cols$prices[[tmp.i]]$cluster[1] #all cluster values in that table should be the same
            boxes$table = tmp.table
            boxes$row = sapply(boxes$top, function(x) { tmp.prices[which.min(abs(x - tmp.prices$top)),"row"] }) #if wrong and hshould be new row will be corrected in addRows
            page.cols$prices[[tmp.i]] = rbind(page.cols$prices[[tmp.i]], boxes[,names(page.cols$prices[[tmp.i]])]) %>% arrange(top)
          }
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
            boxes$row = NA
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
  if (mean(page.cols$price_cols$col.header %in% column.header)==1) {
    page.cols$prices = lapply(page.cols$prices, function(x) {
      tmp.col = which(page.cols$price_cols$cluster == x$cluster[1])
      filter(x, x$bottom > as.numeric(page.cols$price_cols[tmp.col,"col.header.bottom"] - buffer*5))
    })
    page.cols = updatePageCols(page.cols, type = "price")
    if (!is.null(page.cols$ids)) page.cols = updatePageCols(page.cols, type = "ids")
  }
  
  #take this out?
  if (removeType == "prices" | removeType == "all") {
    page.cols$prices = lapply(page.cols$prices, removeDuplicates, buffer = buffer, justify = page.cols$justify)
    
    page.cols$prices = lapply(page.cols$prices, function(x) {
      if (page.cols$justify == "center") {
        x = filter(x, ! (abs(scale(  (x$left + x$right)/2 )) > 2.5 & type == "number") )
        x
      } else { #check both to be conservative. Otherwise may just be white space
        x = filter(x, ! ((abs(scale(x[["left"]])) > 2.5 & type == "number") & (abs(scale(x[["right"]])) > 2.5 & type == "number")) )
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
checkBoxes <- function(df, px, buffer = 10, level = "textline", height, checker = isPrice, psm = 3, rbind = TRUE) {
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
  if(!is.null(tmp.list)) {
    if (rbind) {do.call("rbind", tmp.list)} else {tmp.list}
  }
}

# df is left, bottom, width, height

# 5. Extract the names with words and images ####
nameBoxes <- function(data1, page.cols, prices = page.cols$prices, px , buffer = page.cols$charheight/2, charsize.cutoff = .4, psm = 3, text.level = "textline") { 
  #buffer on order of 1/2 small char size, 1/4 larege; charsize.cutoff determines by percentage same type size whether a line-above should be included in a name box
  #psm i(3) mportant for last stage with reocr
  if (!is.null(page.cols$id_cols)) {
    
    #initialize output
    table.boxes = vector(mode = "list", length = page.cols$n_id_cols)
    table.boxes.words = vector(mode = "list", length = page.cols$n_id_cols)
    table.boxes.words.reocr = vector(mode = "list", length = page.cols$n_id_cols)
    
    for (i in 1:page.cols$n_id_cols) {
      id_table = filter(page.cols$ids, table == i)
      price_table = subset(prices, page.cols$price_cols$table==i)[[1]]
      l = id_table$left - buffer
      r = rep(min(filter(page.cols$price_cols, table == i)$col.left), length(l)) + buffer
      b = id_table$bottom - buffer
      # we use the bottom (top) of the prices as the lower bound for the name box, but have to be careful if more prices than names
      t = price_table$top #better match without buffer
      t = price_table$top[apply(abs(outer(id_table$top, t, "-")), 1, which.min)] + buffer
      t[id_table$top > t] = id_table$top[id_table$top > t] #if id bottom (top) is lower use that one
      
      table.boxes[[i]] = data.frame(l, b, r, t)
      table.boxes.words[[i]] =  lapply(1:nrow(table.boxes[[i]]), function(x) {
        dplyr::filter(data1, left >= l[x] & left <= r[x], 
                      right < ( min(filter(page.cols$price_cols, table == i)$col.right) - 2*buffer ),
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
      n.tables = max(page.cols$price_cols$table)
      table.boxes = vector(mode = "list", length = n.tables)
      table.boxes.words = vector(mode = "list", length = n.tables)
      table.boxes.words.reocr = vector(mode = "list", length = n.tables)
      all.prices = do.call("rbind", prices)
      
      for (i in 1:n.tables) {
      
        #get table left, right, row bottoms and row tops
        tmp.cols = filter(page.cols$price_cols, table ==i)
        l = tmp.cols$table.left.cpt[1]
        r = min(tmp.cols$col.left) + 2*buffer 
        b = (filter(all.prices, table ==i) %>% group_by(row) %>% summarize(b = median(bottom)))[["b"]] - buffer
        t = (filter(all.prices, table ==i) %>% group_by(row) %>% summarize(t = median(top)))[["t"]] + buffer
      
        tmp.boxes = filter(data1, right >= l, left <= r - 2*buffer, top < max(t) + buffer*8, bottom > min(b) - buffer*8)
      
        #remove absurdly large text
        charwidth = charWidth(tmp.boxes)
        tmp.boxes = filter(tmp.boxes, abs(scale(charwidth)) < 10)
      
        char.types = charTypes(tmp.boxes, types = 2, conf.min = quantile(tmp.boxes$confidence, .5)) #only uses somewhat high-conf examples for finding char types
        char.sizes = char.types$means
      
        tmp.boxes$char.sizes = char.types$means[char.types$membership]
        if (buffer == "dynamic") {buffer= min(char.sizes)/2}
      
        #add *row* justification here later to choose bottom or top
        l = rep(l, length(b))
        r = rep(r, length(b))
      
        #if text above the price is in the bigger character type then include it the name
        table.boxes.words[[i]] = vector(mode = "list", length = length(t))
        tmp.price_cols = filter(page.cols$price_cols, table==i)
        for (j in 1:length(b)) {
          tmp.name = filter( tmp.boxes, left < r[j] & (top <= t[j] & bottom >= b[j] - 2*max(char.sizes)) ) #sets how high to look, assumes at most two-row names
          tmp.name = filter( tmp.name, right < tmp.price_cols$table.right[1] - buffer) #remove anything glued to a price
          if (nrow(tmp.name)==0) {next} else {
            # update b
            if (mean(tmp.name$char.sizes == max(char.sizes)) >= charsize.cutoff) { #could add another condition here for text width
              if (j == 1) {b[j] = min(tmp.name$bottom) - buffer} 
              if (j > 1) {b[j] = max(min(tmp.name$bottom - min(char.sizes)), t[j-1]) - buffer}
            }
            tmp.name = filter( tmp.boxes, left < r[j], bottom >= b[j], right < tmp.price_cols$table.right[1] - buffer,  top <= t[j])
            # update l - can only get bigger
            if (l[j] > min(tmp.name$left) & (l[j] - min(tmp.name$left) < page.cols$charheight*4)) {l[j] = min(tmp.name$left) - buffer}
            #-- put maximum move on this?
            # update r
            if (r[j] - max(tmp.name$right) < page.cols$charheight*4) {r[j] = max(tmp.name$right) + buffer}
          } #include another row in name, make sure below other price
          table.boxes.words[[i]][[j]] = tmp.name
      }

      # see if top overlaps with next row -- if so update for re-ocring
      # implemented differently than for IDS. Which is better?
      # find words that the current t value strikes through
      overlap_below = lapply(t, function(x) {
        words = filter(data1, bottom < x-2*buffer, x < top, left > min(l), right < median(r))
        if (nrow(words) > 2) {
          c1 = abs(charWidth(words) - min(char.sizes)) < abs(charWidth(words) - max(char.sizes)) #not the big text size
          c2 = charWidth(words) < 2*max(char.sizes) #remove weirdly large text
          words = filter(words, c1, c2)
          if (!is.null(words) & nrow(words) > 2) return(words) else {NULL}}
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
      
      table.boxes[[i]] = data.frame(left = l, bottom = b, right = r, top = t)

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
