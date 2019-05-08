# Component function for wine_price_tables####
# Jane Carlen

#source("helper.R")

# 1. Get iterative column info  ####

# See wine_price_pageCols.R: 

  # returns found prices, price column locations, number of prices found, character height,
  # column justification, id column locations (left edge), and found ids
  # image attribute only used for height
  # implements no prices in first 10% of page, no ids in last 10% (as deteremined by range of data1$left)

#Use after updating prices
updatePageCols <- function(page.cols, type = "price") {
  
  if (type == "price") {
    
    # updated entries
    page.cols$price_cols$entries = sapply(page.cols$prices[page.cols$price_cols$cluster], nrow)
    # track price-finding progress
    page.cols$progress = c(page.cols$progress, sum(page.cols$price_cols$entries))
  
    # remove a cluster if necessary and reorder, next assumes this order
    if (sum(page.cols$price_cols$entries == 0) > 0) {
        page.cols$price_cols = filter(page.cols$price_cols, entries !=0)
        if(nrow(page.cols$price_cols)==0) {break("Filtered out all entries. Stopping")}
        # relevel clusters
        cluster.old = page.cols$price_cols$cluster
        page.cols$price_cols$cluster  = as.numeric(as.factor(page.cols$price_cols$cluster))
        cluster.new = page.cols$price_cols$cluster
        names(cluster.new) = cluster.old
        page.cols$prices = page.cols$prices[sapply(page.cols$prices, nrow)>0]
        page.cols$prices = lapply(page.cols$prices, function(x) {
          x$cluster = cluster.new[[as.character(x$cluster[1])]]
          x
        })
        # relevel tables 
        table.old = page.cols$price_cols$table
        page.cols$price_cols$table  = as.numeric(as.factor(page.cols$price_cols$table))
        table.new = page.cols$price_cols$table
        names(table.new) = table.old
        page.cols$prices = lapply(page.cols$prices, function(x) {
          x$table = table.new[[as.character(x$table[1])]]
          x
        })
        
        # id information too
        if(!is.null(page.cols$ids)) {
          page.cols$ids$table = table.new[as.character(page.cols$ids$table)]
          page.cols$id_cols$table = table.new[as.character(page.cols$id_cols$table)]
        }
    }
    names(page.cols$prices) = sapply(page.cols$prices, function(x) {as.character(x$cluster[1])})
    page.cols$prices = page.cols$prices[order(sapply(page.cols$prices, function(x) {x$cluster[1]}))]
  
    
    # add/update rows (after other updates)
    if (exists("table", page.cols$price_cols)) {page.cols = addRows(page.cols)}
    
    # expand columnn boundaries if necessary
    page.cols$price_cols[,c("col.left","col.bottom")] = 
        do.call("rbind",lapply(page.cols$prices, function(x) { apply(x[,c("left", "bottom")], 2, min)}))[page.cols$price_cols$cluster,]
    page.cols$price_cols[,c("col.right","col.top")] = 
        do.call("rbind",  lapply(page.cols$prices, function(x) {apply(x[,c("right", "top")], 2, max)}))[page.cols$price_cols$cluster,]
    if (hasName(page.cols$price_cols, "table.size")) {
      page.cols$price_cols  = left_join(page.cols$price_cols, do.call("rbind", page.cols$prices) %>% 
                                          group_by(table) %>% 
                                          summarize(table.size = max(row)), by = "table", suffix = c(".x","")) %>% dplyr::select(-contains("table.size.x"))
    }
  }
  
  if (type == "ids") {
    
    page.cols$id_cols = page.cols$ids %>% group_by(table) %>% summarize(
      col.left = min(left),
      col.right = max(left),
      col.bottom = min(bottom),
      col.top= max(top),
      entries = n())
      
    page.cols$id_cols = page.cols$id_cols %>% arrange(table) #just in case
    if (exists("table", page.cols$ids) & exists("table", page.cols$price_cols)) {page.cols = addRows(page.cols, type = "ids")} else {
      page.cols$ids$row = (page.cols$ids %>% group_by (table) %>% mutate(row = rank(bottom, ties.method = "first")))$row
    }
  }
  
  return(page.cols)
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
  page.cols$prices = lapply(1:page.cols$n_price_cols, function(x) { try({
    
    if (nrow(tmp.boxes.from.cols_price[[x]])>0) {
      
      #evaluate possible new prices
      tmp.boxes.from.cols_price[[x]]$price = isPrice(tmp.boxes.from.cols_price[[x]]$text, maybe = F, dollar = F)
      tmp.boxes.from.cols_price[[x]]$type = isPrice(tmp.boxes.from.cols_price[[x]]$text, maybe = T, dollar = F)
      tmp.boxes.from.cols_price[[x]]$text.new = extractPrice(tmp.boxes.from.cols_price[[x]]$text)
      tmp.boxes.from.cols_price[[x]]$type = isPrice(tmp.boxes.from.cols_price[[x]]$text.new, maybe = T, dollar = F)
      tmp.boxes.from.cols_price[[x]] = filter(tmp.boxes.from.cols_price[[x]], type !="FALSE", 
                                              !str_detect(tmp.boxes.from.cols_price[[x]]$text, "\\$"))
      
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
      }
      return(page.cols$prices[[x]])
    }
    else {return(page.cols$prices[[x]])}
  })}) 
  
  if(sum(sapply(page.cols$prices, class)=="try-error")>0) stop("addPrices failed updating page.cols$prices")
  cat("\n addPrices added prices by column:", 
      sapply(page.cols$prices, nrow) - page.cols$price_cols$entries)
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
    tmp.boxes.from.cols_id[[x]] = filter(tmp.boxes.from.cols_id[[x]],
                                         !grepl("[a-zA-Z-]{3}", tmp.boxes.from.cols_id[[x]]$text))
    
    old.ids = filter(page.cols$ids, table == x)
    compare.ids = abs(outer(tmp.boxes.from.cols_id[[x]]$bottom, old.ids$bottom, "-"))
    tmp.new = apply(compare.ids, 1, min) > page.cols$charheight
    if (sum(tmp.new) > 0) {
      #remove column if it's not a new or a best match for an old
      #shouldn't really do anything since we removed ID duplicates in pageCols
      keep.cols = unique(apply(compare.ids, 1, which.min)[!tmp.new])
      if (length(keep.cols) > 0) {
        compare.ids = as.matrix(compare.ids[,keep.cols],  ncol = length(keep.cols))
        tmp.lost = apply(compare.ids, 2, min) > page.cols$charheight
        new.ids = filter(tmp.boxes.from.cols_id[[x]], (apply(compare.ids, 1, min) > page.cols$charheight))
    
        #better ids? - should have same number of rows as oldx
        tmp.boxes.from.cols_id[[x]] = filter(tmp.boxes.from.cols_id[[x]], !tmp.new)
    
        # replace if original worse thn 90 and new at least 10 better
        tmp.replace = (old.ids$confidence[!tmp.lost])[keep.cols] < 90 &
          (old.ids$confidence[!tmp.lost][keep.cols] - tmp.boxes.from.cols_id[[x]]$confidence) < -10 #old - new
      
        if (sum(tmp.replace) > 0 | nrow(new.ids) > 0) {
          old.ids$text[!tmp.lost][tmp.replace] =  tmp.boxes.from.cols_id[[x]]$text[tmp.replace]
          new.ids$text.new = new.ids$text
          new.ids$table = x
          new.ids$row = 1
          new.ids = rbind(old.ids, new.ids[,names(page.cols$ids)]) %>% arrange(table, top)
          return(new.ids)
        }
      } else {return(old.ids)}
    } else {return(old.ids)}
  }))
  
  cat("\n addIds added prices by column:",   new.ids  %>% group_by(table) %>% group_size() - page.cols$id_cols$entries, "\n")
  
  # update  stuff
  # ids
  page.cols = updatePageCols(page.cols, type = "ids")
  return(page.cols)
}

# 3. a) organize columns into tables + add table column to page.cols b) find table left edges using changepoint method. ####

pageTables <- function(data1, page.cols, buffer = page.cols$charheight/3) {
  
  page.cols$price_cols = page.cols$price_cols %>% arrange(col.bottom, col.top)
  
  # a) organize page into row ####
  
  page.cols$price_cols$table.row = c(1,  1 + cumsum(
            #check column height over gap with next - max 30%
              ((page.cols$price_cols$col.top - page.cols$price_cols$col.bottom)/ 
              (page.cols$price_cols$col.top - lead(page.cols$price_cols$col.bottom))  < .3 |
                #if slight overlap
            (page.cols$price_cols$col.top - page.cols$price_cols$col.bottom)/ 
              (page.cols$price_cols$col.top - lead(page.cols$price_cols$col.bottom)) > 5))
            )[1:length(page.cols$prices)] 
            # or just see if colump tops are ?
            #abs((page.cols$price_cols$col.bottom - lead(page.cols$price_cols$col.bottom))) > 
             # 5*page.cols$charheight))
  
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
    if (length(v.widths) == 1) {
      tmp.table = c(tmp.table, start.table)} else {
        
        #check for words in between - hacky, can make this better
        words.left = unlist(v.prices)[seq(2, length(unlist(v.prices)), by = 2)]
        words.right = lead(unlist(v.prices)[seq(1, length(unlist(v.prices)), by = 2)])
        check.words = sapply( 1: (length(words.left)-1), function(j) {
          nrow(filter(data1, left > words.left[j], right < words.right[j], 
                      top < tmp.cols$col.top[j], bottom > tmp.cols$col.top[j] - 10*page.cols$charheight))
        }) 
        
        tmp.table = c(tmp.table, start.table, 
                      cumsum( v.widths[seq(2, length(v.widths), by = 2)]/v.widths[seq(1, length(v.widths)-1, by = 2)] > 2
                              &  check.words > 2 ) + start.table)
      }
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
  tmp.table.right = (page.cols$price_cols %>% arrange(table) %>% group_by(table) %>% summarize(m = max(col.right)))$m
  page.cols$price_cols = page.cols$price_cols  %>% ungroup () %>%
    mutate(table.left.cpt = rep(table.left, times = table(page.cols$price_cols$table)),
           table.right = rep(tmp.table.right, times = table(page.cols$price_cols$table)))
  
  # Add table and column var to price column table
  page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% mutate(column = rank(col.left)) %>% ungroup()
  page.cols$prices = lapply(page.cols$prices, function(x) {
    x$table = filter(page.cols$price_cols, cluster == x$cluster[1])$table
    x$row = 1:nrow(x)
    x})
  
  # Add row information
  page.cols = addRows(page.cols)
  page.cols = updatePageCols(page.cols)
    
  # See if ID table numbers are labelled correctly:
  
  if (!is.null(page.cols$ids)) {
    page.cols$ids = page.cols$ids %>% arrange(table)
    old.table = (page.cols$ids %>% group_by(table) %>% summarize(old.table = first(table)))[["old.table"]]
    m = (page.cols$ids %>% group_by(table) %>% summarize(m = median(left)))[["m"]]
    t = (page.cols$ids %>% group_by(table) %>% summarize(t = max(top)))[["t"]]
    b = (page.cols$ids %>% group_by(table) %>% summarize(b = min(bottom)))[["b"]]
    new.table = sapply(1:length(m), function(i) {
      (filter(page.cols$price_cols, col.right > m[i], col.bottom < t[i], col.top > b[i]) %>% 
         arrange(col.left))[["table"]][1]
    })
    page.cols$ids$table = rep(new.table, table(page.cols$ids$table))
    page.cols$ids = filter(page.cols$ids, !is.na(table) )
    if (nrow(page.cols$ids) == 0) {
      page.cols$ids =  NULL
      page.cols$id_cols = NULL
      page.cols$n_id_cols = NULL
    } else {
      page.cols$id_cols = filter(page.cols$id_cols, table %in% page.cols$ids$table)
    }
  }
  
  page.cols
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
                          summarize(bottom = median(bottom)) %>% ungroup() %>% dplyr::select(bottom),
              width = rep(quantile(cols.missing[[x]]$right, .8), length(missing.spots[[x]])) -
                        rep(quantile(cols.missing[[x]]$left,.2), length(missing.spots[[x]])),
              height =  table.prices %>% filter(row %in% missing.spots[[x]]) %>% group_by(row) %>% 
                summarize(bottom = median(bottom), top = median(top), height = top - bottom) %>% ungroup() %>% dplyr::select(height)
            )
            boxes = checkBoxes(missing.boxes, px, height = img.height, buffer = buffer, rbind = FALSE)
            if (is.null(boxes)) {boxes = vector(mode = "list", length = nrow(missing.boxes))}
            boxes = checkMissingBoxes(boxes, missing.boxes, type = "price", cluster = cols.missing[[x]]$cluster[1], table = cols.missing[[x]]$table[1])
            boxes = do.call("rbind", boxes)
            boxes$row = missing.spots[[x]]
            return(rbind(cols.missing[[x]], boxes[, names(cols.missing[[x]])]) %>% arrange(row))
        })
      }
      page.cols$prices[filter(page.cols$price_cols, table == t)$cluster] = c(cols.missing, cols.full)
      page.cols = updatePageCols(page.cols, type = "price")
    }    

  }
  
  # 2. by prices less than or misaligned with ids ####
  
  if (!is.null(page.cols$ids)) {
    # go by table since sometimes one table has ids and another doesn't
    tables.with.ids = unique(page.cols$ids$table)
    all.prices = do.call("rbind", page.cols$prices)
    
    for (i in tables.with.ids) {
      table.prices = filter(all.prices, table == i)
      table.ids = filter(page.cols$ids, table == i)
      for (clust in unique(table.prices$cluster)) {
        cluster.prices = filter(table.prices, cluster == clust)
        ids.need.prices = 
          apply(abs(outer(table.ids$top, cluster.prices[["top"]], "-")), 1, min) > buffer & 
          apply(abs(outer(table.ids$bottom, cluster.prices[["bottom"]], "-")), 1, min) > buffer
        if (sum(ids.need.prices) > 0) {
          n.missing = sum(ids.need.prices)
          missing.boxes = data.frame(
              left = rep(median(cluster.prices$left), n.missing),
              bottom = table.ids$bottom[ids.need.prices],
              width = rep(median(cluster.prices$right) - median(cluster.prices$left), n.missing),
              height = table.ids$top[ids.need.prices] - table.ids$bottom[ids.need.prices]
            )
          boxes = checkBoxes(missing.boxes, px, height = img.height, buffer = buffer, psm = 6, rbind = FALSE)
          if (is.null(boxes)) {boxes = vector(mode = "list", length = nrow(missing.boxes))}
          boxes = checkMissingBoxes(boxes, missing.boxes, type = "number", cluster = clust, table = i)  
          boxes = do.call("rbind", boxes)
          boxes$row = NA
          all.prices = rbind(all.prices, boxes[, names(table.prices)])
        }
      }
    }
    all.prices = filter(all.prices, text !="") #for IDs don't keep the blank prices we find
    page.cols$prices = split(all.prices, all.prices$cluster)
  }
  page.cols = updatePageCols(page.cols, type = "price")
  
  # 3. by ids less than prices ####
  
  if (!is.null(page.cols$ids)) {
    
    tables.with.ids = unique(page.cols$ids$table)
    page.cols$price_cols = page.cols$price_cols %>% group_by(table) %>% mutate(missing.id = n_ids < entries) %>% ungroup()
    incomplete.by.id = filter(page.cols$price_cols, missing.id, table %in% tables.with.ids)
    
    if (nrow(incomplete.by.id) > 0) {
      for (row in 1:nrow(incomplete.by.id)) {
        #match the ids and prices
        tmp.table = as.numeric(incomplete.by.id[row,"table"])
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

removeExtra <- function(page.cols, buffer = page.cols$charheight, removeType = "prices", charwidth.cutoff = 2) {
  #charWidth.cutoff for removing false positive ids by charwidth outlier status
  
  if (! removeType %in% c("prices", "ids", "all")) stop("removeType must be 'prices', 'ids', or 'all'")
  if ( is.null(page.cols$ids) & removeType == "all" ) {removeType = "prices"}
    
  #if we found good bottle and case labels for all columns we can presume anything way above is a false positive
  if (sum(is.na(page.cols$price_cols$col.header)) == 0) {
    page.cols$prices = lapply(page.cols$prices, function(x) {
      tmp.col = which(page.cols$price_cols$cluster == x$cluster[1])
      filter(x, x$bottom > as.numeric(page.cols$price_cols[tmp.col,"col.header.bottom"] - buffer*5))
    })
    page.cols = updatePageCols(page.cols, type = "price")
    if (!is.null(page.cols$ids)) page.cols = updatePageCols(page.cols, type = "ids")
  }
  
  if (removeType == "prices" | removeType == "all") {
    page.cols$prices = lapply(page.cols$prices, removeDuplicates, buffer = buffer, justify = page.cols$justify)
    
    # check if wildly out of line by scaling & diffs
    page.cols$prices = lapply(page.cols$prices, function(x) {
      
      #remove prices with no overlap with rest of column and not (by left) close to another
      #not close to another
      x$center = (x$left + x$right)/2  #in case center justified
      diffs1 = abs(outer(x[[page.cols$justify]], x[[page.cols$justify]], "-")); diag(diffs1) = NA
      c1 = apply(diffs1, 1, min, na.rm = T) > buffer/2
      x = filter(x, ! ( (x$right < median(x$left)| x$left > median(x$right)) & c1) )
      
      if (page.cols$justify == "center") {
        x = filter(x, ! (abs(scale( x[["center"]] )) > 2.5 & type == "number") )
        x = x[,-which(names(x)=="center")]
      } else { #check both to be conservative. Otherwise may just be white space
        x = filter(x, ! ((abs(scale(x[["left"]])) > 2.5 & type == "number") & (abs(scale(x[["right"]])) > 2.5 & type == "number")) )
        x = x[,-which(names(x)=="center")]
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
    if (GetSourceYResolution(tpx)==0) {SetSourceResolution(tpx, 600)}
    gb = GetBoxes(tpx, level = level)
    if (length(gb$left) > 0 && gb$left[1] > 0) { #otherwise means an error or nothing found
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
# see wine_price_nameBoxes.R

# 6. Return final results - IDs, prices, names (with locations, words and image) ####
