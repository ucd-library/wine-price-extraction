# Component function(s) for price_table_extraction####
# Jane Carlen

source("wine-price-extraction/dsi/R/helper.R")

# 1. Get iterative column info  ####

# returns found prices, price column locations, number of prices found, character height,
# column justification, id column locations (left edge), and found ids
# image attribute only used for height
# implements no prices in first 10% of page, no ids in last 10% (as deteremined by range of data1$left)

#    findCols  Called by pageCols -- Find price columns, position and justification of text ####
findCols <- function(prices, minGap) {
  prices$center = (prices$left + prices$right)/2
  # Compares objective function value for different numbers of columns and different text justifications
  # 3 rows returns are the objective function value, minimum cluster size (must be >1), and 
  clust = list(
    L = sapply(1:max(1, min(16, (nrow(prices)-2))), function(x) { # range of possible columns in the table
      pam1 = pam(prices$left, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min.size = min(pam1$clusinfo[,1]) #minimum cluster size
      clust.dist = abs(outer(c(pam1$medoids), c(pam1$medoids), "-"))
      suppressWarnings({min.dist = min(clust.dist[clust.dist > 0])})
      return(c(obj1, min.size, min.dist))
    }),
    R = sapply(1:max(1, min(16, (nrow(prices)-2))), function(x) {
      pam1 = pam(prices$right, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min.size = min(pam1$clusinfo[,1]) #minimum cluster size
      clust.dist = abs(outer(c(pam1$medoids), c(pam1$medoids), "-"))
      suppressWarnings({min.dist = min(clust.dist[clust.dist > 0])})
      c(obj1, min.size, min.dist)
    }),
    Ce = sapply(1:max(1, min(16, (nrow(prices)-2))), function(x) {
      pam1 = pam(prices$center, k = x)
      obj1 = pam1$objective[[2]] #evaluation of cluster objective
      min.size = min(pam1$clusinfo[,1]) #minimum cluster size
      clust.dist = abs(outer(c(pam1$medoids), c(pam1$medoids), "-"))
      suppressWarnings({min.dist = min(clust.dist[clust.dist > 0])})
      c(obj1, min.size, min.dist)
    })
  )
  
  # left, right or center align?
  # decision based on which has the best objective function the most often
  clust.table = with(clust, apply(rbind(clust[["L"]][1,],
                                        clust[["R"]][1,],
                                        clust[["Ce"]][1,]),
                                  2, which.min) %>% table)
  just = as.numeric(names(clust.table)[which.max(clust.table)])
  
  k = which.min(clust[[just]][1, clust[[just]][2,]>1 & clust[[just]][3,] > minGap])
  just = switch(just, "left", "right", "center")
  
  column1 = prices[[just]]
  clust1 = pam(column1, k)
  return(list(column_info = clust1, k = k, just = just))
}

#    pageCols - main numeric column locater ----
pageCols <- function(data1, img = NULL, img.height = NULL, show.plot = FALSE, column.header = column.header) {
  # column.header is convered to lower for comparison
  # set dollar to something 
  
  #need img until image size attribute implemented
  if (is.null(img) & is.null(img.height)) {stop("Need image or image height")}
  if (is.null(img.height)) height1 = dim(readJPEG(img))[1] #we'll use the image attribute here later
  
  # 2. Add columns to data1 and create "prices" data frame
  page.Cols.initial = pageCols.initialize(data1, img = NULL, img.height = img.height, show.initial.plot = show.plot) 
  data1 = page.Cols.initial[["data1"]]
  prices = page.Cols.initial[["prices"]]
  charheight = page.Cols.initial[["charheight"]]
  dollar = page.Cols.initial[["dollar"]]
  
  #cut down (just left for now) if glued to other stuff
  pricewidth = median(charWidth(data1[isPrice(data1$text)|data1$type == "*price",]))
  trimleft = prices[prices$type == "*price",]
  if (nrow(trimleft) > 0) {
    trimleft$left = trimleft$right - sapply(extractPrice(trimleft$text), nchar) * pricewidth+1
    trimleft$text = extractPrice(trimleft$text)
    prices[prices$type == "*price", ] = trimleft
  }
  
  #2a. initial column info ####
  # We'll use this to find columns to search in for more prices
  
  cols1 = findCols(prices, minGap = charheight)
  k = cols1[["k"]] #suspected number of price columns
  just = cols1[["just"]]
  clust1 = cols1[["column_info"]]
  prices$cluster = clust1$clustering
  if (show.plot) {try({print(ggplot(prices, aes(x = right, y = bottom, color = as.factor(cluster))) + 
                               geom_point() + ylim(img.height, 0) + xlim(0, max(data1$right, na.rm =T)) +
                               ggtitle("pageCols 1: Prices by cluster")) })}
  
  # remove clusters of size 1 -- won't work for lm 
  prices$cluster_size = table(prices$cluster)[prices$cluster]
  prices = filter(prices, cluster_size > 1)
  
  # split columns vertically
  for(i in 1:k) { # for loop due to cluster numbering 
    nclust = max(prices$cluster) + 1
    # only try a split if the left and right ranges are wide enough
    if (with(filter(prices, cluster == i), 
             (max(left) - min(left)) >= charheight/2 | (max(right) - min(right)) >= charheight/2)) {
      prices = rbind(filter(prices, cluster != i),
                     splitCol(filter(prices, cluster == i), type = "v", new.index = nclust, 
                              buffer = charheight/2, min.chardiff = charheight/10))
    }
  }
  
  # remove clusters with no column structure, i.e. min horizontal distance between any two items is large
  max_diff = median(prices$right - prices$left) # must have at least two entries as close as this, 
  # use right (even if left-justified) because stuff gets glued to left side sometimes:
  remove1 = (prices %>% group_by(cluster) %>%
               mutate(diffs = minDiffs(right)) %>% 
               mutate(min_diffs = min(diffs), remove = min_diffs > max_diff))$remove
  prices = filter(prices, !remove1)
  
  # remove column outliers
  prices = data.frame((prices %>% group_by(cluster) %>% filter(!tableOutlier(.data, charheight)) %>%
                         ungroup() %>% arrange(left)))
  
  # remove clusters of size 1 -- won't work for lm 
  prices$cluster_size = table(prices$cluster)[prices$cluster]
  prices = filter(prices, cluster_size > 1)
  
  #shift cluster labels down if necessary
  prices$cluster = as.numeric(droplevels(factor(prices$cluster))) 
  if (show.plot) {try({print(ggplot(prices, aes(x = right, y = bottom, color = as.factor(cluster))) + 
                               geom_point() + ylim(img.height, 0) + xlim(0, max(data1$right, na.rm =T)) + 
                               ggtitle("pageCols 2: Prices by cluster after outlier removal"))})}
  max.clust = which.max(table(prices$cluster)) #largest cluster
  
  #2b. Find more prices. Chose to do this to get most complete possible table. ####
  
  data1 <- pageCols.search.column(prices, data1, img.height, just, charheight, show.search.plot = show.plot)
  
  #2c. What do we have now? -- create prices2 ####
  prices2 = filter(data1, isPrice(extractPrice(data1$text)) | 
                     isPrice(extractPrice(data1$text.new)))[, -which(names(data1)=="center")]
  prices2 = filter(prices2, left > max(data1$left)*.1) #again create a left margin where prices can't be
  
  #trim again
  trimleft = prices2[prices2$type == "*price",]
  if (nrow(trimleft) > 0) {
    trimleft$left = trimleft$right - sapply(extractPrice(trimleft$text), nchar) * pricewidth+1
    trimleft$text = extractPrice(trimleft$text)
    prices2[prices2$type == "*price", ] = trimleft
  }
  
  # recheck type
  prices2$type = isPrice(prices2$text, maybe = TRUE, dollar = dollar) 
  prices2 = filter(prices2, type != "FALSE")
  cat("progress: ", c(nrow(prices), "prices, then", nrow(prices2)), "\n")
  if (show.plot) {try({print(ggplot(prices2, aes(x = right, y = bottom, color = type)) + 
                               geom_point() + ylim(img.height, 0) + xlim(0, max(data1$right, na.rm =T)) + 
                               ggtitle("pageCols 3: Prices2 by type"))})}
  
  # remove anything not within a column's distance of another price
  tmp.prices2 = prices2 %>% mutate(center = (left+right)/2)
  prices2 = filter(prices2, minDiffs(tmp.prices2[[just]]) < max_diff)
  
  #2d. final price column info ####
  
  cols2 = findCols(prices2, minGap = charheight)
  k = cols2[["k"]]
  just = cols2[["just"]]
  clust2 = cols2[["column_info"]]
  prices2$cluster = clust2$clustering
  if (show.plot) {try({print(ggplot(prices2, aes(x = right, y = bottom, color = as.factor(cluster))) + 
                               geom_point() + ylim(img.height, 0) + xlim(0, max(data1$right, na.rm =T)) + 
                               ggtitle("pageCols 4: Prices2 by cluster"))})}
  
  # Remove unlikely columns ----
  
  # remove clusters of size 1 -- won't work for lm 
  cluster_size = table(prices2$cluster)[prices2$cluster]
  prices2 = filter(prices2, cluster_size > 1)
  
  # renumber clusters if necessary
  prices2$cluster = as.numeric(droplevels(factor(prices2$cluster))) 
  
  # split columns vertically
  for(i in 1:k) { # for loop due to cluster numbering 
    nclust = max(prices2$cluster) + 1
    # only try a split if the left and right ranges are wide enough
    if (with(filter(prices2, cluster == i), 
             (max(left) - min(left)) >= charheight/2 | (max(right) - min(right)) >= charheight/2)) {
      prices2 = rbind(filter(prices2, cluster != i),
                      splitCol(filter(prices2, cluster == i), type = "v", new.index = nclust, 
                               buffer = charheight/2, min.chardiff = charheight/10))
    }
  }
  
  # remove clusters of size 1 -- won't work for lm 
  cluster_size = table(prices2$cluster)[prices2$cluster]
  prices2 = filter(prices2, cluster_size > 1)
  
  # remove column outliers
  prices2 = data.frame(prices2 %>% group_by(cluster) %>% filter(!tableOutlier(.data, charheight)) %>%
                         ungroup() %>% arrange(left))
  
  # remove clusters of size 1 -- won't work for lm 
  cluster_size = table(prices2$cluster)[prices2$cluster]
  prices2 = filter(prices2, cluster_size > 1)
  
  # If no prices left to analyze, stop.
  if (nrow(prices2)==0) {return(NULL)}
  
  # remove clusters with no column structure, i.e. min horizontal distance between any two items is large
  max_diff = median(prices2$right - prices2$left) # must have an entry as close as this, 
  tmp.prices2 = prices2 %>% group_by(cluster) %>% # use right (even if left-just) bc stuff can get glued to left side:
    mutate(diffs = minDiffs(right)) %>% 
    mutate(min_diffs = min(diffs), remove = min_diffs > max_diff) %>% ungroup()
  
  prices2 = filter(prices2, !tmp.prices2$remove)
  tmp.prices2 = filter(tmp.prices2, !tmp.prices2$remove)
  
  # remove clusters with few entries AND 
  # 1. too much (letter) text between prices:
  # for clusters of size three (more?) or less, check for too much A-z text between prices
  few.entries = 3
  toomany.words = 10
  remove1 = (prices2 %>% group_by(cluster) %>% 
               mutate(cluster_size = n()) %>%
               mutate(remove = (cluster_size <= few.entries) &
                        # count words of at least three letters in a row
                        nrow(filter(data1[ (data1$left > min(left) - charheight &
                                              data1$left < max(right)) &
                                             (data1$top < max(top) & data1$bottom > min(bottom)),],
                                    nchar(sapply(text, str_extract, pattern = "[a-zA-z]+")) > few.entries)) > toomany.words
               ))$remove
  
  prices2 = filter(prices2, !remove1)
  
  # 2. too shallow slope magnitude
  
  remove2 = (prices2 %>% mutate(center = (left +  right)/2) %>% 
               mutate(tmp.just = .[[just]]) %>% group_by(cluster) %>%
               mutate(cluster_size = n(),  
                      remove = (cluster_size <= few.entries) &
                        # count words of at least three letters in a row
                        abs((max(top) - min(top))/(1 + max(tmp.just) - min(tmp.just))) < .5))$remove
  
  prices2 = filter(prices2, !remove2)
  
  # If no prices left to analyze, stop.
  if (nrow(prices2)==0) {return(NULL)}
  
  # renumber clusters ----
  prices2$cluster = as.numeric(as.factor(prices2$cluster))
  
  if (show.plot) {try({print(ggplot(prices2, aes(x = right, y = bottom, color = as.factor(cluster))) + 
                               geom_point() + #geom_point(aes(x = left, y = bottom, color = as.factor(cluster))) +
                               ylim(img.height, 0) + xlim(0, max(data1$right, na.rm =T)) + 
                               ggtitle("pageCols 5: Prices2 by cluster after outlier removal"))})}
  
  
  
  
  # reformat and detect outliers by left and right edge and minimum gap from median
  # need at least charheight absolute buffer, e.g. UCD_Lehmann_3392
  prices2 = data.frame(prices2 %>% group_by(cluster) %>% filter(!tableOutlier(.data, charheight)) %>%
                         ungroup() %>% arrange(left))
  
  if (show.plot) {try({print(ggplot(prices2, aes(x = right, y = bottom, color = as.factor(cluster))) + 
                               geom_point() + ylim(img.height, 0) + xlim(0, max(data1$right, na.rm =T)) + 
                               ggtitle("pageCols 6: Prices2 by cluster after vSplit and outlier removal"))})}
  
  # relevel cluster if necessary
  prices2$cluster = as.numeric(as.factor(prices2$cluster))
  max.clust = which.max(table(prices2$cluster)) #largest cluster
  
  colData2 = prices2 %>% group_by(cluster) %>%
    summarize(col.left = min(left), col.right = max(right),
              col.bottom = min(bottom), col.top = max(top),
              entries = n())
  
  #Try adding column headers:
  header = findHeader(colData2, data1, column.header, buffer = charheight)
  colData2[, c("col.header","col.header.bottom","col.header.top")] = header[,c(3,2,1)]
  colData2 = colData2  %>% arrange(cluster)
  
  #2e. id column info ####
  
  # numbers
  if (sum(data1$type=="ID") <= max(2, round(max.clust/3))) {
    tmp.numbers = filter(data1, type == "number", grepl(text, pattern="^[0-9]*.{0,1}$")) %>% arrange(left)
    tmp.numbers = tmp.numbers[,-which(names(tmp.numbers)=="center")]
    idtype = "number"
  } else {
    tmp.numbers = filter(data1, type == "ID") %>% arrange(left)
    tmp.numbers = tmp.numbers[,-which(names(tmp.numbers)=="center")]
    idtype = "ID"
  }
  
  # remove IDS in price columns
  if (nrow(tmp.numbers) > 0) {
    for (i in 1:nrow(prices2)) {
      #use individual instead of column in case column very wide
      tmp.numbers = filter(tmp.numbers, ! 
                             (( left > prices2[i,"left"] - 2*charheight & left < prices2[i,"right"]) & 
                                abs(top - prices2[i,"top"]) < 10*charheight ))
      tmp.numbers = filter(tmp.numbers, left < max(data1$left)*.9) #cant have IDs too far right on page
    }
  }
  
  # if any left, 
  if (nrow(tmp.numbers) > 0) {
    
    tmp.numbers.cum = sapply(1:max(tmp.numbers$left), function(x) {sum(x > tmp.numbers$left)})
    tmp.cpt = cpt.mean(tmp.numbers.cum, method='BinSeg', Q = nrow(colData2))@cpts
    if ( length(  which(diff(tmp.cpt) < charheight/3)  ) > 0 ) {
      tmp.cpt = tmp.cpt[-which(diff(tmp.cpt) < charheight/3)] #filter duplicates
    }
    
    #are there at least _ numbers near each cpt?
    id_cols = tmp.cpt[(rowSums(abs(outer(tmp.cpt, tmp.numbers$left,  "-")) < charheight)) >
                        max(2, min(table(prices2$cluster))/2)]
    
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
      if (show.plot) {try({print(ggplot(ids, aes(x = left, y = bottom, color = as.factor(table))) + 
                                   geom_point() + ylim(img.height, 0) + xlim(0, max(data1$right, na.rm =T)) + 
                                   ggtitle("pageCols 7: IDs by table")) })}
    } else {id_cols = NULL; ids = NULL}
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

#    pageCols.initialize - helper to pageCols ----
pageCols.initialize <- function(data1, img = NULL, img.height = NULL, show.initial.plot = FALSE) {
  
  data1$price = isPrice(data1$text)
  data1$type = isPrice(data1$text, maybe = TRUE)
  print(filter(data1, type != "FALSE") %>% 
          ggplot(aes(x = left, y = bottom, color = type)) + geom_point() + ylim(img.height,0) +
          ggtitle("pageCols.inititalize 1: Numeric data by type") )
  data1$center = (data1$left + data1$right)/2
  charheight = median((filter(data1, price | type == "*price") %>% mutate(diff = top - bottom))$diff)
  data1 = filter(data1, !(left <  2*charheight)) #margin: don't catch prices from previous page
  # Might move this to an external argument later, but since we don't know which pages have legit dollar prices it's here, dynamic for now
  if ( sum(isPrice(data1$text, dollar = T)) / (1 + sum(isPrice(data1$text, dollar = F))) > 3) {dollar = TRUE} else {dollar = FALSE}
  
  prices =  data1[isPrice(extractPrice(data1$text), dollar = dollar),] 
  prices = filter(prices, type != "FALSE") #if not dollar sign prices, dollar sign prices get removed here
  prices = filter(prices, left > max(data1$left)*.1) #cant have IDs too far right on page
  if (show.initial.plot) {try({print(ggplot(prices, aes(x = left, y = bottom)) + geom_point() + 
                                       ylim(img.height, 0) + ggtitle("pageCols.inititalize 2: Prices only"))})}
  
  return(list(data1 = data1, prices = prices, charheight = charheight, dollar = dollar))
}

#    pageCols.search.column - helper to pageCols ----
pageCols.search.column <- function(prices, data1, img.height, just, charheight, show.search.plot = FALSE) {
  
  data1$text.new = "FALSE"
  
  cat("Look along detected price columns\n") 
  for (i in unique(prices$cluster)) {
    
    prices.lm.tmp = subset(prices, cluster == i & price == TRUE) # only use actual prices, others likely to add noise
    
    #if at least two entries, consider non-vertical slope update
    if (nrow(prices.lm.tmp) >= 2) {
      
      # Is the column perfectly vertical? If so look vertically, otherwise find best slope
      
      verticality.check = var(prices.lm.tmp[,just])
      
      if (verticality.check == 0) {
        xintercept1 = mean(prices.lm.tmp[,just])
        if (show.search.plot) {
          try({print(ggplot(data=prices) + 
                       geom_point(aes(x = left, y = bottom), color = "black", size = .5) + 
                       geom_point(aes(x = right, y = bottom, color = type)) + 
                       ylim(img.height, 0) +
                       geom_vline(xintercept = xintercept1) +
                       ggtitle("pageCols.search.column: Prices by cluster")
          )})
        }
        # identify nearby words by vertical search
        near = abs(data1[[just]] - median(prices.lm.tmp[[just]])) < 2*charheight 
      } else {
        
        # try all column alignments
        # fit an lm for each
        lm.coefficients = list(
          lm.left = try({MASS::rlm(img.height - top ~ left, data = prices.lm.tmp, method = "M")$coefficients}),
          lm.right = try({MASS::rlm(img.height - top ~ right, data = prices.lm.tmp, method = "M")$coefficients}),
          lm.center = try({MASS::rlm(img.height - top ~ center, data = prices.lm.tmp, method = "M")$coefficients})
        )
        lm.coefficients = sapply(lm.coefficients, function(x) {
          if (class(x)=="try-error") {
            return(c(0,0)) 
          } else {return(x)}
        })
        
        # pick the steepest one, regardless of previously estimated justification
        tmp.just = c("left","right","center")[which.max(abs(lm.coefficients[2,]))] #find justification best for this purpose
        slope1 = lm.coefficients[2, which.max(abs(lm.coefficients[2,]))]
        yintercept1 = lm.coefficients[1, which.max(abs(lm.coefficients[2,]))]
        
        # identify nearby words
        near = abs((data1$top - data1[[tmp.just]]*slope1 - yintercept1)/slope1) < 2*charheight
        
        if (show.search.plot) {
          try({print(ggplot(data=prices) +
                       geom_point(aes(x = left, y = bottom), color = "black", size = .5) +
                       geom_point(aes(x = right, y = bottom, color = type)) +
                       geom_abline(slope = -slope1, intercept = -yintercept1) +
                       ylim(img.height, 0) +
                       ggtitle("pageCols.search.column: Prices by cluster"))
          })
        }
      }
      
      #look for more prices -- exlude already detected ones
      look2 = (near == TRUE & data1$price == FALSE)
      if (sum (look2 & !data1$type %in% c("FALSE","TRUE")) > 0) {
        # look for new prices among things not definitely price or not price
        data1$text.new[look2 & !data1$type %in% c("FALSE","TRUE")] =  
          sapply(data1[look2 & !data1$type %in% c("FALSE","TRUE"),"text"], numToPrice)
        cat("found", sum(data1$text.new[look2 & !data1$type %in% c("FALSE","TRUE")]!="FALSE"), "new prices\n") 
        data1$price[data1$text.new!="FALSE"] = TRUE
      }
    }
  }
  return(data1)
}

