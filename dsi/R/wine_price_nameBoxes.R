# Function to extract name boxes from a price table.
# Method for with and without IDs
# Used by price_table_extraction function
# Jane Carlen, April 2019

source("wine-price-extraction/dsi/R/helper.R")

nameBoxes <- function(data1, page.cols, prices = page.cols$prices, px , buffer = page.cols$charheight/2, charsize.cutoff = .4, psm = 3, text.level = "textline", min.words = 2, max.words = 12) { 
  # buffer on order of 1/2 small char size, 1/4 large; 
  # charsize.cutoff determines by percentage same type size whether a line-above should be included in a name box
  # psm(3) important for last stage with reocr
  # min.words = min words of at least three letters in name
  # max.words = max words in name
  
  # go table by table
  
  # initialize output
  table.boxes = vector(mode = "list", length = n_distinct(page.cols$price_cols$table))
  table.boxes.words = vector(mode = "list", length = n_distinct(page.cols$price_cols$table))
  table.boxes.words.reocr = vector(mode = "list", length = n_distinct(page.cols$price_cols$table))
  
  for (i in sort(unique(page.cols$price_cols$table))) {
    
    tmp.cols = filter(page.cols$price_cols, table ==i)
    table.prices = do.call("rbind", prices) %>% filter(table == i)
    if (!is.null(page.cols$ids)) {tmp.ids = filter(page.cols$ids, table == i)}
    
    # ignore tables with no structure, too much column variance
    structure.bad = min(apply( 
      table.prices %>% group_by(cluster) %>% 
        summarize(diffs_left = min(diff(sort(left))), diffs_right = min(diff(sort(right)))) %>%
        ungroup() %>% dplyr::select(contains("diffs")), 1, min) > page.cols$charheight * 4
    )
    
    if (structure.bad) {table.boxes[[i]] = table.boxes.words[[i]] = table.boxes.words.reocr[[i]] = NULL}
    
    # Use ids  
    else if (exists("tmp.ids", inherits = FALSE) && nrow(tmp.ids) > 0) {
      
      tmp.prices = prices[[as.character((tmp.cols %>% arrange(col.left))[1,"cluster"])]]
      #fill in missing rows
      if (nrow(tmp.cols) > 1) {
        # has to be for loop to continually update
        for (j in 2:nrow(tmp.cols)) {
          
          # get info for other column in table, from left to right
          tmp.prices2 = prices[[as.character((tmp.cols %>% arrange(col.left))[j,"cluster"])]]
          fill.in.rows = tmp.prices2$row[! tmp.prices2$row %in% tmp.prices$row]
          # see if it has info for the missing row, if so pick the price first, then top-most
          if ( length( fill.in.rows ) > 0 ) {
            
            tmp.prices2 = tmp.prices2 %>% arrange(row, !price, top) %>% group_by(row) %>% summarize_all(first) %>%
              filter(row %in% fill.in.rows) 
            tmp.prices = rbind(tmp.prices, tmp.prices2) %>% arrange(row)
            
          }
        }
      }
      l = pmax(0, tmp.ids$left - buffer) # in case buffer drags off page
      r = rep(min(filter(page.cols$price_cols, table == i)$col.left), length(l)) + buffer
      b = tmp.ids$bottom - buffer
      # we use the bottom (top) of the prices as the lower bound for the name box, 
      # but have to be careful if more prices than names
      t = tmp.prices$top[apply(outer(tmp.ids$top, tmp.prices$top + buffer, "-"), 1, function(x) {
        which.max( (x+max(data1$right)) * (x < 0)  )
      })] + buffer
      print(t[t - tmp.ids$top > 3 * page.cols$charheight]) 
      #t = tmp.ids$top #better match without buffer
      #t = tmp.ids$top[apply(abs(outer(tmp.ids$top, t, "-")), 1, which.min)] + buffer
      #t[tmp.ids$top > t] = tmp.ids$top[tmp.ids$top > t] #if id bottom (top) is lower use that one
      
      table.boxes[[i]] = data.frame(l, b, r, t)
      table.boxes.words[[i]] =  lapply(1:nrow(table.boxes[[i]]), function(x) {
        dplyr::filter(data1, left >= l[x] & left <= r[x], 
                      right < ( min(filter(page.cols$price_cols, table == i)$col.right)), #no buffer to allow things glued to prices, e.g. 0551.jpg
                      bottom >= b[x], top <= t[x] + buffer/2)
      })
      
      r.tmp =  sapply(table.boxes.words[[i]], function(x) max(x$right)) + buffer #update r
      r[is.finite(r.tmp)] = r.tmp[is.finite(r.tmp)]
      t[t<b] = b[t<b] + 2*page.cols$charheight # if stuck to row above, use default name height
      table.boxes[[i]] = data.frame(l, b, r, t) #update r
      
      # see if top overlaps with next row -- if so update for re-ocring
      overlap_below = lapply(t, function(tt) {
        filter(data1, 
               bottom < tt,
               top > (tt + buffer),
               top > (bottom + buffer), 
               left > min(l), 
               right < median(r),
               nchar(text) > 2)
      })
      update_t = which(sapply(overlap_below, nrow) > 2)
      
      cat("Found", length(update_t), "overlap(s) with row below when checking name boxes\n")
      if (length(update_t) > 0) {
        t_new = sapply(overlap_below[update_t], function(x) min(x$bottom))
        t[update_t] = t_new
        overlap_below = lapply(t, function(tt) {
          filter(data1, 
                 bottom < tt,
                 top > (tt + buffer),
                 top > (bottom + buffer), 
                 left > min(l), 
                 right < median(r) )
        })
        update_t = sapply(overlap_below, nrow) > 2
        cat("Now", sum(update_t), "overlaps with row below when checking name boxes\n")
        table.boxes[[i]] = data.frame(l, b, r, t) #update r
      }
      
      #reocr
      table.boxes.words.reocr[[i]] = lapply(1:nrow(table.boxes[[i]]), function(x) {
        tpx = tesseract(px, pageSegMode = psm)
        SetRectangle(tpx, l[x] - buffer/2, b[x] - buffer/2, r[x]-l[x] + buffer , t[x]-b[x] + buffer) 
        if (GetSourceYResolution(tpx)==0) {SetSourceResolution(tpx, 600)}
        gb = GetBoxes(tpx, level = text.level)
        if(text.level  == "textline") {gb$text = gsub("\n", "", gb$text)}
        gb
      })
    } else { 
      #Can't use ids
      
      #get table left, right, row bottoms and row tops without using ids
      l = tmp.cols$table.left.cpt[1]
      r = min(tmp.cols$col.left) + 2*buffer 
      b = (table.prices %>% group_by(row) %>% summarize(b = median(bottom)))[["b"]] - buffer
      t = (table.prices %>% group_by(row) %>% summarize(t = median(top)))[["t"]] + buffer
      
      # tmp.boxes has no minimum for left (other than left margin) in case we need to expand left edge later
      tmp.boxes = filter(data1,
                         left > 2*page.cols$charheight, # small left margin
                         left <= r - 2*buffer, top < max(t) + buffer*8, bottom > min(b) - buffer*8)
      
      #remove absurdly large text
      charwidth = charWidth(tmp.boxes)
      tmp.boxes = filter(tmp.boxes, abs(scale(charwidth)) < 10)
      
      #only uses somewhat high-conf examples for finding char types:
      char.types = charTypes(tmp.boxes, types = 2, conf.min = min(90, quantile(tmp.boxes$confidence, .5))) 
      char.sizes = char.types$means
      
      tmp.boxes$char.sizes = char.types$means[char.types$membership]
      if (buffer == "dynamic") {buffer= min(char.sizes)/2}
      
      #add *row* justification here later to choose bottom or top
      l = rep(l, length(b))
      r = rep(r, length(b))
      
      #if text above the price is in the bigger character type then include it the name
      table.boxes.words[[i]] = vector(mode = "list", length = length(t))
      tmp.price_cols = filter(page.cols$price_cols, table==i)
      
      # update l (FIRST go through J loop)
      for (j in 1:length(b)) {
        
        # sets how high to look, assumes at most two-row names
        tmp.name = filter( tmp.boxes, 
                           left > l[j] - 2*buffer, left < r[j],
                           top <= t[j],
                           bottom >= b[j] - 2*max(char.sizes))
        # bottom is max of preset bottom and bottom of previous name
        if (j > 1) {tmp.name = filter(tmp.name, bottom >= max( b[j] - 2*max(char.sizes), t[j-1] ))}
        
        # remove from tmp.name anything glued to a price or is a price
        tmp.name = filter( tmp.name, 
                           right < tmp.price_cols$table.right[1] - buffer,
                           !isPrice(text))
        
        # if not enough words (of at least 3 char) found, look further left and on same line
        if ( nrow(tmp.name)  < min.words || ( sum(nchar(tmp.name$text) > 2) < min.words ) ) {
          tmp.name = filter(tmp.boxes, right < r[j] & (top <= t[j] & bottom >= b[j] - .5*max(char.sizes)) ) %>%
            arrange(-left) %>% mutate(char.sizes = 1) # placehodler char.size column
          if (nrow(tmp.name)== 0) {
            tmp.name = data.frame(left = 1, bottom = b[j], right = r[j], top = t[j], text = " ",
                                  confidence = 0, char.sizes = 0) 
          }
          
        }
        
        # if the name now has some data (either originally or after expanded search), 
        if (tmp.name$char.sizes[1] !=0) {
          
          # take to the right of detected prices on first line: 
          tmp.name2 = tmp.name %>% arrange(top) %>% 
            mutate(line = ceiling(-apply(outer(top, top, "-"), 1, min)/page.cols$charheight + .001)) %>%
            mutate(isPrice = !isPrice(text, maybe = T) %in% c("FALSE", "number", "ID", "year"))
          
          if ( sum(tmp.name2$isPrice == TRUE & tmp.name2$line == 1) > 0) {
            tmp.name2 = tmp.name2 %>% mutate(keep = left > max(left[isPrice == TRUE & line == 1])) %>% filter(keep)
            if (nrow(tmp.name2[nchar(tmp.name2$text) > 2, ]) >= min.words) {
              tmp.name = tmp.name2 %>% select(-c("line", "isPrice", "keep"))
            }
          }
          
          # limit to max.words
          if (nrow(tmp.name) > max.words) {tmp.name = (tmp.name %>% arrange(-top))[1:max.words,]}  
          
          # update b to prevent overlap with above
          #if (mean(tmp.name$char.sizes == max(char.sizes)) >= charsize.cutoff) {
          #could add another condition here for text width
          if (j == 1) {b[j] = min(tmp.name$bottom)} 
          if (j > 1) {b[j] = max(min(tmp.name$bottom - min(char.sizes)), t[j-1] - buffer/2)}
          #}
          tmp.name = filter(tmp.name, bottom >= b[j])
          
          # remove diacritics posing as words (one or two lower case letters ) on the left)                    
          tmp.name = tmp.name %>% arrange(left) %>% 
            filter(! (
              nchar(text) <=2 & ( grepl(text, pattern = "[a-z]{1}.*") & cumsum(nchar(text) > 2) == 0) 
            ) )
          
          if (nrow(tmp.name) > 0) {
            # update l 
            l[j] = min(tmp.name$left)
            # update r
            r[j] = max(tmp.name$right)
          } else {
            tmp.name = data.frame(left = 1, bottom = b[j], right = r[j], top = t[j], text = " ",
                                  confidence = 0, char.sizes = 0) 
          }
          
          
        } #include another row in name, make sure below other price
        table.boxes.words[[i]][[j]] = tmp.name %>% select(-contains("char.sizes"))
      }
      
      
      #If it didn't find a good name, revert to median left edge
      table.boxes.words[[i]] = lapply(table.boxes.words[[i]], function(x) {
        if (nrow(x) == 1) {
          x$left = median(l)
          x$right = min(tmp.cols$col.left) + 2*buffer #original right
          x
        } else if (sum( nchar(x$text) > 2 ) < min.words ) {
          x = filter(x, left >= median(l) - buffer/2)
          x
        } else {x}
      })
      
      # get names (SECOND go through J loop)
      
      l = rep(median(l), length(l))
      r = rep(min(tmp.cols$col.left) + 2*buffer, length(r))
      b = (table.prices %>% group_by(row) %>% summarize(b = median(bottom)))[["b"]] - buffer
      t = (table.prices %>% group_by(row) %>% summarize(t = median(top)))[["t"]] + buffer
      
      # use left edge discovered in round 1: 
      # Condider using median of .25 percentile l instead? 
      tmp.boxes = filter(tmp.boxes, left > median(l) - 2*buffer) 
      
      for (j in 1:length(b)) {
        
        # sets how high to look, assumes at most two-row names
        tmp.name = filter( tmp.boxes, 
                           left > l[j] - 2*buffer, left < r[j],
                           top <= t[j],
                           bottom >= b[j] - 2*max(char.sizes))
        # bottom is max of preset bottom and bottom of previous name
        if (j > 1) {tmp.name = filter(tmp.name, bottom >= max( b[j] - 2*max(char.sizes), t[j-1] ))}
        
        # remove from tmp.name nything glued to a price
        tmp.name = filter( tmp.name, 
                           right < tmp.price_cols$table.right[1] - buffer)
        
        # if not enough words (of at least 3 char) found, look further left and on same line
        if ( nrow(tmp.name)  < min.words || ( sum(nchar(tmp.name$text) > 2) < min.words ) ) {
          tmp.name = filter(tmp.boxes, right < r[j] & (top <= t[j] & bottom >= b[j] - .5*max(char.sizes)) ) %>%
            arrange(-left) %>% mutate(char.sizes = 1) # placehodler char.size column
          if (nrow(tmp.name)== 0) {
            tmp.name = data.frame(left = 1, bottom = b[j], right = r[j], top = t[j], text = " ",
                                  confidence = 0, char.sizes = 0) 
          }
          
        }
        
        # if the name now has some data (either originally or after expanded search), 
        if (tmp.name$char.sizes[1] !=0) {
          
          # take to the right of detected prices on first line: 
          tmp.name2 = tmp.name %>% arrange(top) %>% 
            mutate(line = ceiling(-apply(outer(top, top, "-"), 1, min)/page.cols$charheight + .001)) %>%
            mutate(isPrice = !isPrice(text, maybe = T) %in% c("FALSE", "number", "ID", "year"))
          
          if ( sum(tmp.name2$isPrice == TRUE & tmp.name2$line == 1) > 0) {
            tmp.name2 = tmp.name2 %>% mutate(keep = left > max(left[isPrice == TRUE & line == 1])) %>% filter(keep)
            if (nrow(tmp.name2[nchar(tmp.name2$text) > 2, ]) >= min.words) {
              tmp.name = tmp.name2 %>% select(-c("line", "isPrice", "keep"))
            }
          }
          
          # limit to max.words
          if (nrow(tmp.name) > max.words) {tmp.name = (tmp.name %>% arrange(-top))[1:max.words,]}  
          
          # update b to prevent overlap with above
          #if (mean(tmp.name$char.sizes == max(char.sizes)) >= charsize.cutoff) {
          #could add another condition here for text width
          if (j == 1) {b[j] = min(tmp.name$bottom)} 
          if (j > 1) {b[j] = max(min(tmp.name$bottom - min(char.sizes)), t[j-1] - buffer/2)}
          #}
          tmp.name = filter(tmp.name, bottom >= b[j])
          
          # remove diacritics posing as words (one or two lower case letters ) on the left)                    
          tmp.name = tmp.name %>% arrange(left) %>% 
            filter(! (
              nchar(text) <=2 & ( grepl(text, pattern = "[a-z]{1}.*") & cumsum(nchar(text) > 2) == 0) 
            ) )
          
          if (nrow(tmp.name) > 0) {
            # update l 
            l[j] = min(tmp.name$left) - buffer/2
            # update r
            r[j] = max(tmp.name$right) + buffer/2
          } else {
            tmp.name = data.frame(left = 1, bottom = b[j], right = r[j], top = t[j], text = " ",
                                  confidence = 0, char.sizes = 0) 
          }
          
          
        } #include another row in name, make sure below other price
        table.boxes.words[[i]][[j]] = tmp.name %>% select(-contains("char.sizes"))
      }
      #If it didn't find a good name, revert to median left edge
      table.boxes.words[[i]] = lapply(table.boxes.words[[i]], function(x) {
        if (nrow(x) == 1) {
          x$left = median(l)
          x$right = min(tmp.cols$col.left) + 2*buffer #original right
          x
        } else if (sum( nchar(x$text) > 2 ) < min.words ) {
          x = filter(x, left >= median(l) - buffer/2)
          x
        } else {x}
      })
      l = sapply(table.boxes.words[[i]], function(x) {min(x$left)})
      r = sapply(table.boxes.words[[i]], function(x) {max(x$right)})
      
      # see if top overlaps with next row -- if so update for re-ocring
      # implemented differently than for IDS. Which is better?
      # find words that the current t value strikes through
      overlap_below = lapply(t, function(x) {
        words = filter(data1, bottom < x-2*buffer, x < top, left > min(l), right < median(r), nchar(text) > 2,
                       grepl(text, pattern = "[a-zA-Z]+"))
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
        if(length(update_t) > 0) {t[update_t] = t_new}
        cat("Now", length(update_t), "overlaps with row below when checking name boxes\n")
      }
      
      table.boxes[[i]] = data.frame(l = l, b = b, r = r, t = t)
      l = pmax(0, l) #in case buffer puts off page
      
      # reocr
      table.boxes.words.reocr[[i]] = lapply(seq_along(b), function(x) {
        tpx = tesseract(px, pageSegMode = psm)
        SetRectangle(tpx, l[x], b[x], r[x]-l[x], t[x]-b[x]) #buffers already implemented
        if (GetSourceYResolution(tpx)==0) {SetSourceResolution(tpx, 600)}
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
