# Function to extract name boxes from a price table.
# Method for with and without IDs
# Used by price_table_extraction function
# Jane Carlen, April 2019

# TO DO:
# Add an option for text justification in row (bottom, top, centered)
# Make sure number of boxes matches number of prices, even if nothing in them

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
    
    tmp.cols = dplyr::filter(page.cols$price_cols, table ==i)
    table.prices = do.call("rbind", prices) %>% dplyr::filter(table == i)
    if (!is.null(page.cols$ids)) {
      tmp.ids = dplyr::filter(page.cols$ids, table == i) %>% 
        #if multiple ids assigned to a row only use the lowest one
        group_by(row) %>% arrange(bottom) %>% summarise_all(first) %>% select(names(page.cols$ids))
    }
    # Select one price for each row, preferring actual prices and left-most
    tmp.prices = do.call("rbind", prices[as.character(tmp.cols$cluster)]) %>%
      arrange(row, !(price), -top) %>% group_by(row) %>% 
      summarize_all(first) %>%
      select(names(page.cols$prices[[1]])) # preserve original order
    
    
    # ignore tables with no structure, too much column variance
    structure.bad = min(apply( 
      table.prices %>% group_by(cluster) %>% 
        summarize(diffs_left = min(diff(sort(left))), diffs_right = min(diff(sort(right)))) %>%
        ungroup() %>% dplyr::select(contains("diffs")), 1, min) > page.cols$charheight * 4
    )
    
    if (structure.bad) 
      # nullify
      {
      table.boxes[[i]] = table.boxes.words[[i]] = table.boxes.words.reocr[[i]] = NULL
      } 
      # else, try to use ids:
      else if (exists("tmp.ids", inherits = FALSE) && nrow(tmp.ids) > 0) {
      
      #1. Initial try at name box coordinates
        
      # For missing ids, use the top and bottom the price in the row that's missing the ID
      # Start by assuming the name is two lines tall
      rows.missing.ids = unique(tmp.prices$row [! tmp.prices$row %in% tmp.ids$row ])
      rows.add.to.ids = tmp.prices %>%
        mutate(bottom = pmax(top - 2.5*page.cols$charheight, lag(top, default = min(top) - 2.5*page.cols$charheight))) %>%
        dplyr::filter(row %in% rows.missing.ids) %>% 
        arrange(row, !price, top) %>% 
        group_by(row) %>%summarize_all(first) %>% 
        ungroup() %>%
        # make left and right in line with the other ids (top and bottom will be adjusted later -currently price-height)
        mutate(left = quantile(tmp.ids$left, .2), right = quantile(tmp.ids$right, .8))
      tmp.ids = rbind(tmp.ids, rows.add.to.ids %>%select(names(tmp.ids))) %>% arrange(row, !price, top)
      
      l = pmax(0, tmp.ids$left - buffer) # in case buffer drags off page
      r = rep(min(dplyr::filter(page.cols$price_cols, table == i)$col.left), length(l)) + buffer
      b = tmp.ids$bottom - buffer
      t = tmp.ids$top + buffer
      # try getting a better top if the rows in ids and prices match -- then take further down of both
      # shouldn't need this after fixing row naming bug for ids, e.g. 0011
      if ( sum(duplicated(tmp.ids$row)) == 0 ) {
        t = (rbind(tmp.ids, tmp.prices %>% select(names(tmp.ids))) %>% group_by(row) %>% summarize(t = max(top)))$t + buffer
      }
      #2. Initial try at name box words -- used to get character types

      table.boxes.words[[i]] =  lapply(1:length(l), function(x) {
        dplyr::filter(data1, #buffers added earlier; also no buffer to allow things glued to prices, e.g. 0551.jpg
                      left >= l[x] & left <= r[x], 
                      right < ( min(dplyr::filter(page.cols$price_cols, table == i)$col.right)), 
                      bottom >= b[x], top <= t[x],
                      !isPrice(text, maybe = FALSE, dollar = FALSE))
      })
      
      # only use somewhat high-conf examples with at least 3 chars (minimize diacritics) for finding char types:
      table.words = do.call("rbind", table.boxes.words[[i]]) %>% dplyr::filter(nchar(text) > 2)
      char.types = charTypes(table.words, types = 2, conf.min = min(90, quantile(table.words$confidence, .5)))
      char.sizes = char.types$means
      
      # Adjust name boxes for prices that didn't have IDs
      rows.missing.names = which(sapply(table.boxes.words[[i]], nrow)==0)
      rows.missing.something = unique(c(rows.missing.names, rows.missing.ids))
      if (length(rows.missing.something) > 0) {
        
        # in this case tmp.boxes has a minimum for left
        tmp.boxes = dplyr::filter(data1,
                           left > max(min(l) - 2*buffer, 2*page.cols$charheight), #id left with buffer or small page left margin
                           left <= r - 2*buffer, top < max(t) + buffer*8, bottom > min(b) - buffer*8)
        tmp.boxes$char.sizes = char.sizes[apply(abs(outer(charWidth(tmp.boxes), char.sizes, "-")), 1,  which.min)]
        
        # detect nameBox 
        for (j in rows.missing.something) {
          table.boxes.words[[i]][[j]] = nameBox(j, tmp.boxes, l, r, b, t, buffer = buffer, char.sizes, 
                                                look.left = FALSE, look.above = TRUE, min.words = 3) #note 3 words cutoff for looking
        }
      }

      t = sapply(table.boxes.words[[i]], function(x) {max(x$top)})
      b = sapply(table.boxes.words[[i]], function(x) {min(x$bottom)})
      t[t<b] = b[t<b] + 2*page.cols$charheight # something went wrong (top below bottom) use default height
      table.boxes[[i]] = data.frame(l, b, r, t) #update r
      
      rownames(table.boxes[[i]]) = tmp.ids$row #for later sorting
      
      # see if name boxes (top dimension) overlaps with next row -- if so update for re-ocring
      # overlap_below = lapply(t, function(tt) {
      #   dplyr::filter(data1, 
      #          bottom < tt, top > (tt + buffer), top > (bottom + buffer), 
      #          left > min(l), right < median(r),
      #          nchar(text) > 2,
      #          confidence > 0)
      # })
      # update_t = which(sapply(overlap_below, nrow) > 2)
      # 
      # if (length(update_t) > 0) {
      #   t_new = sapply(update_t, function(x) min( (overlap_below[[x]]$bottom)[ overlap_below[[x]]$bottom > b[x] ]))
      #   t[update_t] = t_new
      #   table.boxes.words[[i]] = lapply(1:length(t), function(j) {dplyr::filter(table.boxes.words[[i]][[j]], top <= t[j])})
      #   table.boxes[[i]] = data.frame(l, b, r, t)
      # }
      # 
      
      #reocr
      table.boxes.words.reocr[[i]] = lapply(1:nrow(table.boxes[[i]]), function(x) {
        tpx = tesseract(px, pageSegMode = psm)
        SetRectangle(tpx, l[x] - buffer/2, b[x] - buffer/2, r[x]-l[x] + buffer , t[x]-b[x] + buffer) 
        if (GetSourceYResolution(tpx)==0) {SetSourceResolution(tpx, 600)}
        gb = GetBoxes(tpx, level = text.level)
        if(text.level  == "textline") {gb$text = gsub("\n", "", gb$text)}
        gb
      })
    } 
      # else, can't use ids:
      else { 
      
      #get table left, right, row bottoms and row tops without using ids
      b = (table.prices %>% group_by(row) %>% summarize(b = median(bottom)))[["b"]] - buffer
      t = (table.prices %>% group_by(row) %>% summarize(t = median(top)))[["t"]] + buffer
      l = rep(tmp.cols$table.left.cpt[1], length(b))
      r = rep(min(tmp.cols$col.left) + 2*buffer, length(b))
      
      # in this case tmp.boxes has no minimum for left (other than left margin) in case we need to expand left edge later
      tmp.boxes = dplyr::filter(data1,
                         left > 2*page.cols$charheight, # small left margin
                         left <= r[1] - 2*buffer, top < max(t) + buffer*8, bottom > min(b) - buffer*8)
      
      #remove absurdly large text
      charwidth = charWidth(tmp.boxes)
      tmp.boxes = dplyr::filter(tmp.boxes, abs(scale(charwidth)) < 10)
      
      #only uses somewhat high-conf examples for finding char types:
      char.types = charTypes(tmp.boxes, types = 2, conf.min = min(90, quantile(tmp.boxes$confidence, .5))) 
      char.sizes = char.types$means
      
      tmp.boxes$char.sizes = char.types$means[char.types$membership]
      if (buffer == "dynamic") {buffer= min(char.sizes)/2}
      
      table.boxes.words[[i]] =  lapply(1:length(l), function(x) {
        dplyr::filter(data1, #buffers added earlier; also no buffer to allow things glued to prices, e.g. 0551.jpg
                      left >= l[x] & left <= r[x], 
                      right < ( min(dplyr::filter(page.cols$price_cols, table == i)$col.right)), 
                      bottom >= b[x], top <= t[x],
                      !isPrice(text, maybe = FALSE, dollar = FALSE))
      })
      
      # update l (FIRST go through J loop)
      
      for (j in 1:length(l)) {
        if ( sum( nchar( table.boxes.words[[i]][[j]]$text  ) > 2 ) < min.words ) {
             table.boxes.words[[i]][[j]]  = nameBox(j, tmp.boxes, l, r, b, t, buffer, char.sizes, 
                                               look.left =  TRUE, look.above = FALSE, min.words = 2, max.words = 12, charsize.cutoff)
             l[j] = min(table.boxes.words[[i]][[j]]$left)
        }
      }
      
      #If it didn't find a good name, revert to median left edge
      table.boxes.words[[i]] = lapply(table.boxes.words[[i]], function(x) {
        if (nrow(x) == 1) {
          x$left = median(l)
          x$right = min(tmp.cols$col.left) + 2*buffer #original right
          x
        } else if (sum( nchar(x$text) > 2 ) < min.words ) {
          x = dplyr::filter(x, left >= median(l) - buffer/2)
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
      tmp.boxes = dplyr::filter(tmp.boxes, left > median(l) - 2*buffer) 
      
      for (j in 1:length(l)) {
        
        #look left and above
        table.boxes.words[[i]][[j]]  = nameBox(j, tmp.boxes, l, r, b, t, buffer, char.sizes, 
                                                 look.left =  TRUE, look.above = TRUE, min.words = 3, max.words = 12, charsize.cutoff)
        
        # update l if slightly left of  median, otherwise use median -- better for re-ocr
        l[j] = ifelse(median(l) - min(table.boxes.words[[i]][[j]]$left) > 4*buffer | 
                        median(l) < min(table.boxes.words[[i]][[j]]$left),
                      median(l),
                      min(table.boxes.words[[i]][[j]]$left))

      }
      
        
      #If it didn't find a good name, revert to median left edge
      table.boxes.words[[i]] = lapply(table.boxes.words[[i]], function(x) {
        if (nrow(x) == 1) {
          x$left = median(l)
          x$right = min(tmp.cols$col.left) + 2*buffer #original right
          x
        } else if (sum( nchar(x$text) > 2 ) < min.words ) {
          x = dplyr::filter(x, left >= median(l) - buffer/2)
          x
        } else {x}
      })
      
      # order left to right
      table.boxes.words[[i]] = lapply(table.boxes.words[[i]], arrange, left)
      
      # update t and b - not l and r because don't want to overly restrict off re-ocr
      #l = sapply(table.boxes.words[[i]], function(x) {min(x$left)})
      #r = sapply(table.boxes.words[[i]], function(x) {max(x$right)})
      t = pmax(t, sapply(table.boxes.words[[i]], function(x) {max(x$top)}))
      b = pmin(b, sapply(table.boxes.words[[i]], function(x) {min(x$bottom)}))
      
      # see if top overlaps with next row -- if so update for re-ocring
      # implemented differently than it was when IDS are present. Which is better?
      # find words that the current t value strikes through
      overlap_below = lapply(t, function(x) {
        words = dplyr::filter(data1,
                       bottom < x-2*buffer, x < top, left > min(l),
                       right < median(r), nchar(text) > 2,
                       grepl(text, pattern = "[a-zA-Z]+"))
        if (nrow(words) > 2) {
          c1 = abs(charWidth(words) - min(char.sizes)) < abs(charWidth(words) - max(char.sizes)) #not the big text size
          c2 = charWidth(words) < 2*max(char.sizes) #remove weirdly large text
          words = dplyr::filter(words, c1, c2)
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
      rownames(table.boxes[[i]]) = tmp.prices$row #for later sorting
      
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


# find a name box without an ID to guide whether the upper & left edge are
# tmp.boxes is a generous subset of the overall page data where we'll look for name words
# look.left if we don't have ids and we think we should look further left for more words if necessary
# look.above if we do have ids and we think we should look above for more words if necessary

nameBox <- function(j, tmp.boxes, l, r, b, t, buffer, char.sizes, look.left = TRUE, look.above = TRUE, 
                    min.words = 2, max.words = 12, charsize.cutoff = .4) {
    
    tmp.name = dplyr::filter( tmp.boxes, 
                       left > l[j] - 2*buffer, left < r[j],
                       top <= t[j],
                       bottom >= b[j])
    
    # bottom is max of bottom and top of previous name
    if (j > 1 & nrow(tmp.name) > 0) {tmp.name = dplyr::filter(tmp.name, bottom >= max( b[j] - 2*max(char.sizes), t[j-1] - buffer ))}
    
    # remove prices from tmp.name
    tmp.name = dplyr::filter( tmp.name, !isPrice(text))
    
    # remove diacritics of lower-case letters (misread .....) so that we'll look wider if needed                
    tmp.name = tmp.name %>% arrange(-left) %>% 
      dplyr::filter(! 
        (grepl(text, pattern = "^[a-z]+.*") & cumsum(!grepl(text, pattern = "^[a-z]+.*")) == 0) 
        )

    # if not enough words (with at least one alpha-numeric, of at least 3 char) found:
    if ( sum(grepl(tmp.name$text, pattern = "[A-Za-z0-9]")) < min.words || 
         ( sum(nchar(tmp.name$text) > 2) < min.words ) ) {
    
      if (look.left) {
        
      #look further left and on same line
      
        tmp.name = dplyr::filter(tmp.boxes, right < r[j] & (top <= t[j] & bottom >= b[j] - .5*max(char.sizes)) ) %>%
          arrange(-left) %>% mutate(char.sizes = 1) # placeholder char.size column
        if (nrow(tmp.name)== 0) {
          tmp.name = data.frame(left = 1, bottom = b[j], right = r[j], top = t[j], text = " ",
                                confidence = 0, char.sizes = 0) 
        }
      }
      
    }
    
    if (look.above) { 
        # look at the line above
        new.bottom = ifelse(j > 1, max(b[j] - max(char.sizes) - buffer, t[j-1]),b[j] - max(char.sizes) - buffer )
        tmp.add = dplyr::filter(tmp.boxes, right < r[j], top <= t[j], bottom >= new.bottom, bottom < b[j]) 
        if (nrow(tmp.add) >= 2) {
          if (mean(tmp.add$char.sizes == max(char.sizes)) >= charsize.cutoff) {
            if (look.left & tmp.name$char.sizes[1] !=0) { #if tmp.name was already updated in look.left
              tmp.name = rbind(tmp.name, tmp.add)
            } else {
              tmp.name = dplyr::filter(tmp.boxes, right < r[j], top <= t[j], bottom >= new.bottom) %>%
              arrange(-left) 
            }
          }
        }
        if (nrow(tmp.name)== 0) {
          tmp.name = data.frame(left = 1, bottom = b[j], right = r[j], top = t[j], text = " ",
                                confidence = 0, char.sizes = 0) 
        }
    }  
    
    # if the name now has some data (either originally or after expanded search), 
    if (tmp.name$char.sizes[1] !=0) {
      
      # take to the right of detected prices (not the line's price) on first line: 
      tmp.name2 = tmp.name %>% arrange(top) %>% 
        mutate(line = ceiling(-apply(outer(top, top, "-"), 1, min)/max(char.sizes) + .001)) %>%
        mutate(isPrice = !isPrice(text, maybe = T) %in% c("FALSE", "number", "ID", "year"))
      
      if ( sum(tmp.name2$isPrice == TRUE & tmp.name2$line == 1) > 0) {
        tmp.name2 = tmp.name2 %>% mutate(keep = left > max(left[isPrice == TRUE & line == 1])) %>% dplyr::filter(keep)
        if (nrow(tmp.name2[nchar(tmp.name2$text) > 2, ]) >= min.words) {
          tmp.name = tmp.name2 %>% select(-c("line", "isPrice", "keep"))
        }
      }
      
      # limit to max.words
      if ( sum(nchar(tmp.name$text) > 2) > max.words) {tmp.name = (tmp.name %>% arrange(-nchar(text), -top))[1:max.words,]}  
    }

    return(tmp.name %>% select(-contains("char.sizes")))
}