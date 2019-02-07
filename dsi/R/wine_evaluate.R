# File with functions to compare wine_price_table output to truth and summarize results
#
# Jane Carlen
# Created 12-8-2018
#
# Notes:
#  See run_wine_evaluate in the adjacent scripts directory for use of these functions.
#  Test results created using price_table_extracton in wine_price_tables.R
#  Output saved in "~/Documents/DSI/wine-price-extraction/dsi/Data/"
#  Truth files are created by corrected price_table_extraction output (see below)
#  Truth saved in "wine-price-extraction/dsi/Data/price_id_truth/" as of 1/10/19
#  Ok to pay less attention to hard pages of non-wines, at least for now.
# 
# TO DO: 
#  Ratios for tables with two columns (presumably bottle and case)
#
##################################################################################################################
# 0. Setup
##################################################################################################################

library(reshape2)
library(ggplot2)
library(stringr)
library(dplyr)
library(RecordLinkage)

##################################################################################################################
# 1. Evaluation and comparison functions
##################################################################################################################

wine.evaluate <- function(test.prices) {
  list(n.tables = length(test.prices),
       n.columns.per.table = sapply(test.prices, function(x) {length(x$prices)}),
       n.entries.per.column = lapply(test.prices, function(x) {sapply(x$prices, nrow)}),
       n.columns.total = sum(sapply(test.prices, function(x) {length(x$prices)})),
       n.entries.total = sum(unlist(lapply(test.prices, function(x) {sapply(x$prices, nrow)}))),
       table.ratios.summary = lapply(test.prices, function(x) {#x is a price table
         prices = x$prices
         if (length(prices) < 2) {return(NULL)}
         joined.prices = Reduce(function(x, y) merge(x, y, by="row"), prices)
         prices.ratios = matrix(apply(joined.prices, 1, function(x) {as.numeric(x[-c(1,2)])/as.numeric(x[2])}), nrow = length(prices)-1) #row for each comparison
         rownames(prices.ratios) = paste(names(prices)[-1], names(prices[1]), sep = "/")
         apply(prices.ratios, 1, summary)
       }))
}

# Compare output to truth for a single image with output to list, e.g. wine.compare(test = test.prices, truth = truth.prices)
# All differences are test minus truth
wine.compare <- function(test.prices, truth.prices) {
  
  # Output of price_table_extraction
  test.stat = wine.evaluate(test.prices)
  
  # Truth
  truth.stat = list(n.tables = length(truth.prices),
                    n.columns.per.table = sapply(truth.prices, function(x) {length(x$prices)}),
                    n.entries.per.column = lapply(truth.prices, function(x) {sapply(x$prices, nrow)}),
                    n.columns.total = sum(sapply(truth.prices, function(x) {length(x$prices)})),
                    n.entries.total = sum(unlist(lapply(truth.prices, function(x) {sapply(x$prices, nrow)}))))
  
  # Initialize output list
  compare.list = list(test.stat = test.stat,
                      truth.stat = truth.stat,
                      diff.in.tables = truth.stat$n.tables - test.stat$n.tables,
                      diff.in.columns.total = truth.stat$n.columns.total - truth.stat$n.columns.total,
                      diff.in.entries.total = truth.stat$n.entries.total - test.stat$n.entries.total,
                      # If same number of tables
                      diff.in.columns.bytable = NULL,
                      diff.in.entries.bytable = NULL,
                      # If same number of columns in table
                      diff.in.entries = NULL)
                  
  # Same number of tables?
  if (test.stat$n.tables != truth.stat$n.tables) {
    cat("Truth and results have different numbers of tables.\n")
    cat("Truth has", truth.stat$n.tables,"\n")
    cat("Result has", test.stat$n.tables,"\n")
    return(compare.list)
  } else {
    
    # Same number of columns in tables?
    diff.in.columns.bytable = truth.stat$n.columns.per.table - test.stat$n.columns.per.table
    compare.list[["diff.in.columns.bytable"]] = diff.in.columns.bytable
    
    compare.list[["diff.in.entries.bytable"]] =  unlist(lapply(truth.stat$n.entries.per.column, sum, na.rm = T)) -
      unlist(lapply(truth.stat$n.entries.per.column, sum, na.rm = T))
    
    compare.list[["diff.in.entries"]] = lapply(1:length(diff.in.columns.bytable), function(x) {
      
      # Same number of columns in tables:
      if (diff.in.columns.bytable[x] == 0) {
        
        compare.list[["diff.in.entries"]][[x]] = truth.stat$n.entries.per.column[[x]] - test.stat$n.entries.per.column[[x]]
        
        # Compare entries in columns:
        diff.in.entries = lapply(1:truth.stat$n.columns.per.table[x], function(y) {
            
          inner_join(test.prices[[x]]$prices[[y]], truth.prices[[x]]$prices[[y]], by = "row", suffix = c(".test",".truth")) %>% 
                     
              #diffs in dollar amounts
              mutate(dollar.diff = as.numeric(text.new.truth)-as.numeric(text.new.test),
                          
              #levenshtein distances       
              lev.diff = levenshteinDist(text.new.truth, text.new.test),
              
              missing.digit = nchar(text.new.truth) > nchar( text.new.test ),
              
              extra.digit = nchar(text.new.truth) < nchar( text.new.test ),
              
              index = paste(x,y,sep="."))
            
        })
        
        
      } else {
        #Different number of columns in table
        compare.list[["diff.in.entries"]][[x]] = NULL
      }
    })
    
    compare.list[["all.diffs"]] =
      do.call("rbind", lapply(compare.list$diff.in.entries[!sapply(compare.list$diff.in.entries, is.null)], 
                               function(x) { do.call("rbind", x) }))
  }
  return(compare.list)
}

# Convert list output from wine.compare to data frame
wine.summarize = function(compare.list) {
  
  #condense all entrywise comprisons
  all.diffs = compare.list$all.diffs
  
  data.frame(detected.entries = compare.list$test.stat$n.entries.total,
             true.entries = compare.list$truth.stat$n.entries.total,
             diff.in.entries = compare.list$diff.in.entries.total,
             
             detected.tables = compare.list$test.stat$n.tables,
             true.tables = compare.list$truth.stat$n.tables,
             diff.in.tables = compare.list$diff.in.tables,
             # only do if same number of tables
             tables.with.col.mismatch = ifelse(compare.list$test.stat$n.tables == compare.list$truth.stat$n.tables,
                                        sum(compare.list$truth.stat$n.columns.per.table != 
                                              compare.list$test.stat$n.columns.per.table, na.rm = T), NA),
             #percent.detected = round(compare.list$test.stat$n.entries.total/compare.list$truth.stat$n.entries.total, 2),
             percent.correct.entries = ifelse(!is.null(all.diffs), 
                                              round(sum(all.diffs$dollar.diff==0, na.rm = T)/compare.list$truth.stat$n.entries.total,2),
                                              NA),
             percent.lev.diff.1 = ifelse(!is.null(all.diffs), 
                                         round(sum(all.diffs$lev.diff==1, na.rm = T)/compare.list$truth.stat$n.entries.total,2),
                                         NA),
             percent.detected.missing.digit = ifelse(!is.null(all.diffs), 
                                                     round(mean(all.diffs$missing.digit),2),
                                                     NA),
             percent.detected.extra.digit = ifelse(!is.null(all.diffs), 
                                                   round(mean(all.diffs$extra.digit),2),
                                                   NA),
             percent.lev.diff.2 = ifelse(!is.null(all.diffs), 
                                         round(sum(all.diffs$lev.diff==2, na.rm = T)/compare.list$truth.stat$n.entries.total,2),
                                         NA),
             #percent.lev.diff.3plus
             mean.dollar.diff = ifelse(!is.null(all.diffs), 
                                       round(mean(all.diffs$dollar.diff, na.rm = T),4),
                                       NA),
             median.dollar.diff = ifelse(!is.null(all.diffs), 
                                         median(all.diffs$dollar.diff, na.rm = T),
                                         NA),
             mean.lev.diff = ifelse(!is.null(all.diffs), 
                                    round(mean(all.diffs$lev.diff, na.rm = T),4),
                                    NA),
             median.lev.diff = ifelse(!is.null(all.diffs), 
                                      median(all.diffs$lev.diff, na.rm = T),
                                      NA),
             entries.compared = ifelse(!is.null(all.diffs), 
                                       nrow(all.diffs),
                                       NA))
          
}

