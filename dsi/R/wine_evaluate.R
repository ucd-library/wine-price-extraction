# File to compare wine_price_table output to truth and summarize results
#
# Jane Carlen
# Created 12-8-2018
#
# Notes:
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

output.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data"
truth.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth"
file.number = "0069"

test.prices = readRDS(file.path(output.directory, paste0("UCD_Lehmann_", file.number,".RDS")))$prices
truth.prices = readRDS(file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))$prices

#if correction necessary to truth, something like:
#tmp = readRDS(file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))
#tmp$prices[[1]]$prices[[2]]$row = 1:8
#saveRDS(tmp, file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))

##################################################################################################################
# 1. Evaluation and comparison functions
##################################################################################################################

wine.evaluate <- function(test.prices) {
  list(n.tables = length(test.prices),
       n.columns.per.table = sapply(test.prices, function(x) {length(x$prices)}),
       n.entries.per.column = lapply(test.prices, function(x) {sapply(x$prices, nrow)}),
       n.columns.total = sum(sapply(test.prices, function(x) {length(x$prices)})),
       n.entries.total = sum(unlist(lapply(test.prices, function(x) {sapply(x$prices, nrow)}))))
  # each column is a table
}

# Compare output to truth for a single image (list output)
# All differences are test - truth
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
wine.compare(test = test.prices, truth = truth.prices)

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

##################################################################################################################
# 2. Run comparison on fileset
##################################################################################################################

# A. Basic stats for all files ####

fileset1 = str_extract(list.files("~/Documents/DSI/wine-price-extraction/dsi/Data/"), pattern = "UCD.*RDS")
fileset1 = fileset1[!is.na(fileset1)]
evaluate.output = vector("list", length(fileset))
for (i in 1:length(fileset1)) {
  file1 = file.path(output.directory,fileset1[i])
  rds1 = readRDS(file1)$prices
  eval1 = wine.evaluate(rds1)
  evaluate.output[[i]]
}

evaluate.singlestat = data.frame(t(sapply(evaluate.output, function(eval) {
  c("n.tables" = eval$n.tables, "n.columns.total" = eval$n.columns.total, "n.entries.total" = eval$n.entries.total)
  })))

output_singlestat = ggplot(melt(evaluate.singlestat)) + geom_histogram(aes(x = value, group = variable)) + facet_grid(~variable, scales = "free_x")
ggsave(output_singlestat, filename = "~/Documents/DSI/wine-price-extraction/dsi/Data/output_singlestat.png")

# B. For files we have truth for ####

fileset2 = str_extract(list.files("~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/"), "[0-9]{4}")
summary.output = vector("list", length = length(fileset2))
names(summary.output) = paste0("UCD_Lehmann_",fileset2,".jpg")
i=1
for(file.number in fileset2) {
  print(file.number)
  test.prices = readRDS(file.path(output.directory, paste0("UCD_Lehmann_", file.number,".RDS")))$prices
  truth.prices = readRDS(file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))$prices
  compare.list = wine.compare(test.prices, truth.prices)
  
  summary.output[[i]] = wine.summarize(compare.list)
  i = i+1
}
summary.output = do.call("rbind", summary.output)

write.csv(summary.output, "~/Documents/DSI/wine-price-extraction/dsi/Data/summary_vs_truth.csv")

##################################################################################################################
# 3. Code from modifying output to create truth (if necessary, would be run before comparison making)
##################################################################################################################


# saveRDS(UCD_Lehmann_0011_price_truth, "~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/UCD_Lehmann_0011_price_truth.RDS")

truth1 = "1544"
truthdata = readRDS(paste0("~/Documents/DSI/OCR_SherryLehmann/Truth2/",truth1,"_column2.RDS"))
try({truthdata = data.frame(truthdata)})
truthdata = data.frame(apply(truthdata, 2, as.character), stringsAsFactors = F)

#modify truth?
truthdata$Bottle = sub(pattern = "(\\.[0-9]{1}$)",  "\\10", x = truthdata$Bottle)
truthdata$Bottle = sub(pattern = "^0\\.",  "\\.", x = truthdata$Bottle)
truthdata$Bottle = sub(pattern = "(^[0-9]+$)",  "\\1.00", x = truthdata$Bottle)
truthdata$Case = sub(pattern = "(^[0-9]+$)",  "\\1.00", x = truthdata$Case)
truthdata$Case = sub(pattern = "(\\.[0-9]{1}$)",  "\\10", x = truthdata$Case)
truthdata$CaseArrival = sub(pattern = "(\\.[0-9]{1}$)",  "\\10", x = truthdata$CaseArrival)
truthdata$CaseArrival = sub(pattern = "(^[0-9]+$)",  "\\1.00", x = truthdata$CaseArrival)

truthdata

# insert

truthoutput = readRDS(paste0("~/Documents/DSI/wine-price-extraction/dsi/Data/UCD_Lehmann_",truth1,".RDS"))
# From truthfile
i = 2
truthoutput$prices[[i]]$ids[[1]] = data.frame(row = 1:length(truthdata$Description),
                                              name = as.character(truthdata$Description), stringsAsFactors = F)

truthoutput$prices[[i]]$prices[[4]] = data.frame(row = 1:length(truthdata$QuartCase),
                                                 text.new = as.character(truthdata$QuartCase),
                                                 stringsAsFactors = F)

truthoutput$prices[[i]]$prices[[2]] = data.frame(row = 1:length(truthdata$CaseArrival),
                                                 text.new = as.character(truthdata$CaseArrival),
                                                 stringsAsFactors = F)

truthoutput$prices

# Custom changes
#1,1
truthoutput$prices[[1]]$ids[[1]] = rbind(truthoutput$prices[[1]]$ids[[1]], c(6, "234")) %>% arrange(row)
truthoutput$prices[[1]]$prices[[1]] = rbind(truthoutput$prices[[1]]$prices[[1]], c(8, "2.19")) %>% arrange(row)
truthoutput$prices[[1]]$prices[[1]][1,2] = "1.49"
#1,2
truthoutput$prices[[1]]$prices[[2]] = rbind(truthoutput$prices[[1]]$prices[[2]], c(6, "21.50")) %>% arrange(row)
truthoutput$prices[[1]]$prices[[2]][7,2] = "23.65"

#2,1
truthoutput$prices[[2]]$ids[[1]] = rbind(truthoutput$prices[[2]]$ids[[1]], c(0, "614")) %>% arrange(row)
truthoutput$prices[[2]]$ids[[1]]$row = 1:8
truthoutput$prices[[2]]$prices[[1]] = rbind(truthoutput$prices[[2]]$prices[[1]], c(0, "2.29"))
truthoutput$prices[[2]]$prices[[1]] = truthoutput$prices[[2]]$prices[[1]] %>% arrange(row)
truthoutput$prices[[2]]$prices[[1]]$row = 1:8
truthoutput$prices[[2]]$prices[[1]][6,2] = "2.99"

#2,2
truthoutput$prices[[2]]$prices[[2]] = rbind(truthoutput$prices[[2]]$prices[[2]], c(0, "24.75")) %>% arrange(row)
truthoutput$prices[[2]]$prices[[2]]$row = 1:8

#names
names(truthoutput$prices[[1]]$prices) = c("bottle", "2 bottles")
names(truthoutput$prices[[1]]$prices) = c("Bottle","Case")
names(truthoutput$prices[[2]]$prices) = c("Bottle","Case")
names(truthoutput$prices[[3]]$prices) = c("Bottle")
names(truthoutput$prices[[4]]$prices) = c("Bottle")
names(truthoutput$prices[[1]]$prices) = c("Case NOW","Case Price Upon Arrival","You Save")
names(truthoutput$prices[[2]]$prices) = c("Fifth Bottle","Fifth Case","Quart Bottle","Quart Case")

# save
#saveRDS(truthoutput, paste0("~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/UCD_Lehmann_",truth1,"_price_truth.RDS"))

