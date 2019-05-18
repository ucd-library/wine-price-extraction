# Script to run the evaluation functions written in R/wine_evaluate.R
# Jane Carlen

##################################################################################################################
# 0. Setup
##################################################################################################################

library(reshape2)
library(ggplot2)
library(dplyr)
library(RecordLinkage)

#source("~/Documents/DSI/wine-price-extraction/dsi/R/wine_evaluate.R")

#EVAL.INPUT.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data/price_table_output"
#TRUTH.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth"

args = commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  EVAL.INPUT.DIR = args[1] 
  if (length(args) > 1) {
    TRUTH.DIR = args[2] # Where manually created truth file .RDS live
    if (length(args) > 2) {
      EVAL.OUTPUT.DIR = args[3]
    }
  }
}

if (!file.exists (EVAL.INPUT.DIR) ) {
  stop("Path to input data (price table output RDS files) not valid. Stopping.")
} 

if (!file.exists (TRUTH.DIR) ) {
  stop("Path to folder containing truth files (.RDS) not valid. Stopping.")
} 

# file.number = "0069"
# test.prices = readRDS(file.path(output.directory, paste0("UCD_Lehmann_", file.number,".RDS")))$prices
# truth.prices = readRDS(file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))$prices

#if correction necessary to truth, something like:
#tmp = readRDS(file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))
#tmp$prices[[1]]$prices[[2]]$row = 1:8
#saveRDS(tmp, file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))

##################################################################################################################
# 2. Run comparison on fileset
##################################################################################################################

# A. Basic stats for all files ####

    # internal check ----
fileset1 = list.files(EVAL.INPUT.DIR, pattern = ".RDS", full.names = TRUE, recursive = FALSE)

evaluate.output = vector("list", length(fileset1)) #returns list
for (i in 1:length(fileset1)) {
  rds1 = readRDS(fileset1[i])$prices
  eval1 = wine.evaluate(rds1)
  evaluate.output[[i]] = eval1
} 

output_summary_internal = data.frame(t(sapply(evaluate.output, function(eval) {
  c("n.tables" = eval$n.tables,
    "n.columns.total" = eval$n.columns.total,
    "n.entries.total" = eval$n.entries.total,
    "n.columns.per.table" = paste(eval$n.columns.per.table, collapse = ", "),
    "n.entries.per.column" = paste(unlist(eval$n.entries.per.column), collapse = ", "),
    "column.names" = paste(names(unlist(eval$n.entries.per.column)), collapse = ", "),
    "column.ratios" = paste(round(as.numeric(unlist(eval$table.ratios.summary), 2)), collapse = ", "),
    "n.unsorted" = paste(unlist(sapply(eval$table.ordering, sapply, first)), collapse = ", "), #number of prices than next one in column
    "mean.unsorted" =  paste(unlist(sapply(eval$table.ordering, sapply, nth, 2)), collapse = ", ")#mean of how much they're greater by
  )
  })),  
  row.names = basename(fileset1))

write.csv(output_summary_internal, file.path(EVAL.OUTPUT.DIR, "output_summary_internal.csv"))

    # plot that ----
output_summary_internal_singlestat = ggplot( melt(output_summary_internal %>% 
                                                    dplyr::select(c("n.tables", "n.columns.total", "n.entries.total")), id.vars = NULL) %>%
                                               mutate(value = as.numeric(value))) + 
  geom_histogram(aes(x = value, group = variable)) + facet_grid(~variable, scales = "free_x")

ggsave(output_summary_internal_singlestat, filename = file.path(NAME.OUTPUT.DIR, "output_summary_internal_singlestat.png"))

# B. Compare to truth For files we have truth for ####

# Assumes we might have the truth files organized by folder (=  name of truth maker)
truth.subdir = list.dirs(TRUTH.DIR, full.names = T, recursive = T)

summary.output = lapply(truth.subdir, function(elem) {

  fileset.truth = list.files(elem, pattern = ".RDS", full.names = TRUE)

  if (length(fileset.truth) > 0) {
    
    summary.output1 = lapply(fileset.truth, function(truth.file) {

      test.name = gsub( file.path(EVAL.INPUT.DIR, basename(truth.file)),  pattern = "_price_truth", replacement = "" )
      if ( file.exists(test.name)) {
      test.prices = readRDS( test.name )$prices
      } else {
        warning("no truth file:", test.name)
        return(NULL)
      }
      truth.prices =  readRDS(truth.file)$prices
    
      compare.list = NULL
      print(truth.file)
      try({compare.list = wine.compare(test.prices, truth.prices)})
      if (!is.null(compare.list)) {
        wine.summarize(compare.list)
      } 
    })
    
    rownames1 = basename(fileset.truth)[!sapply(summary.output1, is.null)] 
    summary.output1 = do.call("rbind", summary.output1)
    rownames(summary.output1) = rownames1
    summary.output1$name = basename(elem) #name is containing folder
    summary.output1
    
  } else {return(NULL)}
})

summary.output = do.call("rbind", summary.output)

write.csv(summary.output, file.path(EVAL.OUTPUT.DIR, "summary_vs_truth.csv"))

# C. Compile all truth data to bind to extract prices in ENTRY_PRICE ----

truth_all = lapply(truth.subdir, function(elem) {
  
  fileset.truth = list.files(elem, pattern = ".RDS", full.names = TRUE)

  if (length(fileset.truth) > 0) {
    truth1 = lapply(fileset.truth, function(truth.file) {
      truth.found = unlist(sapply(readRDS(truth.file)$prices, "[", "prices"), recursive = FALSE)
      cluster = rep(1:length(truth.found), times =  sapply(truth.found, nrow))
      table = rep(1:length(truth.found), sapply(truth.found, nrow))
      truth.found = do.call("rbind", truth.found)
      truth.found$table = table
      truth.found$cluster = cluster
      truth.found$file_id = gsub(basename(truth.file), pattern = "_price_truth.RDS", replacement = "")
      truth.found$truth_entered_by = basename(elem)
      return(truth.found)
    })
  } else {truth1 = list()}
   
  return(do.call("rbind", truth1))
})

truth_all = do.call("rbind", truth_all)

# Check accuracy of truth by checking for deviation between repeated entries
#View(truth_all %>% group_by(file_id, table, cluster, row) %>% mutate(n = n()) %>%
#  filter(n > 1) %>% summarize(var(n)))

# If all accurate, remove repeated
truth_all = truth_all %>% group_by(file_id, table, cluster, row) %>%
  arrange(truth_entered_by) %>% summarize_all(first) %>% ungroup()

names(truth_all)[names(truth_all)=="text.new"] = "text.true"

write.csv(truth_all, file.path(EVAL.OUTPUT.DIR, "truth_all.csv"))


