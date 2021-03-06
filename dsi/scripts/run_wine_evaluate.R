# Script to run the evaluation functions written in R/wine_evaluate.R
# Jane Carlen

##################################################################################################################
# 0. Setup
##################################################################################################################

library(tablewine)
#library(reshape2)

#EVAL.INPUT.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data/price_table_output"
#TRUTH.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth"
#EVAL.OUTPUT.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data"

# For command line args, case doesn't matter (they'll be converted to upper either way)
possible.args = c("EVAL.INPUT.DIR", "TRUTH.DIR", "EVAL.OUTPUT.DIR")
args = commandArgs(trailingOnly = TRUE)
print("Any command line args?")
print(args)

# Use command line args if running from terminal:
if (length(args) >= 1) {
  
  argnames = toupper(sapply(args, function(x) strsplit(x, "=")[[1]][1])) # For command line args, case doesn't matter
  argnums = sapply(possible.args, match, argnames)
  argvals = rep(NA, length(possible.args))
  argvals[which(!is.na(argnums))] = 
    sapply(args, function(x) trimws(last(strsplit(x, "=")[[1]])) )[argnums[!is.na(argnums)]]
  
  EVAL.INPUT.DIR = argvals[1]
  TRUTH.DIR = argvals[2]
  EVAL.OUTPUT.DIR = argvals[3]
}

# Checks

if (!file.exists (EVAL.INPUT.DIR) ) {
  stop(call. = FALSE, "Path to input data (price table output RDS files) not valid.")
} 

if (!file.exists (TRUTH.DIR) ) {
  stop(call. = FALSE, "Path to folder containing truth files (.RDS) not valid.")
} 

if (!file.exists (EVAL.OUTPUT.DIR) ) {
  stop(call. = FALSE, "Path to store evaluation output not valid.")
} 

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
output_summary_internal_singlestat = ggplot( reshape2::melt(output_summary_internal %>% 
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