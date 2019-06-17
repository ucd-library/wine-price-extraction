# Script to make a truth .csv of all truth files in  evaluation functions written in R/wine_evaluate.R
# Jane Carlen

# 0. Setup

library(tablewine)

# If running from script, set args: ----
#TRUTH.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth"
#EVAL.OUTPUT.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data"

# If running from source, set args: ----

# For command line args, case doesn't matter (they'll be converted to upper either way)
possible.args = c("TRUTH.DIR", "EVAL.OUTPUT.DIR")
args = commandArgs(trailingOnly = TRUE)
print("Any command line args?")
print(args)

if (length(args) >= 1) {
  
  argnames = toupper(sapply(args, function(x) strsplit(x, "=")[[1]][1])) # For command line args, case doesn't matter
  argnums = sapply(possible.args, match, argnames)
  argvals = rep(NA, length(possible.args))
  argvals[which(!is.na(argnums))] = 
    sapply(args, function(x) trimws(last(strsplit(x, "=")[[1]])) )[argnums[!is.na(argnums)]]
  
  TRUTH.DIR = argvals[1]
  EVAL.OUTPUT.DIR = argvals[2]
}

truth.subdir = list.dirs(TRUTH.DIR, full.names = T, recursive = T)

# Compile all truth data to bind to extract prices in ENTRY_PRICE ----

truth_all = lapply(truth.subdir, function(elem) {
  
  fileset.truth = list.files(elem, pattern = ".RDS", full.names = TRUE)
  
  if (length(fileset.truth) > 0) {
    truth1 = lapply(fileset.truth, function(truth.file) {
      tmp.prices = sapply(readRDS(truth.file)$prices, "[", "prices") 
      table = rep(1:length(tmp.prices), times = sapply(tmp.prices, function(x) sum(sapply(x, nrow))))
      truth.found = unlist(tmp.prices, recursive = FALSE)
      cluster = rep(1:length(truth.found), times =  sapply(truth.found, nrow))
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

write.csv(truth_all, file.path(EVAL.OUTPUT.DIR, "ENTRY_TRUTH.csv"))


