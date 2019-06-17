# Code to use Stan's parse_items.R code locally instead of connection to his database the dictionaries are loaded locally
# Jane Carlen

# 0. Setup ####
library(stringr)
library(tablewine)
if("package:MASS" %in% search()) detach("package:MASS")

# 1. Args ####
# IF running from this script:

# NAME.INPUT.DIR = OUTPUT.DIR
# NAME.OUTPUT.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data"

# For command line args, case doesn't matter (they'll be converted to upper either way)
possible.args = c("NAME.INPUT.DIR", "NAME.OUTPUT.DIR")
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

  NAME.INPUT.DIR = argvals[1]
  NAME.OUTPUT.DIR = argvals[2]
}

# 2. Arg Checks ####

if (!file.exists (NAME.INPUT.DIR) ) {
  stop(call. = FALSE, "Path to input data (price table output RDS files) not valid.")
}

if (!file.exists (NAME.OUTPUT.DIR) ) {
  stop(call. = FALSE, "Path to store name parse output not valid.")
}

# 3. More settings ####

# similarity threshold above which we consider two strings as potential match
SIMILARITY_THRESHOLD = 0.8;
# pattern threshold - percentage of items that have to meet criteria to accept considered rule as a general pattern
PATTERN_THRESHOLD = 0.5;

# dictionaries now part of package
# get provinces
data("provinces")
data("regions")
data("producers")
data("designations")
data("varieties")

# 4. Run ####

# parse all pages in the folder  Works for one pages as well
parse_folder = parseFolder(NAME.INPUT.DIR, PATTERN_THRESHOLD, SIMILARITY_THRESHOLD);
saveRDS(parse_folder, file.path(NAME.OUTPUT.DIR, "parsed_folder.RDS"))
