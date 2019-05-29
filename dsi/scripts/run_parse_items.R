# Code to use Stan's parse_items.R code locally instead of connection to his database the dictionaries are loaded locally
# Jane Carlen

# 0. Setup ####

library(dplyr)
library(tidyverse)
library(RecordLinkage)
library(gsubfn) #for cat0
if("package:MASS" %in% search()) detach("package:MASS")

# 1. Args ####
# IF running from this script:

# NAME.INPUT.DIR = OUTPUT.DIR
# DICTIONARY.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data/dictionaries"
# NAME.OUTPUT.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data"

# data.directory = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data/price_table_output/"
# dictionary.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data/dictionaries"
# output.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data/sample_output/"

# For command line args, case doesn't matter (they'll be converted to upper either way)
possible.args = c("NAME.INPUT.DIR", "DICTIONARY.DIR", "NAME.OUTPUT.DIR")
args = commandArgs(trailingOnly = TRUE)
print(args)

# Use command line args if running from terminal:
if (length(args) >= 1) {

  argnames = toupper(sapply(args, function(x) strsplit(x, "=")[[1]][1])) # For command line args, case doesn't matter
  argnums = sapply(possible.args, match, argnames)
  argvals = rep(NA, length(possible.args))
  argvals[which(!is.na(argnums))] =
    sapply(args, function(x) trimws(last(strsplit(x, "=")[[1]])) )[argnums[!is.na(argnums)]]

  NAME.INPUT.DIR = argvals[1]
  DICTIONARY.DIR = argvals[2]
  NAME.OUTPUT.DIR = argvals[3]
}

# 2. Arg Checks ####

if (!file.exists (NAME.INPUT.DIR) ) {
  stop(call. = FALSE, "Path to input data (price table output RDS files) not valid.")
}

if (!file.exists (DICTIONARY.DIR) ) {
  stop(call. = FALSE, "Path to folder containing dictionairies not valid.")
}

if (!file.exists (NAME.OUTPUT.DIR) ) {
  stop(call. = FALSE, "Path to store name parse output not valid.")
}

# 3. More settings ####

# similarity threshold above which we consider two strings as potential match
SIMILARITY_THRESHOLD = 0.8;
# pattern threshold - percentage of items that have to meet criteria to accept considered rule as a general pattern
PATTERN_THRESHOLD = 0.5;

# load dictionaries LOCALLY
# get provinces
provinces = read.csv(file.path(DICTIONARY.DIR, "provinces.csv"))[,-1]
# get regions
regions = read.csv(file.path(DICTIONARY.DIR, "regions.csv"))[,-1]
# get producers
producers = read.csv(file.path(DICTIONARY.DIR, "producers.csv"))[,-1]
# get designations
designations = read.csv(file.path(DICTIONARY.DIR, "designations.csv")) %>% select(Designation) #only one variable, want to keep data frame
 # get varieties
varieties = read.csv(file.path(DICTIONARY.DIR,"varieties.csv")) %>% select(Variety)

# 4. Run ####

source("/opt/dsi/R/parse_items_data.R")

# parse all pages in the folder
parse_folder = parseFolder(NAME.INPUT.DIR, PATTERN_THRESHOLD, SIMILARITY_THRESHOLD);
saveRDS(parse_folder, file.path(NAME.OUTPUT.DIR, "parse_folder_sample.RDS"))
#cat(format_global_stats(global_stats)); <- from old way where outputof parse folder is global stats

# parse single page
# pageResults("UCD_Lehmann_0208.RDS")
