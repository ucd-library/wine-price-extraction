# Code to use Stan's parse_items.R code locally instead of connection to his database the dictionaries are loaded locally
# Jane Carlen

# 0. Setup ####

library(dplyr)
library(tidyverse)
library(RecordLinkage)
library(gsubfn) #for cat0
if("package:MASS" %in% search()) detach("package:MASS")

dir.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data/"
output.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data/sample_output/" #goes in "output" folder there

# similarity threshold above which we consider two strings as potential match
SIMILARITY_THRESHOLD = 0.8;
# pattern threshold - percentage of items that have to meet criteria to accept considered rule as a general pattern
PATTERN_THRESHOLD = 0.5;

# load dictionaries LOCALLY
# get provinces
provinces = read.csv(file.path(dir.directory, "provinces.csv"))[,-1]
# get regions
regions = read.csv(file.path(dir.directory, "regions.csv"))[,-1]
# get producers
producers = read.csv(file.path(dir.directory, "producers.csv"))[,-1]
# get designations
designations = read.csv(file.path(dir.directory, "designations.csv")) %>% select(Designation) #only one variable, want to keep data frame
 # get varieties
varieties = read.csv(file.path(dir.directory,"varieties.csv")) %>% select(Variety)

# 1. Run ####

source("~/Documents/DSI/wine-price-extraction/dsi/R/parse_items_data.R")

# parse all pages in the folder
#setwd(output.directory)
parse_folder = parseFolder(output.directory, PATTERN_THRESHOLD, SIMILARITY_THRESHOLD);
#cat(format_global_stats(global_stats)); <- from old way where outputof parse folder is global stats

# parse single page
# pageResults("UCD_Lehmann_0208.RDS")
 