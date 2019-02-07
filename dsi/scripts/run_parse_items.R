# Code to use Stan's parse_items.R code locally instead of connection to his database the dictionaries are loaded locally
# Jane Carlen

# Notes: when no text

# 0. Setup ####

library(dplyr)
library(tidyverse)
library(RecordLinkage)
library(gsubfn) #for cat0

dir.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data/"
output.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data/sample_output/"

# similarity threshold above which we consider two strings as potential match
SIMILARITY_THRESHOLD = 0.8;
# pattern threshold - percentage of items that have to meet criteria to accept considered rule as a general pattern
PATTERN_THRESHOLD = 0.7;

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

source("~/Documents/DSI/wine-price-extraction/dsi/R/parse_items.R")

# parse all pages in the folder
setwd(output.directory)
global_stats = parse_folder(".");
cat(format_global_stats(global_stats));

# parse single page
parse_page("input/UCD_Lehmann_3794.RDS")
