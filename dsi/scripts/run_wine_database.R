# File to merge price and name output into requested database
#
# TO DO: 
#     - we may have an id from name extraction and one from table extraction, check if they align
#     fix name naming so that ENTRY_PRICE and ENTRY_NAME have same number of entries
#     - Add dictionary hit info to NAME_MATCH table
#
# Tables
# ENTRY_PRICE
  # key is entry_id = 0923_1_23 is file number, table number, entry number
# ENTRY_NAME 
  # key is name_id = 0923_1_23 is file number, table number, row number
# PRICE_NAME 
  # key is entry_id. is a join of ENTRY_PRICE and ENTRY_NAME by name_id with reduced fields.
# NAME_MATCH 
  # **key is word_id**, which is entry_id (also in table) with an additional index pasted on
  # has individual word information (possibly multiple lines) for each entry in ENTRY_NAME

######################################################################################################################################

# 0. Setup ----

library(dplyr)
library(stringr)

setwd("~/Documents/DSI")

# Table extraction
source("wine-price-extraction/dsi/R/wine_price_pageCols.R") #redundant
source("wine-price-extraction/dsi/R/wine_price_tables_functions.R") #redundant
source("wine-price-extraction/dsi/R/wine_price_nameBoxes.R") #redundant
source("wine-price-extraction/dsi/R/helper.R") #redundant
source("wine-price-extraction/dsi/R/wine_price_tables.R")

# Name parsing
source("wine-price-extraction/dsi/R/parse_items_data.R")

# Evaluation
source("~/Documents/DSI/wine-price-extraction/dsi/R/wine_evaluate.R")

# 1. Run image files ----

# These can be overwritten by command line args
# If a valid path is given for the input folder and output folder then the data will be used and saved by default
# If no input folder is given the images will be re-ocr'd and if no valid output folder is given it won't be saved
# SAVE.DATA will be used if set

#FILESET = "/Users/janecarlen/Documents/DSI/OCR_SherryLehmann/test_image"
#FILESET = "/Users/janecarlen/Documents/DSI/OCR_SherryLehmann/MoreTruthPages/" #<- do both
FILESET = "/Users/janecarlen/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages"#<-< do both
OUTPUT.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data/price_table_output/"
DATA.INPUT.DIR = "/Users/janecarlen/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed"
#DATA.OUTPUT.DIR = /Users/janecarlen/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed",
SAVE.DATA = FALSE

source("~/Documents/DSI/wine-price-extraction/dsi/scripts/run_wine_price_tables.R")

#Check that files ran
list.files(FILESET, pattern = ".jpg")[! sapply(str_split(list.files(FILESET, pattern = ".jpg"), "\\."), first) %in% sapply(str_split(list.files(OUTPUT.DIR, pattern = ".RDS"), "\\."), first) ]

# 2. Parse names from output of 1----

NAME.INPUT.DIR = OUTPUT.DIR
DICTIONARY.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data/dictionaries"
NAME.OUTPUT.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data"

source("~/Documents/DSI/wine-price-extraction/dsi/scripts/run_parse_items.R")

parsed_folder = readRDS(file.path(NAME.OUTPUT.DIR, "parse_folder_sample.RDS")) # done in run_parse_items - can source

# 3. Run evaluation code ----

#uses OUTPUT.DIR again
EVAL.INPUT.DIR = OUTPUT.DIR
TRUTH.DIR = "~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth"
EVAL.OUTPUT.DIR = NAME.OUTPUT.DIR

source("~/Documents/DSI/wine-price-extraction/dsi/scripts/run_wine_evaluate.R")

# 4. Build database tables ----

TABLE.OUTPUT.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data"

#     Load NAME data ----
page_results_all = do.call("rbind", parsed_folder$page_results)
dim(page_results_all) # a matrix
colnames(page_results_all)

#     ENTRY_NAME ----

#  categories that aren't data frame or OCRResults type
exclude1 = names(unlist(sapply(page_results_all[1,], nrow)))

# Currently converting all to characters for convencience
ENTRY_NAME = data.frame(page_results_all[,!colnames(page_results_all) %in% exclude1]) %>% mutate_if(is.list, as.character)
ENTRY_NAME$file = str_extract(ENTRY_NAME$file, "UCD.*")
ENTRY_NAME$file_number = str_extract(ENTRY_NAME$file, pattern = "[0-9]{4}")
ENTRY_NAME = ENTRY_NAME %>% group_by(file, table) %>% mutate(name_id = paste(file_number, table, 1:n(), sep = "_"))
write.csv(ENTRY_NAME, file.path(TABLE.OUTPUT.DIR, "ENTRY_NAME.csv"), row.names = FALSE)

#     NAME_MATCH ----

NAME_MATCH = data.frame(page_results_all[,colnames(page_results_all) %in% exclude1])
colnames(NAME_MATCH)
#[1] "text_conf"           "dictionary_hits"     "dictionary_hits_sim"
# ignoring dictionary hits tables for now <<- ADD

NAME_MATCH  = NAME_MATCH[["text_conf"]]
NAME_MATCH  = lapply(1:length(NAME_MATCH ), function(i) {
  if (is.null(dim(NAME_MATCH [[i]]))) {
    return(data.frame(text ="", confidence = 0, name_id = ENTRY_NAME$name_id[i], 
           word_id = paste(ENTRY_NAME$name_id[i], 1, sep = "_")))
  } else {
    NAME_MATCH[[i]]$name_id = ENTRY_NAME$name_id[i] 
    NAME_MATCH[[i]]$word_id = paste(ENTRY_NAME$name_id[i], 1:nrow(NAME_MATCH[[i]]))
    return(NAME_MATCH[[i]])
    }
  })
NAME_MATCH  = do.call("rbind", NAME_MATCH )
write.csv(NAME_MATCH, file.path(TABLE.OUTPUT.DIR, "NAME_MATCH.csv"), row.names = FALSE)

#     Load PRICE data ----

price_RDS_files = list.files(OUTPUT.DIR, full.names = TRUE, pattern = ".RDS", recursive = F)

#     ENTRY PRICE ----

price_output = lapply(price_RDS_files, function(x) {
  output = readRDS(x)
  prices = do.call("rbind", output$page.cols$prices)
  prices = prices %>% mutate(
    file = str_extract(x, pattern = "UCD_Lehmann_[0-9]{4}"),
    file_number = str_extract(file, pattern = "[0-9]{4}"),
    entry_id = paste(file_number, table, 1:nrow(prices), sep = "_"),
    name_id = paste(file_number, table, row, sep = "_"),
    )
})

ENTRY_PRICE = do.call("rbind", price_output)
# CHANGE "text" names to "price"
names(ENTRY_PRICE)[names(ENTRY_PRICE) == "text"] = "price_raw"
names(ENTRY_PRICE)[names(ENTRY_PRICE) == "confidence"] = "confidence"
names(ENTRY_PRICE)[names(ENTRY_PRICE) == "text.new"] = "price_new"

write.csv(ENTRY_PRICE, file.path(TABLE.OUTPUT.DIR, "ENTRY_PRICE.csv"), row.names = FALSE)

#     PRICE_NAME ----

# see https://github.com/ucd-library/wine-price-extraction/issues/9 for discussion of vars
text_vars_to_include = c("text", "text_raw", "name", "id", "name_id")
wine_vars_to_include = c("country", "year", "color", "variety", "region", "province", "designation")
price_vars_to_include = c("price_raw", "confidence", "type", "price_new", "cluster", "table","row", "entry_id", "name_id")

PRICE_NAME = left_join(ENTRY_PRICE[,price_vars_to_include],
                        ENTRY_NAME[,c(text_vars_to_include, wine_vars_to_include)], 
                        by = "name_id", suffix = c(".price", ".name")) 

names(PRICE_NAME)

write.csv(PRICE_NAME, file.path(TABLE.OUTPUT.DIR, "PRICE_NAME.csv"), row.names = FALSE)


