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
# ENTRY_PAGE
  # key is file_id <- change to different ID system?
######################################################################################################################################

# 0. Setup ----

library(tablewine)
library(dplyr)
library(stringr)

# Source function files ----


FLIP.Y=FALSE
SAVE.DATA = FALSE
SAVE.DESKEWED = FALSE
OCR.ONLY = FALSE
BINARY.THRESHOLD = 150

possible.args = c("IN","TRUTH.DIR","FLIP.Y")
args.default = c(NA,NA,FALSE);

args = commandArgs(trailingOnly = TRUE)
print(args)

# Use command line args if running from terminal:
if (length(args) >= 1) {

  argnames = toupper(sapply(args, function(x) strsplit(x, "=")[[1]][1])) # For command line args, case doesn't matter
  argnums = sapply(possible.args, match, argnames)
  args.default[which(!is.na(argnums))] =
    sapply(args, function(x) trimws(last(strsplit(x, "=")[[1]])) )[argnums[!is.na(argnums)]]

# In is the pointer to the parse_folder_sample.RDS file.  OR whatever we rename it.
  IN = args.default[1]
	TRUTH.DIR= args.default[2]
  # flip.y argument for back-rotating image points, accounts for plotting images with y = 0 at top left instead of bottom left
#  FLIP.Y = args.default[3]
# We could add all as command line  arguments
#SAVE.DATA = FALSE
#SAVE.DESKEWED = FALSE
#OCR.ONLY = FALSE
#BINARY.THRESHOLD = 150

 thisFile <- function() {
    cmdArgs <- commandArgs(trailingOnly = FALSE)
    needle <- "--file="
    match <- grep(needle, cmdArgs)
    if (length(match) > 0) {
      # Rscript
      return(dirname(sub(needle, "", cmdArgs[match])))
    } else {
      # 'source'd via R console
      return(dirname(sys.frames()[[1]]$ofile))
    }
  }

  thisdir <- thisFile()
}

# Table extraction
#source(file.path(thisdir,"../R/wine_price_pageCols.R"), echo = F) #redundant
#source(file.path(thisdir,"../R/wine_price_tables_functions.R"), echo = F) #redundant
#source(file.path(thisdir,"../R/wine_price_nameBoxes.R"), echo = F) #redundant
#source(file.path(thisdir,"../R/helper.R"), echo = F) #redundant
#source(file.path(thisdir,"../R/wine_price_tables.R"), echo = F)

# Evaluation
#source(file.path(thisdir,"../R/wine_evaluate.R"))
# Functions to summarize name hit success and add flags to ENTRY_PRICE
#source(file.path(thisdir,"../R/wine_flag_and_summarize.R"))

OUTPUT.DIR = dirname(IN);

# In is the pointer to the parse_folder_sample.RDS file.  OR whatever we rename it.
parsed_folder = readRDS(IN);

# THESE ARE THE GLOBALS FOR THE run_wine_evaluate SCRIPT
EVAL.INPUT.DIR=OUTPUT.DIR;
EVAL.OUTPUT.DIR=OUTPUT.DIR;
NAME.OUTPUT.DIR=OUTPUT.DIR;
#TRUTH.DIR=TRUTH.DIR

# Wait for now
# source(file.path(thisdir,"../scripts/run_wine_evaluate.R"))
# after running, we have data in output_summary_internal.csv, summary_vs_truth.csv,truth_all.csv


#     Load NAME data ----
page_results_all = do.call("rbind", parsed_folder$page_results)
dim(page_results_all) # a matrix
colnames(page_results_all)

#     ENTRY_NAME ----

#  categories that aren't data frame or OCRResults type
exclude1 = names(unlist(sapply(page_results_all[1,], nrow)))

# Currently converting all to characters for convencience

ENTRY_NAME = data.frame(page_results_all[,!colnames(page_results_all) %in% exclude1]) %>% mutate_if(is.list, as.character)
# remove unused file_name col and confidence which is all null
ENTRY_NAME = ENTRY_NAME %>% select(-c("file_name", "confidence")) %>% mutate(file_id = tools::file_path_sans_ext(basename(file)))
ENTRY_NAME = ENTRY_NAME %>% group_by(file_id, table) %>% mutate(name_id = paste(file_id, table, 1:n(), sep = "_"))


#     - Load PRICE data ----
price_RDS_files = list.files(OUTPUT.DIR, full.names = TRUE, pattern = "*-[0-9]+.RDS", recursive = F)

# Can unify this with below so not looping through price_RDS_files twice
name_output =
  do.call("rbind",
  lapply(price_RDS_files, function(x) {
    output = readRDS(x)
    rows = unlist(sapply(output$name.locations, rownames))
    nameboxes = do.call("rbind", output$name.locations)
    nameboxes$table = as.numeric(sapply( strsplit(rownames(nameboxes), "_|\\.") , nth, 2 ))
    nameboxes$number = as.numeric(sapply( strsplit(rownames(nameboxes), "_|\\.") , last ))
    nameboxes$file_id = tools::file_path_sans_ext(basename(x))
    # possible that there can be slightly more names than prices if there were ids with unmatched price rows
      # latest run:
      # sum(!ENTRY_NAME$name_id %in% ENTRY_PRICE$name_id) #5
      # "0027_1_14" "0237_1_26" "0644_1_9"  "2504_1_4"  "2504_2_5"
    nameboxes$name_id = paste(nameboxes$file_id, nameboxes$table, rows, sep = "_")

    # Add an xy coordinate for the app to use
    # flip.y argument can account for when images are plotted with flipped y scales
    nameboxes_xy = nameboxes %>% mutate(x = (l + r)/2, y = (t + b)/2) %>% select(x, y)
    nameboxes_xy_orig = rotatePoints(nameboxes_xy,
                                  angle = output$page.cols$angle[1],
                                  height = output$page.cols$height_orig, width = output$page.cols$width_orig,
                                  flip.y = FLIP.Y)
    nameboxes = cbind(nameboxes, name_center_x_orig = nameboxes_xy_orig[,1], name_center_y_orig = nameboxes_xy_orig[,2])

    return(nameboxes)
  }))

#check
sum(ENTRY_NAME$name_id != name_output$name_id)

ENTRY_NAME = left_join(ENTRY_NAME,
                       name_output[,c("l", "b", "r", "t","name_center_x_orig", "name_center_y_orig", "name_id")],
                       by = "name_id")


write.csv(ENTRY_NAME, file.path(OUTPUT.DIR, "ENTRY_NAME.csv"), row.names = FALSE)

#     Dictionary hit stat summary
# Various performance-evaluating code that acts on TABLE output of run_wine_database.R
# May 2019

# 1. Flag prices by ratio, not increasing order, magnitude, year, digit placement, etc. (in ENTRY_PRICE) ----

# Hangshi's functions

# Flag potential prices based on likely being a year
# Input is a vector of detected prices, most likely ENTRY_PRICE$price_new
# Arguments set allowable year range and which column to check defaults to price_new
# Output a vector of Boolean values, where TRUE means flagged.

year_flag = function(prices_to_check, year_lower = 1800, year_upper = 2000){
  flag = lapply(1:length(prices_to_check),function(i){
    price = as.numeric(as.character((prices_to_check[i])))
    #set conditions to filter out error entries, and filtered ones are marked unflagged
    if (length(price)== 1&& !anyNA(price) && !price %in% c('','FALSE') && !grepl("\\.",price)){
      #flag the price that falls into the year range
      if (price >= year_lower && price < year_upper){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    else{
      return(FALSE)
    }
  })
  flag = matrix(unlist(flag))
  return(flag)
}

# Flag potential prices based on wrong numbers of digits before or after the decimal
# Input is a vector of detected prices, most likely ENTRY_PRICE$price_new
# Aguments set allowable digits left and right of the dot
# Output is a vector of Boolean values, where TRUE means flagged.
size_flag = function(prices_to_check, size_left = 4, size_right = 2){

  flag = lapply(1:length(prices_to_check),function(i){
    price = as.character((prices_to_check[i]))
    #set conditions to filter errors out, and return error with FALSE
    if (length(price)== 1&& !anyNA(price) && !price %in% c('','FALSE')){
      #split the price by dot
      if (grepl("\\.",price)){
        digit1 = unlist(strsplit(as.character(price),'\\.'))[1]
        digit2 = unlist(strsplit(as.character(price),'\\.'))[2]
        #in case there is nothing after the dot
        if (is.na(digit2)){
          #check the number on the left of the dot
          if(nchar(digit1) > size_left){
            return(TRUE)
          }
          else{
            return(FALSE)
          }
        }
        #in case there are digits on both sides
        else if ((nchar(digit1) > size_left) | nchar(digit2) > size_right){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
      else{
        return(FALSE)
      }
    }
    else{
      return(FALSE)
    }
  })
  flag = unlist(flag)
  return(flag)
}

# Flag potential prices based on unlikely last digit (least helpful flag)
# Input is a vector of detected prices, most likely ENTRY_PRICE$price_new
# Argument "ratio" sets minimum percentage a digit has to represent to not get flagged
# Output is a vector of Boolean values, where TRUE means flagged.
digit_flag = function(prices_to_check, ratio = .04){
  #collect the second digits after the dot of each price
  flag_train = lapply(1:length(prices_to_check),function(i){
    price = as.character((prices_to_check[i]))
    #set conditions to filter errors out
    if (length(price)== 1&& !anyNA(price) && !price %in% c('','FALSE')){
      #split price with dot and collect the second digits after dot
      if (grepl("\\.",price)){
        digit = unlist(strsplit(as.character(price),'\\.'))[2]
        #in case there is nothing after the dot
        if (is.na(digit)){
          return(NULL)
        }
        #in case the second digit is zero and ignored
        else if (nchar(digit) == 1){
          return(0)
        }
        #in case the second digit is shown
        else if (nchar(digit) == 2){
          return(as.numeric(substr(digit,2,2)))
        }
      }
    }
  })
  flag_train = matrix(unlist(flag_train))
  #convert output to data frame
  flag_df = as.data.frame(table(flag_train))
  #start trainning with the ratio input
  flag_digit = lapply(1:10,function(i){
    if (flag_df[i,2]/length(flag_train) >= ratio){
      return(as.numeric(as.character(flag_df[i,1])))
    }
  })
  flag_digit = matrix(unlist(flag_digit))
  #flagging where TRUE means flagged
  flag = lapply(1:length(prices_to_check), function(i){
    price = as.character((prices_to_check[i]))
    #set conditions to filter errors out, and return error with FALSE
    if (length(price)== 1&& !anyNA(price) && !price %in% c('','FALSE')){
      if (grepl("\\.",price)){
        digit = unlist(strsplit(as.character(price),'\\.'))[2]
        #in case there is nothing after the dot and 0 is unflagged
        if (is.na(digit) && 0 %in% flag_digit){
          return(FALSE)
        }
        #in case 0 is ignored and unflagged
        else if (nchar(digit) == 1 && 0 %in% flag_digit){
          return(FALSE)
        }
        #in case the second digit falls into flag_digit
        else if (nchar(digit) == 2){
          if (substr(digit,2,2) %in% flag_digit){
            return(FALSE)
          }
          else{
            return(TRUE)
          }
        }
        else{
          return(FALSE)
        }
      }
      else{
        return(FALSE)
      }
    }
    else{
      return(FALSE)
    }
  })
  flag = matrix(unlist(flag))
  return(flag)
}

# One jane added:
# Flag potential prices based on amount being too small or large
# Input is a vector of detected prices, most likely ENTRY_PRICE$price_new
# Arguments sets range (min_price to max_price) that would not be flagged
# Output is a vector of Boolean values, where TRUE means flagged.
amount_flag = function(prices_to_check, min_price = 0.1, max_price = 2000) {
  numeric_price = as.numeric(prices_to_check)
  sapply( numeric_price, function(x) {
    ifelse( !is.na(x) && (x < min_price | x >= max_price), TRUE, FALSE)
  })
}

# David's functin:
# check for nonincreasing values in table price column.
# Takes a table, mostly likely ENTRY_PRICE, as input
  # argument "tocheck" says which column of detected prices to check, defaulting to "price_new"
    # "price_raw" may be useful in some cases.
# Output is a vector of Boolean values where TRUE means the row in TABLE is not in increasing order,
  # i.e. the selected row had a price that was lower than the row before it

order_flag = function(TABLE, tocheck = "price_new"){

  TABLE = TABLE[c(tocheck,'cluster','table','row','file_id')]
  result = vector(length = nrow(TABLE))

  for (i in unique(TABLE$file_id)){
    subsetfile = TABLE[TABLE$file_id == i,]
    for (j in unique(subsetfile$table)){
      subsettable = subsetfile[subsetfile$table == j,]
      for (k in unique(subsettable$cluster)){
        subsetfinal = subsettable[subsettable$cluster == k,]
        tmp = suppressWarnings(as.numeric(as.character(subsetfinal[,1])))
        differ = vector(length = length(tmp)-1)
        for (l in seq(length(differ))) {
          if (is.na(tmp[l+1]) | is.na(tmp[l])){next}
          differ[l] = tmp[l+1]-tmp[l]
          if (differ[l] < 0) {
            result[which(TABLE$file_id==i & TABLE$table ==j & TABLE$cluster ==k & TABLE$row == l+1)] = TRUE
          }
        }
      }
    }
  }
  result
}

# 2. Summarize success of name dictionary matching (in ENTRY_NAME) ----
# See parse_items.R for more description of fields

name_summary_global_stats <- function(ENTRY_NAME) {

  round(t(

    with(ENTRY_NAME,

         data.frame(
           items = nrow(ENTRY_NAME),
           pages = n_distinct(file),
           #note, no actual dictionary for color since only a few possible values, but I'm including it in dict hits
           total_dict_hits = sum(ENTRY_NAME[,c("color", "province", "region",
                                               "producer", "designation", "variety", "country")] != "NULL"),
           avg_dict_hits_per_item = sum(ENTRY_NAME[,c("color", "province", "region",
                                                      "producer", "designation", "variety", "country")] != "NULL")/nrow(ENTRY_NAME),
           pct.dictionary_hit = mean(as.logical(dictionary_hit)), # % items w/ any hit in dictionaries (full, decent, or sub)
           pct.any_hit = mean(as.logical(any_hit)), # % items w /no hits at all (e.g. id, year, dictionary hits, etc.)
           pct.inspect.any = 1 - mean(inspect == "NULL"),
           pct.id = 1 - mean(id == "NULL"),
           pct.year = 1 - mean(year== "NULL"),
           pct.color = 1 - mean(color== "NULL"),
           pct.province = 1 - mean(province == "NULL"),
           pct.region = 1 - mean( region== "NULL" ),
           pct.producer = 1 - mean( producer== "NULL"),
           pct.designation = 1 - mean( designation== "NULL"),
           pct.variety = 1 - mean( variety== "NULL"),
           pct.country = 1 - mean( country== "NULL"),

           # Unique values found (-1 for "NULL")
           unique.id = length(unique(id)) - 1,
           unique.year = length(unique(year)) - 1,
           unique.color= length(unique(color)) - 1,
           unique.province= length(unique(province)) - 1,
           unique.region= length(unique(region)) - 1,
           unique.producer= length(unique(producer)) - 1,
           unique.designation= length(unique(designation)) - 1,
           unique.variety= length(unique(variety)) - 1,
           unique.country = length(unique(country)) - 1
         )
    )
  ) , 2)
}

write.csv(name_summary_global_stats(ENTRY_NAME),  file.path(OUTPUT.DIR, "name_summary_global_stats.csv"), row.names = TRUE)

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
write.csv(NAME_MATCH, file.path(OUTPUT.DIR, "NAME_MATCH.csv"), row.names = FALSE)

#     ENTRY_PRICE ----

#     - Load PRICE data ----
price_RDS_files = list.files(OUTPUT.DIR, full.names = TRUE, pattern = "*-[0-9][0-9][0-9].RDS", recursive = T)

#     - Initialize table  ----

price_output = lapply(price_RDS_files, function(x) {
  page.cols = readRDS(x)$page.cols
  match.cluster.order = match(sapply(page.cols$prices, function(x) {first(x$cluster)}), page.cols$price_cols$cluster)
  col.header = rep(page.cols$price_cols$col.header[match.cluster.order],
                   times = sapply(page.cols$prices, nrow))
  prices = do.call("rbind", page.cols$prices)
  prices = prices %>% mutate(
    file_id = tools::file_path_sans_ext(basename(x)),
    entry_id = paste(file_id, table, 1:nrow(prices), sep = "_"),
    name_id = paste(file_id, table, row, sep = "_"),
    col.header = col.header
  )
  # Add an xy coordinate for the app to use
  # flip.y argument can account for when images are plotted with flipped y scales
  prices_xy = prices %>% mutate(x = (left + right)/2, y = (top + bottom)/2) %>% select(x, y)
  prices_xy_orig = rotatePoints(prices_xy,
                                angle = page.cols$angle[1],
                                height = page.cols$height_orig, width = page.cols$width_orig,
                                flip.y = FLIP.Y)
  prices = cbind(prices, price_center_x_orig = prices_xy_orig[,1], price_center_y_orig = prices_xy_orig[,2])
})
ENTRY_PRICE = do.call("rbind", price_output) #n_distinct(ENTRY_PRICE$name_id)


# Add in Truth Tables
ENTRY_TRUTH = read.csv(file.path(TRUTH.DIR, "truth_all.csv"), stringsAsFactors = FALSE)
#ENTRY_TRUTH = read.csv(file.path(EVAL.OUTPUT.DIR, "ENTRY_TRUTH.csv"), stringsAsFactors = FALSE)

accurate_file = inner_join(ENTRY_PRICE %>% group_by(file_id) %>% summarize(n_table = n_distinct(table)),
                          ENTRY_TRUTH %>% group_by(file_id) %>% summarize(n_table = n_distinct(table)), by = c("file_id", "n_table")) %>%
                select(file_id)

ENTRY_TRUTH$accurate_file = ENTRY_TRUTH$file_id %in% accurate_file$file_id

ENTRY_PRICE = left_join(ENTRY_PRICE,
                        ENTRY_TRUTH[ENTRY_TRUTH$accurate_file,c("text.true", "truth_entered_by", "file_id",
                                                            "table", "row", "cluster")],
                        by = c("file_id" = "file_id", "table" = "table", "row" = "row", "cluster" = "cluster"))

#     - Variable transformations --------

# change "text" names to "price" and move to end
# replace existing "price" and "type" fields with just "type" on raw and new -> "type_raw" and "type_new"

ENTRY_PRICE$price_raw = ENTRY_PRICE$text
ENTRY_PRICE$type_raw = isPrice(ENTRY_PRICE$price_raw, maybe = TRUE, dollar = FALSE, years = c(1800, 1999))
ENTRY_PRICE$price_new = ENTRY_PRICE$text.new
ENTRY_PRICE$type_new = isPrice(ENTRY_PRICE$price_new, maybe = TRUE, dollar = FALSE)
ENTRY_PRICE = ENTRY_PRICE %>% select(-c("price","type", "text", "text.new"))

#     - Add flags ----

ENTRY_PRICE$flag_year = year_flag(ENTRY_PRICE$price_new)
ENTRY_PRICE$flag_amount = amount_flag(ENTRY_PRICE$price_new)
ENTRY_PRICE$flag_size = size_flag(ENTRY_PRICE$price_new, size_left = 4, size_right = 2)
#ENTRY_PRICE$flag_digit = digit_flag(ENTRY_PRICE$price_new, ratio = .04)
ENTRY_PRICE$flag_order = order_flag(ENTRY_PRICE, tocheck = "price_new")
ENTRY_PRICE$flag_type_new = ENTRY_PRICE$type_new!="TRUE"
# Sum of flags is a placeholder for class detection until we develop a better model
ENTRY_PRICE$sum_flag = rowSums(ENTRY_PRICE %>% select(contains("flag_"))) #digit has too many false positives

#ENTRY_PRICE %>% arrange(-sum_flag*!is.na(text.true))

# Save ----
write.csv(ENTRY_PRICE, file.path(OUTPUT.DIR, "ENTRY_PRICE.csv"), row.names = FALSE)

#     ENTRY_PAGE ----

page_output = lapply(price_RDS_files, function(x) {
  output = readRDS(x)
  page_info = data.frame(angle = output$page.cols$angle[1],
                         angle_conf= output$page.cols$angle[2], #I think this is a confidence measure on the angle
                         height = output$page.cols$height_orig,
                         width = output$page.cols$width_orig, binary.threshold = output$page.cols$binary.threshold,
                         pix.threshold = NA, pix.newValue = NA)
  if (exists("output$page.cols$pix.threshold")) {page_info$pix.threshold  = output$page.cols$pix.threshold}
  if (exists("output$page.cols$pix.threshold")) {page_info$pix.newValue  = output$page.cols$pix.newValue}
  page_info$file_id = tools::file_path_sans_ext(basename(x))
  return(page_info)
})

ENTRY_PAGE = do.call("rbind", page_output)
write.csv(ENTRY_PAGE, file.path(OUTPUT.DIR, "ENTRY_PAGE.csv"), row.names = FALSE)

#     PRICE_NAME ----

# see https://github.com/ucd-library/wine-price-extraction/issues/9 for discussion of vars
text_vars_to_include = c("text", "text_raw", "name", "id", "name_id", "file_id") #remember this id is the id in the catalog
wine_vars_to_include = c("country", "year", "color", "variety", "region", "province", "designation")
price_vars_to_include = c("price_raw", "confidence", "type_new","price_new", "cluster", "table","row", "entry_id", "name_id", "text.true", "truth_entered_by", "col.header")

PRICE_NAME = left_join(ENTRY_PRICE[,price_vars_to_include],
                        ENTRY_NAME[,c(text_vars_to_include, wine_vars_to_include)],
                        by = "name_id", suffix = c(".price", ".name"))

names(PRICE_NAME)

write.csv(PRICE_NAME, file.path(OUTPUT.DIR, "PRICE_NAME.csv"), row.names = FALSE)
