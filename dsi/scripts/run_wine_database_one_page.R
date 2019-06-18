# File to merge price and name output into requested database
#
# TO DO:
#     - we may have an id from name extraction and one from table extraction, check if they align
#     fix name naming so that ENTRY_PRICE and ENTRY_NAME have same number of entries
#     - Add dictionary hit info to NAME_MATCH table
#
# Tables
# ENTRY_PRICE
#   key is entry_id = 0923_1_23 is file number, table number, entry number
# ENTRY_NAME
#   key is name_id = 0923_1_23 is file number, table number, row number
# PRICE_NAME
#   key is entry_id. is a join of ENTRY_PRICE and ENTRY_NAME by name_id with reduced fields.
# NAME_MATCH
#   **key is word_id**, which is entry_id (also in table) with an additional index pasted on
#   has individual word information (possibly multiple lines) for each entry in ENTRY_NAME
# ENTRY_PAGE
#   key is file_id <- change to different ID system?
###################################################################################################

# 0. Setup ----

library(tablewine)
library(stringr)
library(dplyr)

# flip.y argument for back-rotating image points, accounts for plotting images with y = 0 at top left instead of bottom left
FLIP.Y = FALSE
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

parsed_folder = readRDS(IN)

# 3. Current evaluation code (Optional. Will be changed to work on TABLES, not .RDS) ----

#uses OUTPUT.DIR again
OUTPUT.DIR = dirname(IN);

EVAL.INPUT.DIR=OUTPUT.DIR;
EVAL.OUTPUT.DIR=OUTPUT.DIR;
NAME.OUTPUT.DIR=OUTPUT.DIR;
TABLE.OUTPUT.DIR=OUTPUT.DIR;

#     Load NAME data ----
page_results_all = do.call("rbind", parsed_folder$page_results)
dim(page_results_all) # a matrix
#colnames(page_results_all)

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
  lapply(price_RDS_files,function(x) {
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

#check (may be off if different numbers of files)
sum(ENTRY_NAME$name_id != name_output$name_id)

ENTRY_NAME = left_join(ENTRY_NAME,
                       name_output[,c("l", "b", "r", "t","name_center_x_orig", "name_center_y_orig", "name_id")],
                       by = "name_id")


write.csv(ENTRY_NAME, file.path(TABLE.OUTPUT.DIR, "ENTRY_NAME.csv"), row.names = FALSE)

#     Dictionary hit stat summary (optional)

write.csv(name_summary_global_stats(ENTRY_NAME),  file.path(TABLE.OUTPUT.DIR, "name_summary_global_stats.csv"), row.names = TRUE)

#     NAME_MATCH ----
exclude1=c("text_conf","dictionary_hits","dictionary_hits_sim")
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

#     ENTRY_PRICE ----
#     - Initialize table  ----

price_output = lapply(price_RDS_files, function(x) {
  page.cols = readRDS(x)$page.cols
  match.cluster.order = match(sapply(page.cols$prices, function(x) {first(x$cluster)}), page.cols$price_cols$cluster)
  col.header = rep(page.cols$price_cols$col.header[match.cluster.order],
                   times = sapply(page.cols$prices, nrow))
  prices = do.call("rbind", page.cols$prices)

  #add in column variable which was already available in page.cols$price_cols
  prices = left_join(prices, page.cols$price_cols %>% select(c("table", "cluster", "column")), by = c("table", "cluster"))

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

#     - make ENTRY_TRUTH ####

#set above
#TRUTH.DIR = "~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth"
#EVAL.OUTPUT.DIR = NAME.OUTPUT.DIR

ENTRY_TRUTH = read.csv(file.path(TRUTH.DIR, "truth_all.csv"), stringsAsFactors = FALSE)

accurate_file = inner_join(ENTRY_PRICE %>% group_by(file_id) %>% summarize(n_table = n_distinct(table)),
                          ENTRY_TRUTH %>% group_by(file_id) %>% summarize(n_table = n_distinct(table)), by = c("file_id", "n_table")) %>%
                select(file_id)

ENTRY_TRUTH$accurate_file = ENTRY_TRUTH$file_id %in% accurate_file$file_id
#names(ENTRY_TRUTH)
#     - Add truth to table ----
ENTRY_PRICE = left_join(ENTRY_PRICE, ENTRY_TRUTH[ENTRY_TRUTH$accurate_file,c("price_true", "truth_entered_by", "file_id",
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

ENTRY_PRICE = ENTRY_PRICE %>% arrange(file_id, table, column, row)
ENTRY_PRICE$flag_order = order_flag(ENTRY_PRICE, tocheck = "price_new")
# note the ratio flag will flag both entries (e.g. bottle and case) if only one is wrong
# so may want to use ratio flag in combination with order flag
ENTRY_PRICE$flag_ratio = ratio_flag(ENTRY_PRICE)
ENTRY_PRICE$flag_amount = amount_flag(ENTRY_PRICE$price_new)
ENTRY_PRICE$flag_format = format_flag(ENTRY_PRICE$price_new, size_left = 4, size_right = 2)
ENTRY_PRICE$flag_digit = digit_flag(ENTRY_PRICE$price_new, ratio = .04)
# we convert raw years to prices format, so flag if started as year:
ENTRY_PRICE$flag_raw_year = ENTRY_PRICE$type_raw == "year"
ENTRY_PRICE$flag_type_new = ENTRY_PRICE$type_new!="TRUE"
# Sum of flags is a placeholder for class detection until we develop a better model
ENTRY_PRICE$sum_flag = rowSums(ENTRY_PRICE %>% select(contains("flag_")) %>%
                                 select(-"flag_digit"), na.rm = T) #digit has too many false positives

#table(ENTRY_PRICE$sum_flag)
#Check that new prices improve on old
#table(ENTRY_PRICE$type_new, ENTRY_PRICE$type_raw)


#     - Save ----
write.csv(ENTRY_PRICE, file.path(TABLE.OUTPUT.DIR, "ENTRY_PRICE.csv"), row.names = FALSE)

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
write.csv(ENTRY_PAGE, file.path(TABLE.OUTPUT.DIR, "ENTRY_PAGE.csv"), row.names = FALSE)

#     PRICE_NAME ----

# see https://github.com/ucd-library/wine-price-extraction/issues/9 for discussion of vars
text_vars_to_include = c("text_raw", "name", "name_trim", "id", "name_id", "file_id") #remember this id is the id in the catalog
wine_vars_to_include = c("country", "year", "color", "variety", "region", "province", "designation")
price_vars_to_include = c("price_raw", "confidence", "type_new", "price_new", "cluster", "table","row", "column", "entry_id", "name_id", "price_true", "truth_entered_by", "col.header")

PRICE_NAME = left_join(ENTRY_PRICE[,price_vars_to_include],
                        ENTRY_NAME[,c(text_vars_to_include, wine_vars_to_include)],
                        by = "name_id", suffix = c(".price", ".name"))

#names(PRICE_NAME)

write.csv(PRICE_NAME, file.path(TABLE.OUTPUT.DIR, "PRICE_NAME.csv"), row.names = FALSE)
