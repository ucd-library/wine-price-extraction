# See R/wine_price_tables.R for overview of the workflow and To Do's associated with price_table_extraction function called below
# Jane Carlen
# Notes, assumes .jpg image files

####################################################################################################
# Setup ####
####################################################################################################

# Load packages, source files ----
library(dplyr, warn.conflicts = FALSE)
library(Rtesseract, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(jpeg, warn.conflicts = FALSE)
library(cluster, warn.conflicts = FALSE)
library(changepoint, warn.conflicts = FALSE)
library(RecordLinkage, warn.conflicts = FALSE)
#library(MASS) #<- Should have MASS installed for rlm, but load creates conflict with select

# Source necessary files when running from command line ----
if (length(commandArgs(trailingOnly = TRUE)) >= 1) {
  #https://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
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
  
  source(file.path(thisdir, "../R/wine_price_tables.R"), echo = FALSE)
  source(file.path(thisdir, "../R/wine_price_pageCols.R"), echo = FALSE)
  source(file.path(thisdir, "../R/wine_price_tables_functions.R"), echo = FALSE)
  source(file.path(thisdir, "../R/wine_price_nameBoxes.R"), echo = FALSE)
  source(file.path(thisdir, "../R/helper.R"), echo = FALSE)
}

# Get args (defaults and process from command line) ----

# IF running from this script, set args like...: 
#FILESET = "/Users/janecarlen/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages"
#OUTPUT.DIR = "/Users/janecarlen/Documents/DSI/wine-price-extraction/dsi/Data/price_table_output/"
#DATA.OUTPUT.DIR = "/Users/janecarlen/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed"
#DATA.INPUT.DIR = "/Users/janecarlen/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/fullboxes_deskewed"
#SAVE.DATA = FALSE

#DEFAULTS
OCR.ONLY = FALSE
SAVE.DATA = TRUE
DATA1 = NULL

# For command line args, case doesn't matter (they'll be converted to upper either way)
possible.args = c("FILESET", "OUTPUT.DIR", "DATA.OUTPUT.DIR", "DATA.INPUT.DIR", "OCR.ONLY", "SAVE.DESKEWED", "PIX.THRESHOLD")
args = commandArgs(trailingOnly = TRUE)

# Use command line args if running from terminal:
if (length(args) >= 1) {
  
  argnames = toupper(sapply(args, function(x) strsplit(x, "=")[[1]][1])) # For command line args, case doesn't matter
  argnums = sapply(possible.args, match, argnames)
  argvals = rep(NA, length(possible.args))
  argvals[which(!is.na(argnums))] = 
    sapply(args, function(x) trimws(last(strsplit(x, "=")[[1]])) )[order(argnums[!is.na(argnums)])]
  
  FILESET = argvals[1]
  OUTPUT.DIR = argvals[2]
  DATA.OUTPUT.DIR = argvals[3]
  DATA.INPUT.DIR = argvals[4] 
  OCR.ONLY = as.logical(argvals[5])
  SAVE.DESKEWED = as.logical(argvals[6])
  PIX.THRESHOLD = argvals[7] 
}

print(argvals)

# Arg checks ---- 

# Check if a valid file or folder was given as FILESET, and run file or folder accordingly
# If non, will stop.
if (!exists("FILESET") || is.na(FILESET) || !file.exists (FILESET) ) {
  stop(call.= FALSE, "Path to image file or folder containing images missing or not valid. Stopping.")
} else {
  RUN.FILE = ifelse (grepl(FILESET, pattern = "\\.[a-zA-Z]+$"), TRUE, FALSE) #otherwise run folder
}

# Unless OCR.ONLY is explicitly set to true, set it to false
# If OCR.ONLY is specific but not valid DATA.OUTPUT.DIR was given, stop
if (! exists("OCR.ONLY") || is.na(OCR.ONLY)) {
  OCR.ONLY = FALSE
} else if (OCR.ONLY!= TRUE) {
  OCR.ONLY = FALSE
} else {
  print("OCR.ONLY is set to TRUE")
}

# Check for valid directory to store table extraction output.
# If none, will stop unless OCR.ONLY is true
if ( (!exists("OUTPUT.DIR") || is.na(OUTPUT.DIR) || !file.exists(OUTPUT.DIR)) && !OCR.ONLY) {
  stop("No valid path to store output supplied and OCR.ONLY is false")
}

# Check for valid directory to store data (getBoxes) output.
# If none, data won't be stored. SAVE.DATA will be false.
if ( !exists("DATA.OUTPUT.DIR") || is.na(DATA.OUTPUT.DIR) || !file.exists (DATA.OUTPUT.DIR) ) {
  DATA.OUTPUT.DIR = NULL
  SAVE.DATA = FALSE
  ifelse(OCR.ONLY, stop(call. = FALSE, "OCR.ONLY is set to TRUE but no valid path to store data was supplied."),
         warning("No valid path to store data (output of GetBoxes). Will not store."))
} else {
  # If path to save data output not already given as false, SAVE.DATA -> TRUE
  if (SAVE.DATA != FALSE) {SAVE.DATA = TRUE}
}

# Check for valid directory for input data. If given, function will attempt to use it
if (!exists("DATA.INPUT.DIR") || is.na(DATA.INPUT.DIR) || !file.exists (DATA.INPUT.DIR) ) {
  DATA.INPUT.DIR = NULL
  DATA1 = NULL
  if (!OCR.ONLY) warning("No valid path to existing data (output of GetBoxes) supplied. Will OCR images instead.")
} 

# Unless save.deskewed is explicitly set to true, set it to false
if (!exists("SAVE.DESKEWED") || is.na(SAVE.DESKEWED)) {
  SAVE.DESKEWED = FALSE
} else {
  if (SAVE.DESKEWED!=TRUE) {SAVE.DESKEWED = FALSE}
}

if ( !exists("PIX.THRESHOLD") || is.na(PIX.THRESHOLD) || is.na(as.numeric(PIX.THRESHOLD)) ) {
  PIX.THRESHOLD = NULL
  PIX.NEWVALUE = NULL
} else if (is.numeric(PIX.THRESHOLD) && (PIX.THRESHOLD <=1 || PIX.THRESHOLD > 255)) {
  warning("Invalid PIX.THRESHOLD supplied (outside 1-255 range). Using defaults instead")
  PIX.THRESHOLD = NULL
  PIX.NEWVALUE = NULL
}

####################################################################################################
# RUN ####
####################################################################################################

# 0. Notes on files run ----

# 0551, 3392 0260, 3001 (hard, lots of un-aligned tables), 
# 0208 & 0194 (hard, year columns)
# 3737 ("Savings" columns)
# 4105 (unaligned price in table)
# 1591 (dollar sign used)
# 1544 (repeat col), 
# 2535 (needs new pix.threshold 100?), 0939 (try 200), 1176 (try 150)
# 2504 (many rotated)
# 1452 (hard, lots of text no IDs)
# 0644 (mixed ID types)
# 1835 (before fixing, too few words in name), 0008, 1470
# 0550 two types of IDs on one pages
# 4043, 3737 why binaryThreshold should be 150
# 0644, heavier rotation


# Other possible filesets:

# pages with lots of marks
# marks_top_files = read.csv("wine-price-extraction/dsi/Data/marks_top_files.csv", stringsAsFactors = F)

# pages we have truth for
# truth_files = unique(unlist(sapply(list.files("~/Downloads/price_id_truth_RDS_files", recursive = T), str_extract, pattern =   "UCD_Lehmann_[0-9]{4}"))) #downloaded then deleted
# truth_files = truth_files[-grep(truth_files, pattern = "2759")] #remove bad one
#-- files in truth not in sample folder: fileset = c("UCD_Lehmann_0008", "UCD_Lehmann_0027", "UCD_Lehmann_0267", "UCD_Lehmann_1470  ", "UCD_Lehmann_1994", "UCD_Lehmann_1544", "UCD_Lehmann_1835")

# 1. Run on example -------------------------------------------------------------------------------------------------------------

if (RUN.FILE) {
  
  ### will remove later ###
  #file1 = "UCD_Lehmann_2535"# 0011
  #
  #if (paste0(file1, ".jpg") %in% list.files("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages")) {
  #  file1 = file.path("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages", paste0(file1, ".jpg"))
  #}  else {
  #  sample_files = readRDS("~/Documents/DSI/wine-price-extraction/dsi/Data/sample_files.RDS")
  #  folder = sample_files[which(sample_files$file == file1),"Sample"]
  #  file1 = file.path("~/Documents/DSI/OCR_SherryLehmann/Sample", folder, paste0(file1, ".jpg"))
  #}
  ###
  
  file1 = FILESET
  
  if (!is.null(DATA.INPUT.DIR)) {
    try.data.path = file.path(DATA.INPUT.DIR, paste0(strsplit(basename(file1), "\\.")[[1]][1], "_data1.RDS"))
    if (file.exists(try.data.path)) {
      DATA1 = readRDS(try.data.path)
    } else {DATA1 = NULL}
  } else {DATA1 = NULL}
  
  price_table_extraction(file1,
                         data1 = DATA1,
                         output.dir = OUTPUT.DIR, 
                         data.output.dir = DATA.OUTPUT.DIR,
                         save.data = SAVE.DATA,
                         save.deskewed = SAVE.DESKEWED,
                         pix.threshold = NULL, pix.newValue = NULL, #pix.threshold = 200, pix.newValue = 0
                         binary.threshold = 150,
                         ocr.only = OCR.ONLY,
                         column.header = c("bottle", "case", "quart", "fifth", "half", "of", "24"),
                         res1 = 600,
                         image.check = FALSE, 
                         show.ggplot = TRUE) 
  
}

# 2. Run on fileset ----------------------------------------------------------------------------------------------------------

if (!RUN.FILE) {
  
  fileset = FILESET
  fileset = list.files(FILESET, full.names = T, pattern = "\\.") #only files, not folders
  
  output = lapply(fileset, function(file1) {
    
    if (!is.null(DATA.INPUT.DIR)) {
      try.data.path = file.path(DATA.INPUT.DIR, paste0(strsplit(basename(file1), "\\.")[[1]][1], "_data1.RDS"))
      if (file.exists(try.data.path)) {
        DATA1 = readRDS(try.data.path)
      } else {DATA1 = NULL}
    } else {DATA1 = NULL}
    
    # Decided to move page classification out of here 
    # Use DATA1 to classify if a page (probably) has readable price tables:
    # if (median(DATA1$confidence) > 30 & sum(isPrice(DATA1$text, dollar = FALSE)) >= 3) {
    
    output1 = try({ # names will tell us if it was successful, but output is stored externally
      price_table_extraction(file1, 
                             data1 = DATA1,
                             output.dir = OUTPUT.DIR, 
                             data.output.dir = DATA.OUTPUT.DIR,
                             save.data = SAVE.DATA,
                             save.deskewed = SAVE.DESKEWED,
                             pix.threshold = PIX.THRESHOLD, pix.newValue = PIX.NEWVALUE, #pix.threshold = 200, pix.newValue = 0
                             binary.threshold = 150,
                             ocr.only = OCR.ONLY,
                             column.header = c("bottle", "case", "quart", "fifth", "half", "of", "24"),
                             res1 = 600,
                             image.check = FALSE, 
                             show.ggplot = FALSE)
    }) 
    
    if (class(output1) == "try-error") {
      if (!is.null(DATA1)) {
        output1 = c(output1, "median_conf" = median(DATA1$confidence), "n_price" = sum(isPrice(DATA1$text, dollar = FALSE, maybe = F)))
      }
    }
    
    return(output1)
  })
  
  names(output) = fileset
  
  
}
