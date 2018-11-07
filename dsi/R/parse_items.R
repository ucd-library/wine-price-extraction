
# Parser for wine catalogs, v 0.1
#
# As an input it expects trimmed part of the image containig potential item
# (whole item line with id, name, prices, and additional info).
# It will try to extract:
# -wine color
# -vintage (year)
# -country
# -producer
# -region
# -anything in brackets
# -and finally name (what's left)


####################
# TODO:
# 1. add confidence to results
# 2. "smart" matching using agrep
# 3. build region and producers dictionaries (scrap web)
# -parse section titles and item details
# -prices validation by multiplying per bottle price
# -create name dictionaries
# -measure performance with Levenshtein distance
# -store results in database
####################


# load libraries
library(dplyr)
library(tidyverse)
library(tesseract)
library(RODBC)



# create database connection
conn = odbcDriverConnect('driver={SQL Server};server=localhost;database=DataFest;trusted_connection=true')
# get regions from SQL
regions = sqlQuery(conn, paste("SELECT Region, Country FROM Regions", sep=""));


# get producers from SQL
producers = sqlQuery(conn, paste("SELECT Producer, Region, Country FROM Producers", sep=""));


# recognize text from image
eng = tesseract("eng")
item_text = ocr_data("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0036_5.jpg", engine = eng)
item_text


################
# START PARSING


# get year - look for number 18xx or 19xx
year = str_extract(item_text, "1[8|9]\\d\\d");

# if year not recognized mark as FALSE, else trim from text
if (is.na(year)) {
  year = FALSE;
} else {
  item_text = gsub(year, "", item_text);
}


# get color - look for key-words
color = str_extract(item_text, regex(pattern = "white|blanc|rose|pink|red|rouge", ignore_case = TRUE));

# if color not recognized mark as FALSE
# don't trim color as it may be a part of name
if (is.na(color)) {
  color = FALSE;
} else {
  color = tolower(color);
  if (color == "blanc" || color == "white") {
    color = "white";
  } else if (color == "rose" || color == "pink") {
    color = "rose";
  } else if (color == "red" || color == "rouge") {
    color = "red";
  }
}


# get region
region = str_extract(item_text, regex(pattern = paste(regions$Region, collapse="|"), ignore_case = TRUE));

# if region not found mark as FALSE, else trim region and set country
if (is.na(region)) {
  region = FALSE;
} else {
  item_text = gsub(region, "", item_text);
  country = as.character(regions$Country[which(regions$Region == region)]);
}


# get producer
producer = str_extract(item_text, regex(pattern = paste(producers$Producer, collapse="|"), ignore_case = TRUE));

# if producer not found mark as FALSE, else trim producer and set country
if (is.na(producer)) {
  producer = FALSE;
} else {
  item_text = gsub(producer, "", item_text);
  country = as.character(producers$Country[which(producers$Producer == producer)]);
}


# get anything that is in brackets
brackets = str_extract(item_text, "[\\[\\(].*?[\\]\\)]");

# if brackets not found mark as FALSE, else trim bracket content
if (is.na(brackets)) {
  brackets = FALSE;
} else {
  item_text = gsub(brackets, "", item_text, fixed = TRUE);
}


# treat the rest as a name, clean it
# remove commas, dots, new lines and widow chars
name = gsub("[\\.|\n|:|,]", "", item_text);
# remove redundant spaces
name = gsub(" +", " ", name);




name
year
color
region
producer
country
brackets
