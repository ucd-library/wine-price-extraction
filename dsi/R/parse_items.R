
# Parser for wine catalogs, v 0.1
#
# As an input it expects trimmed part of the image containig potential item name
# (just item line - without id, prices, or additional info).
# It will try to extract:
# -wine color
# -vintage (year)
# -country
# -producer
# -region
# -anything in brackets
# -and finally name (what's left)
#
# Parsing steps (and explanation):
# 1. Find and trim ID
# 2. Look for color (don't trim cause it may be part of the name)
# 3. Recognize and trim content in brackets - it usually contains region, producer or other info, e.g. (red dry)
# 4. Look for and trim coma and year - these usually separate name and region / producer
# 5. Check letter case - sometimes name is written in upper-case, while region / producer in lower-case
# 6. If we discovered separate parts analyze them as name or region / producer
# 7. Otherwise analyze whole string to get region / producer (and trim)
# 8. Analyze leftovers as a name.


####################
# TODO:
# 1. refactor - reuse search functions
# 2. "smart" matching using agrep
# 3. build dictionaries from winemag data
#
# -try NLP approach
# -use id to look for similar items
# -parse section titles and item details
# -prices validation by multiplying per bottle price
# -track performance using Levenshtein distance
# -store results in database
####################


# load libraries
library(dplyr)
library(tidyverse)
library(tesseract)
library(RODBC)
library(RecordLinkage)



# create database connection
conn = odbcDriverConnect('driver={SQL Server};server=localhost;database=DataFest;trusted_connection=true')

# get regions from SQL
regions = sqlQuery(conn, paste("SELECT Region, Country FROM Regions", sep=""));

# get producers from SQL
producers = sqlQuery(conn, paste("SELECT Producer, Region, Country FROM Producers", sep=""));


# recognize text from image
eng = tesseract("eng")
item_text = ocr("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0011_1.jpg", engine = eng)
item_text_conf = ocr_data("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0011_1.jpg", engine = eng)
item_text
item_text_conf

################
# START PARSING


# 0. clean item_text
# remove commas, dots, new lines and widow chars
item_text = gsub("[\\.|\n|:|,]", " ", item_text);
# remove redundant spaces
item_text = gsub("\\s+", " ", item_text);
item_text = gsub("^\\s|\\s$", "", item_text);



# 1. get ID - look for a number at the beggining of the text
id = str_extract(item_text, "^\\d+");

# if id not recognized mark as FALSE, else get confidence and trim from text
if (is.na(id)) {
  id = FALSE;
} else {
  id_conf = as.numeric(item_text_conf$confidence[grep(id, item_text_conf$word)]);
  item_text = gsub(id, "", item_text);
}


# 2. get color - look for key-words
color = str_extract(item_text, regex(pattern = "white|blanc|rose|pink|red|rouge", ignore_case = TRUE));

# if color not recognized mark as FALSE, else get confidence
# don't trim color as it may be part of the name
if (is.na(color)) {
  color = FALSE;
} else {
  color_conf = as.numeric(item_text_conf$confidence[grep(color, item_text_conf$word)]);
  color = tolower(color);
  if (color == "blanc" || color == "white") {
    color = "white";
  } else if (color == "rose" || color == "pink") {
    color = "rose";
  } else if (color == "red" || color == "rouge") {
    color = "red";
  }
}


# 3. get anything that is in brackets
brackets = str_extract(item_text, "[\\[\\(].*?[\\]\\)]");

# if brackets not found mark as FALSE, else trim bracket content
if (is.na(brackets)) {
  brackets = FALSE;
  brackets_conf_df = FALSE;
} else {
  brackets_conf_df = subset(item_text_conf, grepl(pattern = paste(strsplit(brackets, " ")[[1]], collapse="|"), word), select = c(1:2));
  item_text = gsub(brackets, "", item_text, fixed = TRUE);
}


# 4. get year - look for number 18xx or 19xx
year = str_extract(item_text, "1[8|9]\\d\\d");

# if year not recognized mark as FALSE, else get confidence and trim from text
if (is.na(year)) {
  year = FALSE;
} else {
    year_conf = as.numeric(item_text_conf$confidence[grep(year, item_text_conf$word)]);
  item_text = gsub(year, "", item_text);
}


# 5. divide text into upper- and lower-case parts
item_text = gsub("^\\s|\\s$", "", item_text);
upper_case_text = str_extract(item_text, "^([A-Z\\s]+[A-Z\\s',.]*[A-Z])\\b");
lower_case_text = str_extract(item_text, "\\b([A-Z]?[a-z\\s',.]+)+$");



# 6. if upper_case_text is longer than 4 assume this is a name
# look for name and region/producer matches in dictionaries, if not found add them
if (length(upper_case_text) > 4) {
  # TODO: check in dictionary, add if not found
  name = upper_case_text;
  
  
  # if lower_case_text is shorter than 4 letter look for region within brackets
  if (length(lower_case_text) > 4) {
    region = str_extract(item_text, regex(pattern = paste(regions$Region, collapse="|"), ignore_case = TRUE));
    country = FALSE;
    
    # if region not found mark as FALSE, else get confidence, trim region, and set country
    if (is.na(region)) {
      region = FALSE;
      region_conf_df = FALSE;
    } else {
      region_conf_df = subset(item_text_conf, grepl(pattern = paste(strsplit(region, " ")[[1]], collapse="|"), word), select = c(1:2));
      item_text = gsub(region, "", item_text);
      country = as.character(regions$Country[which(regions$Region == region)]);
    }
  } else if (brackets) {
    region = str_extract(brackets, regex(pattern = paste(regions$Region, collapse="|"), ignore_case = TRUE));
    country = FALSE;
    
    # if region not found mark as FALSE, else get confidence, trim region, and set country
    if (is.na(region)) {
      region = FALSE;
      region_conf_df = FALSE;
    } else {
      region_conf_df = subset(item_text_conf, grepl(pattern = paste(strsplit(region, " ")[[1]], collapse="|"), word), select = c(1:2));
      country = as.character(regions$Country[which(regions$Region == region)]);
    }
  }
} else {
  # 7. there were no upper letters, look for region/producer and consider leftovers as a name
  # get region
  region = str_extract(item_text, regex(pattern = paste(regions$Region, collapse="|"), ignore_case = TRUE));
  country = FALSE;
  
  # if region not found mark as FALSE, else get confidence, trim region, and set country
  if (is.na(region)) {
    region = FALSE;
    region_conf_df = FALSE;
  } else {
    region_conf_df = subset(item_text_conf, grepl(pattern = paste(strsplit(region, " ")[[1]], collapse="|"), word), select = c(1:2));
    item_text = gsub(region, "", item_text);
    country = as.character(regions$Country[which(regions$Region == region)]);
  }
  
  
  # get producer
  producer = str_extract(item_text, regex(pattern = paste(producers$Producer, collapse="|"), ignore_case = TRUE));
  
  # if producer not found mark as FALSE, else get confidence, trim producer, and set country
  if (is.na(producer)) {
    producer = FALSE;
    producer_conf_df = FALSE;
  } else {
    producer_conf_df = subset(item_text_conf, grepl(pattern = paste(strsplit(producer, " ")[[1]], collapse="|"), word), select = c(1:2));
    item_text = gsub(producer, "", item_text);
    country = as.character(producers$Country[which(producers$Producer == producer)]);
  }
  
  
  #8. consider the rest as a name, clean it
  # remove commas, dots, new lines and widow chars
  name = gsub("[\\.|\n|:|,]", "", item_text);
  # remove redundant spaces
  name = gsub(" +", " ", name);
  name_conf_df = subset(item_text_conf, grepl(pattern = paste(strsplit(name, " ")[[1]], collapse="|"), word), select = c(1:2));
}




id
id_conf
name
name_conf_df
year
year_conf
color
color_conf
region
region_conf_df
producer
producer_conf_df
country
brackets
brackets_conf_df

upper_case_text
lower_case_text






# Function looking for the most similar word / name.
# TODO: javadoc
# make other function that accepts two arrays
# longer strings (3 words) require smaller max.distance, e.g. 0.1
# shorter strings (single word) require greater max.distance, e.g. 0.3

closest_match = function(string_to_match, string_vector){
  # get distances to all values in vector
  distance = levenshteinSim(str1 = tolower(string_to_match), str2 = tolower(string_vector));
  # return best match and it's distance
  return(list("phrase" = string_vector[distance == max(distance)], "distance" = max(distance)));
}

#stringVector = c("CHATEAU D\'ARLAY BLANC", "CHATEAU D\'ARLAY");
#best = closest_match("CHATEAU", stringVector);
#best