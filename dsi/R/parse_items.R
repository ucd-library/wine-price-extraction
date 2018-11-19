#
# Parser approach for wine catalogs, v 0.4
#
# As an input it expects trimmed part of the image containig potential item name
# (just item line - without prices or additional info).
# It will try to extract:
# -id
# -wine color
# -vintage (year)
# -province
# -region
# -anything in brackets
# -and finally name (what's left and written in upper-case)
#
# Parsing steps (and explanation):
# 1. Find and trim ID
# 2. Look for color (don't trim cause it may be part of the name)
# 3. Recognize and trim content in brackets - it usually contains region, producer or other info, e.g. (red dry)
# 4. Look for and trim year
# 5. Divide text into upper- and lower-case parts
# 6. Look for province, region, producer in brackets, upper- and lower-case parts
# 7. Set upper-case part as name???


####################
# TODO:
# -if more than one matches from dictionaries pick the best (look for other info?)
# -try NLP approach
# -use id to look for similar items
# -parse section titles and item details
# -prices validation by multiplying per bottle price
# -track performance (using Levenshtein distance?)
# -store results in database
####################


# load libraries
library(dplyr)
library(tidyverse)
library(tesseract)
library(RODBC)
library(RecordLinkage)


# similarity threshold below which two strings will be considered different
SIMILARITY_THRESHOLD = 0.7;
lockBinding("SIMILARITY_THRESHOLD", globalenv())


# create database connection
conn = odbcDriverConnect('driver={SQL Server};server=localhost;database=DataFest;trusted_connection=true')


# get provinces from SQL
provinces = sqlQuery(conn, paste("SELECT Province FROM kProvinces", sep=""));
# get regions from SQL
regions = sqlQuery(conn, paste("SELECT Region FROM kRegions", sep=""));
# get producers from SQL
producers = sqlQuery(conn, paste("SELECT Producer FROM kProducers", sep=""));


close(conn);




# Look for the most similar string.
# Returns list of matches (one or more) and their similarity with string_to_match 
closest_match = function(string_to_match, string_vector){
  # get similarities to all values in vector
  similarity = levenshteinSim(str1 = tolower(string_to_match), str2 = tolower(string_vector));
  # return best match and it's similarity
  return(list("phrase" = string_vector[similarity == max(similarity)], "similarity" = max(similarity)));
}


# Checks dictionaries for the provided string.
# Returns lists of closest matches.
gather_info = function(string_to_match) {
  # look for province
  best_prov = closest_match(string_to_match, as.character(provinces$Province));
  # look for region
  best_reg = closest_match(string_to_match, as.character(regions$Region));
  # look for producer
  best_prod = closest_match(string_to_match, as.character(producers$Producer));
  return(list("best_province" = best_prov, "best_region" = best_reg, "best_producer" = best_prod));
}


# Picks best value for province / region / producer from brackets / upper_case / lower_case
# Provide category: province / region / producer
# Returns list
pick_best = function(category) {
  max_similarity = 0;
  best_match = NULL;
  # check brackets
  
  if (!is.null(brackets_info)) {
    max_similarity = eval(parse(text = paste("brackets_info$best_", category, "$similarity", sep = "")));
    best_match = eval(parse(text = paste("brackets_info$best_", category, "$phrase", sep = "")));
  }
  # check upper_case
  if (!is.null(upper_case_info)) {
    if (eval(parse(text = paste("upper_case_info$best_", category, "$similarity", sep = ""))) > max_similarity) {
      max_similarity = eval(parse(text = paste("upper_case_info$best_", category, "$similarity", sep = "")));
      best_match = eval(parse(text = paste("upper_case_info$best_", category, "$phrase", sep = "")));
    }
  }
  # check lower_case
  if (!is.null(lower_case_info)) {
    if (eval(parse(text = paste("lower_case_info$best_", category, "$similarity", sep = ""))) > max_similarity) {
      max_similarity = eval(parse(text = paste("lower_case_info$best_", category, "$similarity", sep = "")));
      best_match = eval(parse(text = paste("lower_case_info$best_", category, "$phrase", sep = "")));
    }
  }
  return(list("phrase" = best_match, "similarity" = max_similarity));
}


# Parse item
parse_item = function(item_img_path) {
  
  # item_img_path = "C:\\Users\\ssaganowski\\Desktop\\wines\\items\\3.jpg";
  
  # recognize text from image
  eng = tesseract("eng");
  item_text = ocr(item_img_path, engine = eng);
  item_text_conf = ocr_data(item_img_path, engine = eng);
  print(item_text);
  #item_text_conf
  
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
    id = NULL;
    id_conf = NULL;
  } else {
    id_conf = as.numeric(item_text_conf$confidence[grep(id, item_text_conf$word)]);
    item_text = gsub(id, "", item_text);
  }
  
  
  # 2. get color - look for key-words
  color = str_extract(item_text, regex(pattern = "white|blanc|rose|pink|red|rouge", ignore_case = TRUE));
  
  # if color not recognized mark as FALSE, else get confidence
  # don't trim color as it may be part of the name
  if (is.na(color)) {
    color = NULL;
    color_conf = NULL;
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
    brackets = NULL;
    brackets_conf_df = NULL;
  } else {
    brackets_conf_df = subset(item_text_conf, grepl(pattern = paste(strsplit(brackets, " ")[[1]], collapse="|"), word), select = c(1:2));
    item_text = gsub(brackets, "", item_text, fixed = TRUE);
  }
  
  
  # 4. get year - look for number 18xx or 19xx
  year = str_extract(item_text, "1[8|9]\\d\\d");
  
  # if year not recognized mark as FALSE, else get confidence and trim from text
  if (is.na(year)) {
    year = NULL;
    year_conf = NULL;
  } else {
    year_conf = as.numeric(item_text_conf$confidence[grep(year, item_text_conf$word)]);
    item_text = gsub(year, "", item_text);
  }
  
  
  # 5. divide text into upper- and lower-case parts
  item_text = gsub("^\\s|\\s$", "", item_text);
  upper_case_text = str_extract(item_text, "^([A-Z\\s]+[A-Z\\s',.]*[A-Z])\\b");
  lower_case_text = str_extract(item_text, "\\b([A-Z]?[a-z\\s',.]+)+$");
  
  
  
  # 6. Look for province, region, and producer in brackets, upper- and lower-case parts
  assign("brackets_info", NULL, envir = .GlobalEnv);
  if (!is.null(brackets)) {
    assign("brackets_info", gather_info(substr(brackets, 2, nchar(brackets) - 1)), envir = .GlobalEnv);
  }
  
  assign("upper_case_info", NULL, envir = .GlobalEnv);
  if (!is.na(upper_case_text) && nchar(upper_case_text) > 4) {
    assign("upper_case_info", gather_info(upper_case_text), envir = .GlobalEnv);
  }
  
  assign("lower_case_info", NULL, envir = .GlobalEnv);
  if (!is.na(lower_case_text) && nchar(lower_case_text) > 4) {
    assign("lower_case_info", gather_info(lower_case_text), envir = .GlobalEnv);
  }
  
  # pick best info
  best_province = pick_best("province");
  best_region = pick_best("region");
  best_producer = pick_best("producer");
  
  # set NULL if below 0.6
  if (best_province$similarity < SIMILARITY_THRESHOLD) {
    best_province = NULL;
  }
  if (best_region$similarity < SIMILARITY_THRESHOLD) {
    best_region = NULL;
  }
  if (best_producer$similarity < SIMILARITY_THRESHOLD) {
    best_producer = NULL;
  }
  
  # if all below 0.7 set UPPER CASE as a name  ??
  # 7. Set upper-case part as name???
  name = upper_case_text;
  name_conf_df = subset(item_text_conf, grepl(pattern = paste(strsplit(name, " ")[[1]], collapse="|"), word), select = c(1:2));
  
  
  
  # show what have been found
  show_info = "";
  if (is.null(id)) {
    show_info = paste(show_info, "\n        id:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n        id:  ", id, " (", round(id_conf, 0), "%)", sep = "");
  }
  if (is.null(year)) {
    show_info = paste(show_info, "\n      year:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n      year:  ", year, " (", round(year_conf, 0), "%)", sep = "");
  }
  if (is.null(color)) {
    show_info = paste(show_info, "\n     color:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n     color:  ", color, " (", round(color_conf, 0), "%)", sep = "");
  }
  if (is.null(best_province)) {
    show_info = paste(show_info, "\n  province:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n  province:  ", best_province$phrase[1], " (", round(best_province$similarity, 2), ")", sep = "");
  }
  if (is.null(best_region)) {
    show_info = paste(show_info, "\n    region:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n    region:  ", best_region$phrase[1], " (", round(best_region$similarity, 2), ")", sep = "");
  }
  if (is.null(best_producer)) {
    show_info = paste(show_info, "\n  producer:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n  producer:  ", best_producer$phrase[1], " (", round(best_producer$similarity, 2), ")", sep = "");
  }
  if (is.null(upper_case_text)) {
    show_info = paste(show_info, "\nupper_case:  ", sep = "");
  } else {
    show_info = paste(show_info, "\nupper_case:  ", upper_case_text, sep = "");
  }
  if (is.null(brackets)) {
    show_info = paste(show_info, "\n  brackets:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n  brackets:  ", brackets, " (", round(mean(brackets_conf_df$confidence), 0), "%)", sep = "");
  }
  
  cat(show_info, sep = "");
  
  
  # save to SQL
  
}



parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0011_1.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\2.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\3.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\4.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0036_1.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0036_2.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0036_3.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0036_4.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0036_5.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0295_1.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0237_250.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0237_250b.jpg");
parse_item("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\0237_250c.jpg");



