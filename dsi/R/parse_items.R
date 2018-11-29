#
# Parser approach for wine catalogs, v 0.5
#
# As an input it expects RDS file with tables containign recognized item names
#
# It will try to extract:
# -id
# -wine color
# -vintage (year)
# -province
# -region
# -producer (winery)
# -designation (the vineyard within the winery where the grapes that made the wine are from)
# -variety (the type of grapes used to make the wine, e.g. Pinot Noir)
# 
#
# Parsing steps:
# 1. Find and trim ID
# 2. Find and trim year
# 3. Store current text as a name (but correct spelling errors if you find them)
# 4. Look for color
# 5. Recognize content in brackets - it usually contains region, producer or other info, e.g. (red dry)
# 6. Divide text into upper- and lower-case parts
# 7. Look for province, region, producer, designation and variety in brackets, upper- and lower-case parts
# in the first iteration look for exact matches (sim = 1), exclude matched category and recognized part from further process
# next look for most similar matches (similarity > 0.9)
# then look for partial matches (substring)


####################
# TODO:
# -make use of "key_words" found
# -if more than one matches from dictionaries pick the best (look for other info?)
# -try to "understand" the pattern - if most of items on this page start with REGION, then give more confidence to REGION dictionary
# -use id to look for similar items
# -track performance (using Levenshtein distance?)
# -store results in database
# -try NLP approach
#
# -parse section titles and item details
# -prices validation by multiplying per bottle price
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
# get designations from SQL
designations = sqlQuery(conn, paste("SELECT Designation FROM kDesignations", sep=""));
# get varieties from SQL
varieties = sqlQuery(conn, paste("SELECT Variety FROM kVarieties", sep=""));


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
# Returns matrix of closest matches.
gather_info = function(string_to_match) {
  # look for province
  best_prov = closest_match(string_to_match, as.character(provinces$Province));
  # look for region
  best_reg = closest_match(string_to_match, as.character(regions$Region));
  # look for producer
  best_prod = closest_match(string_to_match, as.character(producers$Producer));
  # look for designation
  best_desig = closest_match(string_to_match, as.character(designations$Designation));
  # look for variety
  best_var = closest_match(string_to_match, as.character(varieties$Variety));
  
  return(
    rbind(
      "best_province" = best_prov,
      "best_region" = best_reg,
      "best_producer" = best_prod,
      "best_designation" = best_desig,
      "best_variety" = best_var
    )
  );
  
}

# Picks best value for province / region / producer from brackets / upper_case / lower_case
# Provide category: province / region / producer / designation / variety
# Returns list or NULL if not similar enough
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
  
  # return NULL if max_similarity is lower than SIMILARITY_THRESHOLD
  if (max_similarity >= SIMILARITY_THRESHOLD) {
    return(list("phrase" = best_match, "similarity" = max_similarity));
  } else {
    return(NULL);
  }
}


set_flags = function(category, phrase, similarity) {
    switch(category,
         "1" = {
           assign("hit_province", phrase, envir = .GlobalEnv);
           assign("province_sim", similarity, envir = .GlobalEnv);
           },
         "2" = {
           assign("hit_region", phrase, envir = .GlobalEnv);
           assign("region_sim", similarity, envir = .GlobalEnv);
         },
         "3" = {
           assign("hit_producer", phrase, envir = .GlobalEnv);
           assign("producer_sim", similarity, envir = .GlobalEnv);
         },
         "4" = {
           assign("hit_designation", phrase, envir = .GlobalEnv);
           assign("designation_sim", similarity, envir = .GlobalEnv);
         },
         "5" = {
           assign("hit_varirety", phrase, envir = .GlobalEnv);
           assign("variety_sim", similarity, envir = .GlobalEnv);
         });
}

# Parse item
parse_item = function(item_text, item_text_conf) {
  
  #items = readRDS("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_0011.RDS");
  #item_text = paste(items$name.words$table_1[[5]]$text, collapse = " ");
  #item_text_conf = items$name.words$table_1[[5]][,c("text","confidence")];
  #item_text = "237 CHATEAU BELAIR (St. Emillion). ...";
  
  # recognize text from image
  #eng = tesseract("eng");
  #item_text = ocr(item_img_path, engine = eng);
  #item_text_conf = ocr_data(item_img_path, engine = eng);
  cat("\n\n", item_text);
  #item_text_conf;
  
  ################
  # START PARSING
  
  
  # 0. clean item_text
  # remove multiple dots, new lines and widow chars
  item_text = gsub("[\n|:]+", " ", item_text);
  item_text = gsub("\\.{2,}", " ", item_text);
  # remove redundant spaces
  item_text = gsub("\\s+", " ", item_text);
  item_text = gsub("^\\s|\\s$", "", item_text);
  
  
  # 1. Find and trim ID
  # look for a number at the beggining of the text (or number and max 3 characters, e.g. N-4)
  id = str_extract(item_text, "^[0-9]+\\S{0,3}\\b|^\\S{0,3}[0-9]+\\b");
  
  # if id not recognized mark as FALSE, else get confidence and trim from text
  if (is.na(id)) {
    id = NULL;
    id_conf = NULL;
  } else {
    id_conf = as.numeric(item_text_conf$confidence[grep(id, item_text_conf$text)]);
    item_text = gsub(id, "", item_text);
  }
  
  
  # 2. Find and trim year
  # get year - look for number 18xx or 19xx
  year = str_extract(item_text, "1[8|9]\\d\\d");
  
  # if year not recognized mark as FALSE, else get confidence and trim from text
  if (is.na(year)) {
    year = NULL;
    year_conf = NULL;
  } else {
    year_conf = as.numeric(item_text_conf$confidence[grep(year, item_text_conf$text)]);
    item_text = gsub(year, "", item_text);
  }
  
  
  # 3. Store current text as a name (but correct spelling errors if you find them)
  # remove redundant spaces
  item_text = gsub("\\s+", " ", item_text);
  item_text = gsub("^\\s|\\s$", "", item_text);
  name = item_text;
  cat("\n", name);
  
  
  # at that point we have item name, now try to recognize some values from dictionary
  # start with replacing abbreviations (St. -> Saint), this will improve matching with dictionaries
  item_text = gsub("St\\.", "Saint ", item_text, ignore.case = FALSE);
  item_text = gsub("ST\\.", "SAINT ", item_text, ignore.case = FALSE);
  # remove commas and other unneccessary characters
  item_text = gsub("-|_|,|\\.|;|\\?", " ", item_text);
  # remove redundant spaces
  item_text = gsub("\\s+", " ", item_text);
  
  
  # recognize and trim key words like: "estate bottled", "cooperation"
  # put short words within a word boundary, e.g. \bdry\b, so we don't accidentaly trim the middle of some word
  key_words = c("original abfullung", "estate bottled", "estate", "bottled", "cooperation", "cooperative", "co op", "\\bcoop\\b", "\\bdry\\b", "\\brich\\b");
  item_key_words = key_words[!is.na(str_extract(item_text, regex(key_words, ignore_case = TRUE)))];
  item_text = gsub(paste(item_key_words, collapse = "|"), "", item_text, ignore.case = TRUE);
  # remove redundant spaces
  item_text = gsub("\\s+", " ", item_text);
  
  
  # 4. Look for color
  # look for key-words
  color = str_extract(item_text, regex(pattern = "white|blanc|rose|pink|red|rouge", ignore_case = TRUE));
  
  # if color not recognized mark as FALSE, else get confidence
  # don't trim color as it may be part of producer or variety
  if (is.na(color)) {
    color = NULL;
    color_conf = NULL;
  } else {
    color_conf = as.numeric(item_text_conf$confidence[grep(color, item_text_conf$text)]);
    color = tolower(color);
    if (color == "blanc" || color == "white") {
      color = "white";
    } else if (color == "rose" || color == "pink") {
      color = "rose";
    } else if (color == "red" || color == "rouge") {
      color = "red";
    }
  }
  
  
  # 5. get anything that is in brackets
  brackets = str_extract(item_text, "[\\[\\({].*?[\\]\\)}]");
  
  # if brackets not found mark as FALSE, else trim bracket content
  if (is.na(brackets)) {
    brackets = NULL;
    brackets_conf_df = NULL;
  } else {
    brackets_conf_df = subset(item_text_conf, grepl(pattern = paste(strsplit(brackets, " ")[[1]], collapse="|"), text, fixed = TRUE), select = c(1:2));
    item_text = gsub(brackets, "", item_text, fixed = TRUE);
  }
  
  
  # 6. divide text into upper- and lower-case parts
  item_text = gsub("^\\s|\\s$", "", item_text);
  cat("\n", item_text);
  upper_case_text = str_extract(item_text, "[A-Z][A-Z\\s-,.;']*[A-Z]\\b"); #more rigorous pattern: ^([A-Z\\s]+[A-Z\\s',.]*[A-Z])\\b
  lower_case_text = gsub(upper_case_text, "", item_text); #more rigorous pattern: \\b([^A-Z]*[A-Z]{0,2}[^A-Z]+)+$
  # remove redundant spaces
  lower_case_text = gsub("\\s+", " ", lower_case_text);
  lower_case_text = gsub("^\\s|\\s$", "", lower_case_text);
  
  # 7. Look for province, region, producer, designation and variety in brackets, upper- and lower-case parts
  # assign("brackets_info", NULL, envir = .GlobalEnv);
  # if (!is.null(brackets)) {
  #   assign("brackets_info", gather_info(substr(brackets, 2, nchar(brackets) - 1)), envir = .GlobalEnv);
  # }
  # 
  # assign("upper_case_info", NULL, envir = .GlobalEnv);
  # if (!is.na(upper_case_text) && nchar(upper_case_text) > 4) {
  #   assign("upper_case_info", gather_info(upper_case_text), envir = .GlobalEnv);
  # }
  # 
  # assign("lower_case_info", NULL, envir = .GlobalEnv);
  # if (!is.na(lower_case_text) && nchar(lower_case_text) > 4) {
  #   assign("lower_case_info", gather_info(lower_case_text), envir = .GlobalEnv);
  # }

  ############ !!!!!!!!!!!!!!!!!!!!!
  # TODO: refactor code below this point
  
  brackets_mat = gather_info(substr(brackets, 2, nchar(brackets) - 1));
  upper_mat = gather_info(upper_case_text);
  lower_mat = gather_info(lower_case_text);
  
  info_phrase = data.frame(cbind("brackets" = brackets_mat[,1], "upper" = upper_mat[,1], "lower" = lower_mat[,1]));
  info_similarity = data.frame(cbind("brackets" = brackets_mat[,2], "upper" = upper_mat[,2], "lower" = lower_mat[,2]));
  #info_phrase
  #info_similarity
  
  hit_brackets = FALSE;
  hit_upper = FALSE;
  hit_lower = FALSE;
  
  assign("hit_province", NULL, envir = .GlobalEnv);
  assign("hit_region", NULL, envir = .GlobalEnv);
  assign("hit_producer", NULL, envir = .GlobalEnv);
  assign("hit_designation", NULL, envir = .GlobalEnv);
  assign("hit_variety", NULL, envir = .GlobalEnv);
  
  assign("province_sim", NULL, envir = .GlobalEnv);
  assign("region_sim", NULL, envir = .GlobalEnv);
  assign("producer_sim", NULL, envir = .GlobalEnv);
  assign("designation_sim", NULL, envir = .GlobalEnv);
  assign("variety_sim", NULL, envir = .GlobalEnv);

  
  # check if any bracket is 100% match
  if (any(info_similarity$brackets == 1)) {
    # if there is more than one match in column we have duplicates in dictionaries
    if (sum(info_similarity$brackets == 1, na.rm = TRUE) > 1) {
      # TODO warning
    }
    
    #if we have a hit set other values in column and row to 0
    info_similarity$brackets[info_similarity$brackets != 1] = 0
    info_similarity[info_similarity$brackets == 1, names(info_similarity) != "brackets"] = 0;
    
    #store value and set flags
    hit_brackets = TRUE;
    row_id = which(info_similarity$brackets == 1);
    set_flags(row_id, info_phrase$brackets[[row_id]], 1);
  } else {
    #print("nope");
  }
  
  # check if any upper is 100% match
  if (any(info_similarity$upper == 1)) {
    if (sum(info_similarity$upper == 1, na.rm = TRUE) > 1) {
      # TODO warning
    }
    
    #if so set other values in column and row to 0
    info_similarity$upper[info_similarity$upper != 1] = 0;
    info_similarity[info_similarity$upper == 1, names(info_similarity) != "upper"] = 0;
    
    #store value and set flags
    hit_upper = TRUE;
    row_id = which(info_similarity$upper == 1);
    set_flags(row_id, info_phrase$upper[[row_id]], 1);
  } else {
    #print("nope");
  }
  
  # check if any lower is 100% match
  if (any(info_similarity$lower == 1)) {
    if (sum(info_similarity$lower == 1, na.rm = TRUE) > 1) {
      # TODO warning
    }
    
    #if so set other values in column and row to 0
    info_similarity$lower[info_similarity$lower != 1] = 0
    info_similarity[info_similarity$lower == 1, names(info_similarity) != "lower"] = 0;
    
    #store value and set flags
    hit_lower = TRUE;
    row_id = which(info_similarity$lower == 1);
    set_flags(row_id, info_phrase$lower[[row_id]], 1);
  } else {
    #print("nope");
  }
  
  
  #info_similarity;
  
  
  #### second iteration - remove anything below threshold and pick the max value from what's left
  
  info_similarity[info_similarity == 1] = 0;
  info_similarity[info_similarity < SIMILARITY_THRESHOLD] = 0;
  
  #info_similarity;
  
  # TODO: if max(info_similarity$brackets) returns a list, we have a duplication between dictionaries
  
  if (!hit_brackets & any(info_similarity$brackets > SIMILARITY_THRESHOLD)) {
    #store value and set flags
    hit_brackets = TRUE;
    row_id = which.max(info_similarity$brackets);
    set_flags(row_id, info_phrase$brackets[[row_id]], max(unlist(info_similarity$brackets)));
  }
  if (!hit_upper & any(info_similarity$upper > SIMILARITY_THRESHOLD)) {
    #store value and set flags
    hit_upper = TRUE;
    row_id = which.max(info_similarity$upper);
    set_flags(row_id, info_phrase$upper[[row_id]], max(unlist(info_similarity$upper)));
  }
  if (!hit_lower & any(info_similarity$lower > SIMILARITY_THRESHOLD)) {
    #store value and set flags
    hit_lower = TRUE;
    row_id = which.max(info_similarity$lower);
    set_flags(row_id, info_phrase$lower[[row_id]], max(unlist(info_similarity$lower)));
  }
  
  ######## third iteration - look for substrings
  
  
  
  # pick best info
  # best_province = pick_best("province");
  # best_region = pick_best("region");
  # best_producer = pick_best("producer");
  # best_designation = pick_best("designation");
  # best_variety = pick_best("variety");
  

  
  
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
  
  if (is.null(hit_province)) {
    show_info = paste(show_info, "\n  province:  ", sep = ""); # 
  } else {
    show_info = paste(show_info, "\n  province:  ", paste(hit_province, collapse = " / "), " (", round(province_sim, 2), ")", sep = "");
  }
  if (is.null(hit_region)) {
    show_info = paste(show_info, "\n    region:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n    region:  ", paste(hit_region, collapse = " / "), " (", round(region_sim, 2), ")", sep = "");
  }
  if (is.null(hit_producer)) {
    show_info = paste(show_info, "\n  producer:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n  producer:  ", paste(hit_producer, collapse = " / "), " (", round(producer_sim, 2), ")", sep = "");
  }
  if (is.null(hit_designation)) {
    show_info = paste(show_info, "\ndesignation  ", sep = "");
  } else {
    show_info = paste(show_info, "\ndesignation  ", paste(hit_designation, collapse = " / "), " (", round(designation_sim, 2), ")", sep = "");
  }
  if (is.null(hit_variety)) {
    show_info = paste(show_info, "\n   variety:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n   variety:  ", paste(hit_variety, collapse = " / "), " (", round(province_sim, 2), ")", sep = "");
  }
  # if (is.null(best_province)) {
  #   show_info = paste(show_info, "\n  province:  ", sep = "");
  # } else {
  #   show_info = paste(show_info, "\n  province:  ", best_province$phrase[1], " (", round(best_province$similarity, 2), ")", sep = "");
  # }
  # if (is.null(best_region)) {
  #   show_info = paste(show_info, "\n    region:  ", sep = "");
  # } else {
  #   show_info = paste(show_info, "\n    region:  ", best_region$phrase[1], " (", round(best_region$similarity, 2), ")", sep = "");
  # }
  # if (is.null(best_producer)) {
  #   show_info = paste(show_info, "\n  producer:  ", sep = "");
  # } else {
  #   show_info = paste(show_info, "\n  producer:  ", best_producer$phrase[1], " (", round(best_producer$similarity, 2), ")", sep = "");
  # }
  # if (is.null(best_designation)) {
  #   show_info = paste(show_info, "\ndesignation  ", sep = "");
  # } else {
  #   show_info = paste(show_info, "\ndesignation  ", best_designation$phrase[1], " (", round(best_designation$similarity, 2), ")", sep = "");
  # }
  # if (is.null(best_variety)) {
  #   show_info = paste(show_info, "\n   variety:  ", sep = "");
  # } else {
  #   show_info = paste(show_info, "\n   variety:  ", best_variety$phrase[1], " (", round(best_variety$similarity, 2), ")", sep = "");
  # }
  
  
  if (is.null(upper_case_text)) {
    show_info = paste(show_info, "\nupper_case:  ", sep = "");
  } else {
    show_info = paste(show_info, "\nupper_case:  ", upper_case_text, sep = "");
  }
  if (is.null(lower_case_text)) {
    show_info = paste(show_info, "\nlower_case:  ", sep = "");
  } else {
    show_info = paste(show_info, "\nlower_case:  ", lower_case_text, sep = "");
  }
  if (is.null(brackets)) {
    show_info = paste(show_info, "\n  brackets:  ", sep = "");
  } else {
    show_info = paste(show_info, "\n  brackets:  ", brackets, " (", round(mean(brackets_conf_df$confidence), 0), "%)", sep = "");
  }
  show_info = paste(show_info, "\n key_words:  ", paste(item_key_words, collapse = "; "));
  
  cat(show_info, sep = "");
  
  
  # save to SQL
  
}


parse_RDS_items = function(RDS_path) {
  items = readRDS(RDS_path);
  
  for (i in items$name.words) {
    for (j in i) {
      parse_item(paste(j$text, collapse = " "), j[,c("text","confidence")])
    }
  }
}


parse_RDS_items("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_0011.RDS");
parse_RDS_items("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_3392.RDS");
parse_RDS_items("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_1106.RDS");
parse_RDS_items("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_0237.RDS");


