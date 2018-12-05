#
# Parser approach for wine catalogs, v 0.6
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
# It will also compute some statistics on how many matches with dictionaries were full matches, how many items didn't have any match, etc.
#
# Parsing steps:
# 1. Clean text from dots and unwanted characters
# 2. Find and trim ID
# 3. Find and trim year
# 4. Store current text as a name
# 5. Clean text more aggressively to improve dictionary matching, e.g. replace "St." with "Saint", remove dots, commas, etc.
# 6. Find and trim keywords, e.g. estate bottled
# 7. Look for color
# 8. Recognize content in brackets - it usually contains region, producer or other info, e.g. (red dry)
# 9. Divide text into upper- and lower-case parts
# 10. Look for province, region, producer, designation and variety in brackets, upper- and lower-case parts
#       -in the first iteration look for exact matches (similarity = 1), exclude matched category and recognized part from further process
#       -next look for most similar matches (similarity > 0.8)
#       -then look for partial matches (substring)
# 11. Assign country based on province / region / producer


####################
# TODO:
# -make use of "key_words" found
# -save output to file
# -compute more stats
# -make use of stats (patterns)
# -compute global stats (for all pages)
# -use id to look for similar items
# -store results in database
# -try NLP approach
#
# -parse section titles and item details
# -prices validation by multiplying per bottle price
####################


# load libraries
library(dplyr)
library(tidyverse)
library(RODBC)
library(RecordLinkage)


# similarity threshold above which we consider two strings as potential match
SIMILARITY_THRESHOLD = 0.8;
lockBinding("SIMILARITY_THRESHOLD", globalenv())


# create database connection
conn = odbcDriverConnect('driver={SQL Server};server=localhost;database=DataFest;trusted_connection=true')


# get provinces from SQL
provinces = sqlQuery(conn, paste("SELECT Province, Country FROM kProvinces", sep=""));
# get regions from SQL
regions = sqlQuery(conn, paste("SELECT Region, Country FROM kRegions", sep=""));
# get producers from SQL
producers = sqlQuery(conn, paste("SELECT Producer, Country FROM kProducers", sep=""));
# get designations from SQL
designations = sqlQuery(conn, paste("SELECT Designation FROM kDesignations", sep=""));
# get varieties from SQL
varieties = sqlQuery(conn, paste("SELECT Variety FROM kVarieties", sep=""));


close(conn);



# Initializes result object. 
# Returns empty result object.
init_result = function() {
  result = list(
    
    text = NULL,            # original text as recognized by tesseract
    text_conf = NULL,       # tesseract confidence of recognizing particular parts of text
    
    # extracted from the original text:
    name = NULL,            # cleaned original text without year and id, e.g. "CHATEAU LYNCH BAGES (Pauillac)"
    keywords = NULL,        # keywords, e.g. "estate bottled"
    upper_text = NULL,      # cleaned upper case text
    lower_text = NULL,      # cleaned lower case text
    brackets_text = NULL,   # brackets content, e.g. "(red wine)"
    dictionary_hits = NULL, # matrix of dictionary matches (all dictionaries vs upper, lower,bracket texts)
    
    # item attributes:
    id = NULL,              # wine id, e.g. 246 or N-4
    year = NULL,            # wine vintage, e.g. 1957
    color = NULL,           # wine color: white / red / rose
    province = NULL,        # corresponds to provinces dictionary, province or state that the wine is from, e.g. Bordeaux
    region = NULL,          # corresponds to regions dictionary, wine growing area in a province or state, e.g. Margaux
    producer = NULL,        # corresponds to producers dictionary, winery that made the wine, e.g. Chateau Margaux
    designation = NULL,     # corresponds to designations dictionary, vineyard within the producer where the grapes that made the wine are from, e.g. Pavillon Blanc de Chateau Margaux
    variety = NULL,         # corresponds to varieties dictionary, type of grapes used to make the wine, e.g. Sauvignon Blanc
    country = NULL,         # country associated with province, region, producer (value from dictionary)
    
    # fields' confidences:
    id_conf = NULL,         # tesseract confidence
    year_conf = NULL,       # tesseract confidence
    color_conf = NULL,      # tesseract confidence
    province_sim = NULL,    # Levenshtein similarity with a value from dictionary
    region_sim = NULL,      # Levenshtein similarity with a value from dictionary
    producer_sim = NULL,    # Levenshtein similarity with a value from dictionary
    designation_sim = NULL, # Levenshtein similarity with a value from dictionary
    variety_sim = NULL,     # Levenshtein similarity with a value from dictionary
    brackets_conf = NULL,   # tesseract confidence, data frame
    dictionary_hits_sim = NULL, # matrix of similarity to dictionary matches 
    
    # additional fields:
    upper_text_hit = NULL,    # flag telling if this part of text had a match in dictionary
    lower_text_hit = NULL,    # flag telling if this part of text had a match in dictionary
    brackets_text_hit = NULL, # flag telling if this part of text had a match in dictionary
    file_name = NULL,         # page being processed, e.g. UCD_Lehmann_0011
    confidence = NULL,        # how reliable results are: 100% means all hits were in the dictionaries
    inspect = NULL            # tells which fields should be inspected (cause they were not a full hit or had other issues)
  );
  
  return(result);
}


# Removes redundant spaces.
remove_spaces = function(text) {
  # replace multiple spaces with a single space
  text = gsub("\\s+", " ", text);
  # remove space at the begining and at the end
  text = gsub("^\\s|\\s$", "", text);
  
  return(text)
}


# Cleans original item text.
clean_text = function(text) {
  # remove new lines and some special characters
  text = gsub("[\n:@^*]+", " ", text);
  # remove multiple dots
  text = gsub("(\\s\\.|\\.){2,}.*", " ", text);
  # remove redundant spaces
  text = remove_spaces(text);
  
  return(text);
}


# Looks for and trims id.
# TODO: handle more than one number in text
find_id = function(result_object) {
  # look for a number at the beggining of the text (or number and max 3 characters, e.g. N-4)
  id = str_extract(result_object$text, "^[0-9]+\\S{0,3}\\b|^\\S{0,3}[0-9]+\\b");
  
  # if found trim from text and look for confidence
  if (!is.na(id)) {
    result_object$id = id;
    result_object$id_conf = as.numeric(result_object$text_conf$confidence[grep(id, result_object$text_conf$text)]);
    result_object$text = gsub(id, "", result_object$text);
  }
  
  return(result_object);
}


# Looks for and trims year.
find_year = function(result_object) {
  # look for number 18xx or 19xx
  year = str_extract(result_object$text, "1[8|9]\\d\\d");
  
  # if found trim from text and look for confidence
  if (!is.na(year)) {
    result_object$year = year;
    result_object$year_conf = as.numeric(result_object$text_conf$confidence[grep(year, result_object$text_conf$text)]);
    result_object$text = gsub(year, "", result_object$text);
  }
  
  return(result_object);
}


# Cleans item text more aggressively.
clean_text_aggressive = function(text) {
  # start with replacing abbreviations (St. -> Saint), this will improve matching with dictionaries
  text = gsub("St\\.", "Saint ", text, ignore.case = FALSE);
  text = gsub("ST\\.", "SAINT ", text, ignore.case = FALSE);
  # remove dots, commas and other unneccessary characters
  text = gsub("[-_,.;?]+", " ", text);
  # remove redundant spaces
  text = remove_spaces(text);
  
  return(text);
}


# Looks for and trims keywords.
# TODO: make use of keywords (e.g. estate bottled)
find_keywords = function(result_object) {
  text = result_object$text;
  # put short words within a word boundary, e.g. \bdry\b, so we don't accidentaly trim the middle of some word
  keywords = c("original abfullung", "estate bottled", "estate", "bottled", "cooperation", "cooperative", "co op", "\\bcoop\\b", "\\bdry\\b", "rich");
  matched_keywords = keywords[!is.na(str_extract(text, regex(keywords, ignore_case = TRUE)))];
  # trim keywords from text
  text = gsub(paste(matched_keywords, collapse = "|"), "", text, ignore.case = TRUE);
  text = remove_spaces(text);
  
  result_object$text = text;
  result_object$keywords = matched_keywords;
  
  return(result_object);
}


# Looks for color.
# Don't trim color as it may be part of producer or variety.
find_color = function(result_object) {
  # look for color keywords
  color = str_extract(result_object$text, regex(pattern = "white|blanc|rose|pink|red|rouge", ignore_case = TRUE));
  
  # if found look for confidence
  if (!is.na(color)) {
    result_object$color_conf = as.numeric(result_object$text_conf$confidence[grep(color, result_object$text_conf$text)]);
    color = tolower(color);
    if (color == "blanc" || color == "white") {
      color = "white";
    } else if (color == "rose" || color == "pink") {
      color = "rose";
    } else if (color == "red" || color == "rouge") {
      color = "red";
    }
    result_object$color = color;
  }
  
  return(result_object);
}


# Looks for and trims brackets content.
find_brackets = function(result_object) {
  
  brackets = str_extract(result_object$text, "[\\[\\({].*?[\\]\\)}]");
  
  # if found trim from text and look for confidence
  if (!is.na(brackets)) {
    result_object$brackets_text = substr(brackets, 2, nchar(brackets) - 1);
    result_object$brackets_conf = subset(result_object$text_conf, grepl(pattern = paste(strsplit(brackets, " ")[[1]], collapse="|"), text, fixed = TRUE), select = c(1:2));
    text = gsub(brackets, "", result_object$text, fixed = TRUE);
    text = remove_spaces(text);
    result_object$text = text;
  }
    
  return(result_object);
}


# Divides text into upper- and lower-case text.
divide_upper_lower = function(result_object) {
  
  upper_text = str_extract(result_object$text, "[A-Z][A-Z\\s-,.;']*[A-Z]\\b"); #more rigorous pattern: ^([A-Z\\s]+[A-Z\\s',.]*[A-Z])\\b
  lower_text = gsub(upper_text, "", result_object$text); #more rigorous pattern: \\b([^A-Z]*[A-Z]{0,2}[^A-Z]+)+$
  
  # check if longer than 4 chars
  if (!is.na(upper_text) & nchar(upper_text) > 4) {
    result_object$upper_text = upper_text;
  }
  if (!is.na(lower_text) & nchar(lower_text) > 4) {
    lower_text = remove_spaces(lower_text);
    result_object$lower_text = lower_text;
  }
  
  return(result_object);
}


# Look for the most similar string.
# Returns list of matches (one or more) and their similarity with string_to_match 
closest_match = function(string_to_match, string_vector){
  # get similarities to all values in vector
  similarity = levenshteinSim(str1 = tolower(string_to_match), str2 = tolower(string_vector));
  # return best match and it's similarity
  return(list("phrase" = string_vector[similarity == max(similarity)], "similarity" = max(similarity)));
}


# Finds the most similar values in  all dictionaries to the provided text.
# Returns matrix of closest matches.
dictionary_matches = function(string_to_match) {
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
      "province" = best_prov,
      "region" = best_reg,
      "producer" = best_prod,
      "designation" = best_desig,
      "variety" = best_var
    )
  );
}


# Looks for full matches (similarity = 1) only among all matches.
# Modifies matrix for further searches.
find_full_matches = function(result_object) {
  
  # helper object for shorter code
  hits_sim = result_object$dictionary_hits_sim;

  # go through all columns (parts) separately
  for (part in colnames(hits_sim)) {
    # check if there is any full match
    if (any(hits_sim[[part]] == 1)) {
      # if there is more than one match in column we have duplicates in dictionaries
      if (sum(hits_sim[[part]] == 1, na.rm = TRUE) > 1) {
        # add warning to 'inspect' field
        result_object$inspect = paste0(result_object$inspect, "duplication in dictionaries; ");
      }
      
      # if we have a full match set other values in column and row to 0
      hits_sim[[part]][hits_sim[[part]] != 1] = 0;
      hits_sim[hits_sim[[part]] == 1, names(hits_sim) != part] = 0;
      
      # store values in result_object
      row_id = which(hits_sim[[part]] == 1);
      row_name = rownames(hits_sim)[row_id];
      # set attribute
      result_object[[row_name]] = result_object$dictionary_hits[[part]][row_id];
      # and its similarity
      result_object[[paste0(row_name, "_sim")]] = 1;
      # keep note that this part was a full match (for further iterations and statistics)
      result_object[[paste0(part, "_hit")]] = row_name;
    }
  };
  
  # remove full matches from similarity matrix for further searches
  hits_sim[hits_sim == 1] = 0;

  # don't forget it was only a helper object
  result_object$dictionary_hits_sim = hits_sim;
  
  return(result_object);
}


# Looks for very probable matches (similarity > SIMILARITY_THRESHOLD).
find_decent_matches = function(result_object) {
  
  # helper object for shorter code
  hits_sim = result_object$dictionary_hits_sim;
  
  # go through all columns (parts) separately
  for (part in colnames(hits_sim)) {
    # check if this part wasn't a full match and if there is any decent match
    if (is.null(result_object[[paste0(part, "_hit")]]) & any(hits_sim[[part]] > SIMILARITY_THRESHOLD)) {

      # store values in result_object
      row_id = which.max(hits_sim[[part]]);
      row_name = rownames(hits_sim)[row_id];
      # set attribute
      result_object[[row_name]] = result_object$dictionary_hits[[part]][row_id];
      # and its similarity
      result_object[[paste0(row_name, "_sim")]] = max(unlist(hits_sim[[part]]));
      # keep note that this part was a match (for further iterations and statistics)
      result_object[[paste0(part, "_hit")]] = row_name;
      # append attribute to 'inspect' field
      result_object$inspect = paste0(result_object$inspect, paste0(row_name, " (decent ", part,"); "));
    }
  };
  
  return(result_object);
}


# Looks for submatches of particular text part within particular dictionary.
dictionary_submatches = function(result_object, text_part, attribute, dictionary) {
  
  # consider only attributes that didn't have a hit
  if (is.null(result_object[[attribute]])) {
    # get all matching substrings
    submatches = na.omit(str_extract(result_object[[text_part]], regex(as.character(dictionary), ignore_case = TRUE, fixed = TRUE)));
    if (length(submatches) > 0) {
      # pick the longest one
      submatch = submatches[nchar(submatches) == max(nchar(submatches))];
      # store it as in dictionary (letter case matters later)
      submatch = as.character(dictionary[tolower(dictionary) == tolower(submatch)]);
      
      #cat("\n", attribute, ": ", paste0(submatch, collapse = " / "));
      
      # store values in result_object
      # set attribute
      result_object[[attribute]] = submatch;
      # and its similarity
      result_object[[paste0(attribute, "_sim")]] = levenshteinSim(str1 = tolower(submatch), str2 = tolower(result_object[[text_part]]));
      # keep note that this part was a match (for statistics)
      result_object[[paste0(text_part, "_hit")]] =  attribute;
      # append attribute to 'inspect' field
      result_object$inspect = paste0(result_object$inspect, paste0(attribute, " (submatch ", text_part,"); "));
    }
  }
  return(result_object);
}


# Looks for sub-matches. For each text part try every entry from each dictionary as a substring.
find_submatches = function(result_object) {
  
  # go through all columns (parts) separately
  for (part in colnames(result_object$dictionary_hits_sim)) {
    # check if this part wasn't a full match and if it's not null
    if (is.null(result_object[[paste0(part, "_hit")]]) & !is.null(result_object[[part]])) {
      
      #cat("\ntext: ", result_object[[part]]);
      
      # go over all attributes (that have a dictionary)
      result_object = dictionary_submatches(result_object, part, "province", provinces$Province);
      result_object = dictionary_submatches(result_object, part, "region", regions$Region);
      result_object = dictionary_submatches(result_object, part, "producer", producers$Producer);
      result_object = dictionary_submatches(result_object, part, "designation", designations$Designation);
      result_object = dictionary_submatches(result_object, part, "variety", varieties$Variety);
    }
  };
  
  return(result_object);
}


# Find best matches in dictionaries for all text parts (upper, lower, brackets).
check_dictionaries = function(result_object) {
  
  # create similarity matrix (all dictionaries vs all considered texts)
  result_object$dictionary_hits = as.data.frame(matrix(character(0), nrow = 5, ncol = 3,
                              dimnames = list(c("province","region","producer", "designation", "variety"),
                                              c("upper_text","lower_text","brackets_text"))));
  
  result_object$dictionary_hits_sim = as.data.frame(matrix(0, nrow = 5, ncol = 3,
                                  dimnames = list(c("province","region","producer", "designation", "variety"),
                                                  c("upper_text","lower_text","brackets_text"))));
  
  # update matrix only if text part is not null
  if (!is.null(result_object$upper_text)) {
    upper_mat = dictionary_matches(result_object$upper_text);
    result_object$dictionary_hits$upper_text = upper_mat[,1];
    result_object$dictionary_hits_sim$upper_text = upper_mat[,2];
  }
  if (!is.null(result_object$lower_text)) {
    lower_mat = dictionary_matches(result_object$lower_text);
    result_object$dictionary_hits$lower_text = lower_mat[,1];
    result_object$dictionary_hits_sim$lower_text = lower_mat[,2];
  }
  if (!is.null(result_object$brackets_text)) {
    brackets_mat = dictionary_matches(result_object$brackets_text);
    result_object$dictionary_hits$brackets_text = brackets_mat[,1];
    result_object$dictionary_hits_sim$brackets_text = brackets_mat[,2];
  }
  
  # look for full matches
  result_object = find_full_matches(result_object);
  
  # look for decent matches
  result_object = find_decent_matches(result_object);
  
  # look for sub-matches
  result_object = find_submatches(result_object);
  
  return(result_object);
}


# Assigns country based on province / region / producer.
assign_country = function(result_object) {
  
  # create vector for countries
  countries_found = character(0);
  
  # if attributes are not null look for country in the dictionary
  if (!is.null(result_object$province)) {
    country_prov = as.character(provinces$Country[which(provinces$Province == result_object$province)]);
    # add country to vector
    # FIXME: for some reason a great number of white spaces is present at the end of the country. problem might be in the way dictonaries are created or in data set
    countries_found = append(countries_found, trimws(country_prov));
  }
  if (!is.null(result_object$region)) {
    country_reg = as.character(regions$Country[which(regions$Region == result_object$region)]);
    countries_found = append(countries_found, trimws(country_reg));
  }
  if (!is.null(result_object$producer)) {
    country_prod = as.character(producers$Country[which(producers$Producer == result_object$producer)]);
    countries_found = append(countries_found, trimws(country_prod));
  }
  
  # remove empty values (dictionaries may have missing values)
  if (length(countries_found) > 0) {
    countries_found = Filter(length, countries_found);
    
    # check if all values are the same
    if (length(unique(countries_found)) == 1) {
      # store value
      result_object$country = countries_found[1];
    } else if (length(unique(countries_found)) > 1){
      # append info to 'inspect' field
      result_object$inspect = paste0(result_object$inspect, "multiple countries (", paste(countries_found, collapse = " / "), ");");
    }
  }
  
  
  return(result_object);
}


# Returns well formated text, ready to be printed or stored in output file.
format_result = function(result) {
  
  text = "";
  
  text = paste0(text, "\n\n    raw text:  ", paste(result$text_conf$text, collapse = " "));
  if (is.null(result$name)) {
    text = paste0(text, "\n        name:  ");
  } else {
    text = paste0(text, "\n        name:  ", result$name);
  }
  text = paste0(text, "\n  clean text:  ", result$text);
  
  if (is.null(result$upper_text)) {
    text = paste0(text, "\n  upper_text:  ");
  } else {
    text = paste0(text, "\n  upper_text:  ", result$upper_text);
  }
  if (is.null(result$lower_text)) {
    text = paste0(text, "\n  lower_text:  ");
  } else {
    text = paste0(text, "\n  lower_text:  ", result$lower_text);
  }
  if (is.null(result$brackets_text)) {
    text = paste0(text, "\n    brackets:  ");
  } else {
    text = paste0(text, "\n    brackets:  ", result$brackets_text, " (", round(mean(result$brackets_conf$confidence), 0), "%)");
  }
  if (is.null(result$keywords)) {
    text = paste0(text, "\n    keywords:  ");
  } else {
    text = paste0(text, "\n    keywords:  ", paste(result$keywords, collapse = " / "));
  }
  
  ###################
  ###
  ### ATTRIBUTES
  ###
  ###################
  
  if (is.null(result$id)) {
    text = paste0(text, "\n          id:  ");
  } else {
    text = paste0(text, "\n          id:  ", result$id, " (", round(result$id_conf, 0), "%)");
  }
  if (is.null(result$year)) {
    text = paste0(text, "\n        year:  ");
  } else {
    text = paste0(text, "\n        year:  ", result$year, " (", round(result$year_conf, 0), "%)");
  }
  if (is.null(result$color)) {
    text = paste0(text, "\n       color:  ");
  } else {
    text = paste0(text, "\n       color:  ", result$color, " (", round(result$color_conf, 0), "%)");
  }
  if (is.null(result$province)) {
    text = paste0(text, "\n    province:  "); # 
  } else {
    text = paste0(text, "\n    province:  ", paste(result$province, collapse = " / "), " (", round(result$province_sim, 2), ")");
  }
  if (is.null(result$region)) {
    text = paste0(text, "\n      region:  ");
  } else {
    text = paste0(text, "\n      region:  ", paste(result$region, collapse = " / "), " (", round(result$region_sim, 2), ")");
  }
  if (is.null(result$producer)) {
    text = paste0(text, "\n    producer:  ");
  } else {
    text = paste0(text, "\n    producer:  ", paste(result$producer, collapse = " / "), " (", round(result$producer_sim, 2), ")");
  }
  if (is.null(result$designation)) {
    text = paste0(text, "\n designation:  ");
  } else {
    text = paste0(text, "\n designation:  ", paste(result$designation, collapse = " / "), " (", round(result$designation_sim, 2), ")");
  }
  if (is.null(result$variety)) {
    text = paste0(text, "\n     variety:  ");
  } else {
    text = paste0(text, "\n     variety:  ", paste(result$variety, collapse = " / "), " (", round(result$variety_sim, 2), ")");
  }
  if (is.null(result$country)) {
    text = paste0(text, "\n     country:  ");
  } else {
    text = paste0(text, "\n     country:  ", result$country);
  }
  
  if (is.null(result$inspect)) {
    text = paste0(text, "\n     inspect:  ");
  } else {
    text = paste0(text, "\n     inspect:  ", result$inspect);
  }
  
  return(text);
}


# Parses single item
# Gets items$name.words$table_X. Gets only column text and confidence.
# Returns result object.
parse_item = function(item_text_conf) {
  
  
  #items = readRDS("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_0011.RDS");
  #item_text = paste(items$name.words$table_1[[5]]$text, collapse = " ");
  #item_text_conf = items$name.words$table_1[[3]][,c("text","confidence")];
  
  
  # init result object
  result = init_result();
  result$text_conf = item_text_conf;
  
  # collate text
  result$text = paste(item_text_conf$text, collapse = " ");
  #cat("\n\n", result$text);
  #item_text_conf;
  
  # 1. Clean text
  result$text = clean_text(result$text);
  
  # 2. Find and trim ID
  result = find_id(result);
  
  # 3. Find and trim year
  result = find_year(result);
  
  # 4. Store current text as a name
  # TODO: correct spelling errors (using dictionaries?)
  result$name = remove_spaces(result$text);
  
  # At that point we have item name, now try to recognize some values from dictionary
  
  # 5. Clean text more aggressively
  result$text = clean_text_aggressive(result$text);
  
  # 6. Recognize and trim key words like: "estate bottled", "cooperation"
  result = find_keywords(result);
  
  # 7. Find color
  result = find_color(result);
  
  # 8. Find and trim brackets
  result = find_brackets(result);
  
  # 9. divide text into upper- and lower-case parts
  result = divide_upper_lower(result);
  
  # 10. Identify entities using dictionaries
  result = check_dictionaries(result);
  
  # 11. assign country based on province / region / producer
  result = assign_country(result);
  
  
  return(result);
}


# Initializes page_stats object. 
# Returns empty page stats object.
init_page_stats = function() {
  page_stats = list(
    
    # general
    items = 0,          # number of items in RDS file
    dictionary_hits = 0,# total number of dictionary hits
    items_no_hits = 0,  # number of items that didn't have any hit in dictionaries (full or decent)
    items_some_hits = 0,# number of items that had at least one hit in dictionary (full or decent)
    inspect = 0,        # how many items have to be inspected
    
    # number of non-dictionary attributes found (non-null attributes in result_object)
    id = 0,
    year = 0,
    color = 0,
    country = 0,
    
    # top
    top_countries = character(0), # list of countries found within this page
    
    # number of non-empty parts of text
    upper_text = 0,
    lower_text = 0,
    brackets_text = 0,
    
    # number of particular types of matches in dictionary (full, decent, sub)
    matches_matrix = matrix(0, nrow = 5, ncol = 3, dimnames = list(c("province","region","producer", "designation", "variety"),
                                                                c("full","decent","sub"))),

    # number of particular attirbutes in particular part of text, e.g. how many regions were found in upper_text part
    hits_matrix = matrix(0, nrow = 5, ncol = 3, dimnames = list(c("province","region","producer", "designation", "variety"),
                                                                c("upper_text","lower_text","brackets_text")))
  );
  
  return(page_stats);
}


# Helper function to compute stats. Tells which match occured: 1-full, 2-decent, 3-submatch.
type_of_match = function(similarity) {
  if (similarity == 1) {
    return(1);
  } else if (similarity > SIMILARITY_THRESHOLD) {
    return(2);
  } else {
    return(3);
  }
}


# Computes stats for a single page. Uses result_object.
compute_page_stats = function(stats, result) {
  
  # count non-dictionary fields
  if (!is.null(result$id)) {
    stats$id = stats$id + 1;
  }
  if (!is.null(result$year)) {
    stats$year = stats$year + 1;
  }
  if (!is.null(result$color)) {
    stats$color = stats$color + 1;
  }
  if (!is.null(result$country)) {
    stats$country = stats$country + 1;
    # add to list
    stats$top_countries = append(stats$top_countries, result$country);
  }
  
  
  # count types of matches
  if (!is.null(result$province)) {
    match_type = type_of_match(result$province_sim);
    stats$matches_matrix["province", match_type] = stats$matches_matrix["province", match_type] + 1;
  }
  if (!is.null(result$region)) {
    match_type = type_of_match(result$region_sim);
    stats$matches_matrix["region", match_type] = stats$matches_matrix["region", match_type] + 1;
  }
  if (!is.null(result$producer)) {
    match_type = type_of_match(result$producer_sim);
    stats$matches_matrix["producer", match_type] = stats$matches_matrix["producer", match_type] + 1;
  }
  if (!is.null(result$designation)) {
    match_type = type_of_match(result$designation_sim);
    stats$matches_matrix["designation", match_type] = stats$matches_matrix["designation", match_type] + 1;
  }
  if (!is.null(result$variety)) {
    match_type = type_of_match(result$variety_sim);
    stats$matches_matrix["variety", match_type] = stats$matches_matrix["variety", match_type] + 1;
  }
  
  
  # count hits in particular text types
  # FIXME: when text part produces more than one hit, we count it only once (this happens in sub-matching)
  if (!is.null(result$upper_text_hit)) {
    stats$hits_matrix[result$upper_text_hit, 1] = stats$hits_matrix[result$upper_text_hit, 1] + 1;
  }
  if (!is.null(result$lower_text_hit)) {
    stats$hits_matrix[result$lower_text_hit, 2] = stats$hits_matrix[result$lower_text_hit, 2] + 1;
  }
  if (!is.null(result$brackets_text_hit)) {
    stats$hits_matrix[result$brackets_text_hit, 3] = stats$hits_matrix[result$brackets_text_hit, 3] + 1;
  }
  

  # total dictionary hits
  stats$dictionary_hits = sum(stats$matches_matrix);
  # inspect
  if (!is.null(result$inspect)) {
    stats$inspect = stats$inspect + 1;
  }
  
  return(stats);
}


# Returns well formated text, ready to be printed or stored in output file.
format_page_stats = function(stats) {
  
  text = "\n";
  
  text = paste0(text, "\n          items:  ", stats$items);
  text = paste0(text, "\n        inspect:  ", stats$inspect, " (", round((stats$inspect / stats$items) * 100, 0), "%)");
  text = paste0(text, "\n     total hits:  ", stats$dictionary_hits, " (", round(stats$dictionary_hits / stats$items, 2), " per item)");
  text = paste0(text, "\n     upper hits:  ", sum(stats$hits_matrix[,1]));
  text = paste0(text, "\n     lower hits:  ", sum(stats$hits_matrix[,2]));
  text = paste0(text, "\n   bracket hits:  ", sum(stats$hits_matrix[,3]));
  text = paste0(text, "\n      ids found:  ", stats$id);
  text = paste0(text, "\n   colors found:  ", stats$color);
  text = paste0(text, "\n    years found:  ", stats$year);
  text = paste0(text, "\ncountries found:  ", stats$country);
  text = paste0(text, "\n      countries:  ", paste(stats$top_countries, collapse = " "));
  text = paste0(text, "\n   hits matrix:\n");
  
  
  return(text);
}


# Parse given page.
# As an input requires Jane's RDS file.
parse_RDS_items = function(RDS_path) {
  
  # init stats object
  page_stats = init_page_stats();
  
  # read items
  items = readRDS(RDS_path);
  
  # iterate over columns
  for (i in items$name.words) {
    
    # update page stats (number of items)
    page_stats$items = page_stats$items + length(i);
    
    # iterate over each item
    for (j in i) {
      # parse single item
      item_result = parse_item(j[,c("text","confidence")]);
      
      # print results
      cat(format_result(item_result));
      
      # store results in db ??
      
      # update page stats
      page_stats = compute_page_stats(page_stats, item_result);
    }
  }
  
  # show stats
  cat(format_page_stats(page_stats));
  prmatrix(page_stats$hits_matrix); #TODO: include that in format_page_stats
  prmatrix(page_stats$matches_matrix); #TODO: include that in format_page_stats
}


parse_RDS_items("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_0011.RDS");
parse_RDS_items("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_3392.RDS");
parse_RDS_items("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_1106.RDS");
parse_RDS_items("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_0237.RDS")





