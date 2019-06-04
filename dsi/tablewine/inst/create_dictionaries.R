# File used to create dictionaries for wine catalogs, v 0.1
# This file is here for posterity. The resulting dictionaries are in the Data folder.
#
# Based on the Kaggle wine data set:
# https://www.kaggle.com/zynicide/wine-reviews#winemag-data-130k-v2.csv
#
# Data contains (among others):
# -designation - The vineyard within the winery where the grapes that made the wine are from
# -province - The province or state that the wine is from
# -region_1 - The wine growing area in a province or state (ie Napa)
# -region_2 - Sometimes there are more specific regions specified within a wine growing area (ie Rutherford inside the Napa Valley), but this value can sometimes be blank
# -title - The title of the wine review, which often contains the vintage if you're interested in extracting that feature
# -variety - The type of grapes used to make the wine (ie Pinot Noir)
# -winery - The winery that made the wine
#
#
# This data set is encoded with UTF-8, however catalogs' editiors usually used only ASCII chars, so we have to remove diacritics.
# Also some minor cleaning is neccessary, e.g. remove countries from regions.
#
# Overall, the following steps are taken:
# -COLLATE to latin (remove diacritics)
# -select unique values
# -remove non-word chars with spaces (with some exceptions), e.g. "Saint-d'Emilion" -> "Saint d'Emilion"
# -remove countries from other columns
# -remove values shorter than 5 chars (usually dummy values)
# -remove values that are duplicated among dictionaries (e.g. same value in Province and Region)
# 


####################
# FIXME:
# -values duplicated among dictionaries are being removed, but sometimes we should keep duplicates, e.g. Prosecco is region and type of wine
#
# TODO:
# -add columns with diacritics, so we can have a 'true' value
# -add region2 to region1 before cleanning
#
####################


library(dplyr)
library(RODBC)
library(sqldf)


# Removes redundant spaces.
remove_spaces = function(column) {
  # replace multiple spaces with a single space
  column = gsub("\\s+", " ", column);
  # remove space at the begining and at the end
  column = gsub("^\\s|\\s$", "", column);
  
  return(column)
}


# Helper function for aggregating data.
# Pick the most common country.
most_common = function(x) {
  ux = unique(x)
  return(ux[which.max(tabulate(match(x, ux)))]);
}


# Clears provided column. Removes duplicates, empty values, countries.
# Replaces '-' with " ", and diacritics.
# Keeps country if we want to.
clear_column = function(column_to_clear, keep_country) {
  
  # select column(s)
  subset = wine_data[,c(column_to_clear, "country")];
  
  # replace diacritics
  # FIXME: iconv doesn't recognize some of the characters, so far I identified only one: ??
  subset[,c(1)] = iconv(subset[,c(1)], from="UTF-8", to='ASCII//TRANSLIT');
  
  # remove non-letter, non-number chars, some values are dummy, e.g. "%@#$!" or "1,618"
  subset[,c(1)] = gsub("[^a-zA-Z0-9'&\\.]", " ", subset[,c(1)]); ##more rigorous pattern: \\W

  # remove redundant spaces
  subset[,c(1)] = remove_spaces(subset[,c(1)]);

  # remove values shorter than 5 chars (mostly dummy)
  subset = subset[(which(nchar(subset[,c(1)]) > 4)),];
  
  # remove duplicates and sort
  subset = aggregate(x = subset,
                     by = list(subset[,c(1)]),
                     FUN = most_common);
  subset = subset[-1];
  
  #remove countries (from province and region columns)
  subset = subset[(which(!subset[,c(1)] %in% countries$Country)),];
  
  
  if (keep_country) {
    return(subset);
  } else {
    return(subset[,c(1)]);
  }
}


# create database connection
conn = odbcDriverConnect('driver={SQL Server};server=localhost;database=DataFest;trusted_connection=true')

# get countries from SQL (list from some github repo)
countries = sqlQuery(conn, "SELECT Country FROM Countries");

# read wine csv data encoded with utf-8
wine_data = read.csv(file="C:\\Users\\ssaganowski\\Desktop\\wines\\corpus\\wine_data.csv", header=TRUE, sep=",", encoding="UTF-8", stringsAsFactors = FALSE);


# clear province column
provinces = clear_column("province", TRUE);
# set column name as in SQL table
colnames(provinces) =  c("Province", "Country");


# clear region_1 column
regions = clear_column("region_1", TRUE);
# set column name as in SQL table
colnames(regions) =  c("Region", "Country");
# remove values that are in provinces
regions = regions[!regions$Region %in% provinces$Province,];


# clear producer column
producers = clear_column("winery", TRUE);
# set column name as in SQL table
colnames(producers) =  c("Producer", "Country");
# manually update producer that we know that have wrong value (Chile instead of France).
# For some reason kaggle experts entered Chile as a country more often than France. There may be more errors like this.
row_id = which(producers$Producer %in% c("Domaines Barons de Rothschild Lafite"));
producers$Country[row_id] = "France";
# remove values that are in other dictionaries
producers = producers[!producers$Producer %in% provinces$Province,];
producers = producers[!producers$Producer %in% regions$Region,];

# clear variety column
varieties = clear_column("variety", FALSE);
# remove values that are in other dictionaries
varieties = varieties[!varieties %in% provinces$Province];
varieties = varieties[!varieties %in% regions$Region];
varieties = as.data.frame(varieties[!varieties %in% producers$Producer]);
# set column name as in SQL table
colnames(varieties) =  c("Variety");

# clear designation column
designations = clear_column("designation", FALSE);
# remove values that are in other dictionaries
designations = designations[!designations %in% provinces$Province];
designations = designations[!designations %in% regions$Region];
designations = designations[!designations %in% producers$Producer];
designations = as.data.frame(designations[!designations %in% varieties$Variety]);
# set column name as in SQL table
colnames(designations) =  c("Designation");



# store in db
# truncate tables
sqlQuery(conn, "TRUNCATE TABLE kProvinces"); #'k' stands for kaggle
sqlQuery(conn, "TRUNCATE TABLE kRegions");
sqlQuery(conn, "TRUNCATE TABLE kProducers");
sqlQuery(conn, "TRUNCATE TABLE kDesignations");
sqlQuery(conn, "TRUNCATE TABLE kVarieties");
# save tables
sqlSave(conn, provinces, tablename = "kProvinces", rownames = FALSE, append = TRUE, fast = TRUE);
sqlSave(conn, regions, tablename = "kRegions", rownames = FALSE, append = TRUE, fast = TRUE);
sqlSave(conn, producers, tablename = "kProducers", rownames = FALSE, append = TRUE, fast = TRUE);
sqlSave(conn, designations, tablename = "kDesignations", rownames = FALSE, append = TRUE, fast = TRUE);
sqlSave(conn, varieties, tablename = "kVarieties", rownames = FALSE, append = TRUE, fast = TRUE);
close(conn)
