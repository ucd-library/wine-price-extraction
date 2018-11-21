#
# Creating dictionaries for wine catalogs, v 0.1
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
# 


####################
# FIXME:
# -some values are replicated in two columns, e.g. province and region_1 have Bordeaux, decide which to delete
# 
# TODO:
# -merge countries, provinces, regions, producers into one table
# -add columns with diacritics, so we can have a 'true' value
# -add region2 to region1 before cleanning
#
####################


library(dplyr)
library(RODBC)
library(sqldf)


# Clears provided column. Removes duplicates, empty values, countries.
# Replaces '-' with " ", and diacritics.
clear_column = function(column_to_clear) {
  # replace diacritics
  # FIXME: iconv doesn't recognize some of the characters, so far I identified only one: ??
  values = iconv(column_to_clear, from="UTF-8", to='ASCII//TRANSLIT');
  # remove non-letter, non-number chars, some values are dummy, e.g. "%@#$!" or "1,618"
  values = gsub("[^a-zA-Z0-9'&\\.]", " ", values); ##more rigorous pattern: \\W
  # remove redundant spaces
  values = gsub("\\s+", " ", values);
  values = gsub("^\\s|\\s$", "", values);
  # remove values shorter than 5 chars (mostly dummy)
  values = values[nchar(values) > 4];
  # remove duplicates and sort
  values = sort(unique(values));
  # FIXME distinct() works faster?
  # values = df %>% distinct(column_to_clear) %>% arrange(column_to_clear);
  #remove countries
  values = values[!values %in% countries$Country];
  # coerce as a data.frame for sqlSave function
  values = as.data.frame(values);
  
  return(values);
}


# create database connection
conn = odbcDriverConnect('driver={SQL Server};server=localhost;database=DataFest;trusted_connection=true')

# get countries from SQL
countries = sqlQuery(conn, "SELECT Country FROM Countries");


# read wine csv data encoded with utf-8
wine_data = read.csv(file="C:\\Users\\ssaganowski\\Desktop\\wines\\corpus\\wine_data.csv", header=TRUE, sep=",", encoding="UTF-8");
head(wine_data, n = 10);


# clear province column
provinces = clear_column(wine_data$province);
# set column name as in SQL table
colnames(provinces) =  c("Province");
NROW(provinces);
head(provinces, n=100);


# clear region_1 column
regions = clear_column(wine_data$region_1);
# set column name as in SQL table
colnames(regions) =  c("Region");
NROW(regions);
head(regions, n=100);


# clear producer column
producers = clear_column(wine_data$winery);
# set column name as in SQL table
colnames(producers) =  c("Producer");
NROW(producers);
head(producers, n=100);


# clear designation column
designations = clear_column(wine_data$designation);
# set column name as in SQL table
colnames(designations) =  c("Designation");
NROW(designations);
head(designations, n=100);


# clear variety column
varieties = clear_column(wine_data$variety);
# set column name as in SQL table
colnames(varieties) =  c("Variety");
NROW(varieties);
head(varieties, n=100);




# clear title column ?
# title contains winery, province / region, year, variety and sometimes sth more


# store in db
sqlSave(conn, provinces, tablename = "kProvinces", rownames = FALSE, append = TRUE, fast = TRUE);
sqlSave(conn, regions, tablename = "kRegions", rownames = FALSE, append = TRUE, fast = TRUE);
sqlSave(conn, producers, tablename = "kProducers", rownames = FALSE, append = TRUE, fast = TRUE);
sqlSave(conn, designations, tablename = "kDesignations", rownames = FALSE, append = TRUE, fast = TRUE);
sqlSave(conn, varieties, tablename = "kVarieties", rownames = FALSE, append = TRUE, fast = TRUE);
close(conn);
