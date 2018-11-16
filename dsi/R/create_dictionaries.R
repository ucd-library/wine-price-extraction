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
# -replace '-' with spaces, e.g. "Saint-Emilion" -> "Saint Emilion"
# -add short names, e.g. "Saint-Emilion" -> "St. Emilion"
# -remove countries from provinces and regions
# -remove empty regions / producers / names
# 


####################
# TODO:
# -add region2 to region1 before clearing
# -what to do with designation?



library(dplyr)
library(RODBC)
library(sqldf)

# create database connection
conn = odbcDriverConnect('driver={SQL Server};server=localhost;database=DataFest;trusted_connection=true')

# get countries from SQL
countries = sqlQuery(conn, "SELECT Country FROM Countries");


# read wine csv data encoded with utf-8
wine_data = read.csv(file="C:\\Users\\ssaganowski\\Desktop\\wines\\corpus\\wine_data.csv", header=TRUE, sep=",", encoding="UTF-8");
head(wine_data, n = 10);

# clear province column
provinces = clear_column(wine_data$province);
NROW(provinces);
head(provinces, n=100);

# clear region_1 column
regions = clear_column(wine_data$region_1);
NROW(regions);
head(regions, n=100);

# clear title column ?
# title contains winery, province / region, year, and sometimes name of the wine

# clear producer column
producers = clear_column(wine_data$winery);
NROW(producers);
head(producers, n=100);


# Clears provided column. Removes duplicates, empty values, countries.
# Replaces '-' with " ", and diacritics.
clear_column = function(column_to_clear) {
  # remove duplicates and sort
  values = as.character(levels(sort(column_to_clear)));
  # FIXME distinct() works faster?
  # values = df %>% distinct(column_to_clear) %>% arrange(column_to_clear);
  # remove empty objects
  values = values[values != ""];
  #remove countries
  values = values[!values %in% countries$Country];
  # replace '-' with spaces
  values = gsub("-", " ", values);
  # replace diacritics
  values = iconv(values, from="UTF-8", to='ASCII//TRANSLIT');

  return(values);
}
