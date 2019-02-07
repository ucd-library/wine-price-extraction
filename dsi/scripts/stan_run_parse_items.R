# Moved code to run the functions in parse_items.R to this seperate script so that Jane and Stan could use them with different
# global parameters, e.g., source for the dictionaries and avalues for SIMILARITY_THRESHOLD and PATTERN_THRESHOLD

# similarity threshold above which we consider two strings as potential match
SIMILARITY_THRESHOLD = 0.8;
lockBinding("SIMILARITY_THRESHOLD", globalenv())
# pattern threshold - percentage of items that have to meet criteria to accept considered rule as a general pattern
PATTERN_THRESHOLD = 0.7;
lockBinding("PATTERN_THRESHOLD", globalenv())


# create database connection
conn = odbcDriverConnect('driver={SQL Server};server=localhost;database=DataFest;trusted_connection=true');
  
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


# parse all pages in the folder
setwd("C:/Users/ssaganowski/Desktop/wines");
global_stats = parse_folder("input/");
cat(format_global_stats(global_stats));

# parse single page
parse_page("input/UCD_Lehmann_3794.RDS")