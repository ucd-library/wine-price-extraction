# Code that acts on table output of run_wine_database.R 
# Jane Carlen
# May 2019

# 1. Summarize success of name dictionary matching (in ENTRY_NAME) ----
# See parse_items.R for more description of fields

name_summary_global_stats <- function(ENTRY_NAME) {
  
  round(t(
      
    with(ENTRY_NAME,

      data.frame(
        items = nrow(ENTRY_NAME),
        pages = n_distinct(file),
        #note, no actual dictionary for color since only a few possible values, but I'm including it in dict hits
        total_dict_hits = sum(ENTRY_NAME[,c("color", "province", "region",
                "producer", "designation", "variety", "country")] != "NULL"),
        avg_dict_hits_per_item = sum(ENTRY_NAME[,c("color", "province", "region",
                "producer", "designation", "variety", "country")] != "NULL")/nrow(ENTRY_NAME),
        pct.dictionary_hit = mean(as.logical(dictionary_hit)), # % items w/ any hit in dictionaries (full, decent, or sub)
        pct.any_hit = mean(as.logical(any_hit)), # % items w /no hits at all (e.g. id, year, dictionary hits, etc.)
        pct.inspect.any = 1 - mean(inspect == "NULL"),
        pct.id = 1 - mean(id == "NULL"),
        pct.year = 1 - mean(year== "NULL"),
        pct.color = 1 - mean(color== "NULL"),
        pct.province = 1 - mean(province == "NULL"),
        pct.region = 1 - mean( region== "NULL" ),
        pct.producer = 1 - mean( producer== "NULL"),
        pct.designation = 1 - mean( designation== "NULL"),
        pct.variety = 1 - mean( variety== "NULL"),
        pct.country = 1 - mean( country== "NULL"),

        # Unique values found (-1 for "NULL")
        unique.id = length(unique(id)) - 1,
        unique.year = length(unique(year)) - 1,
        unique.color= length(unique(color)) - 1,
        unique.province= length(unique(province)) - 1,
        unique.region= length(unique(region)) - 1,
        unique.producer= length(unique(producer)) - 1,
        unique.designation= length(unique(designation)) - 1,
        unique.variety= length(unique(variety)) - 1,
        unique.country = length(unique(country)) - 1
      )
    )
    ) , 2)
}

name_summary_global_stats(ENTRY_NAME)

# 2. (TO DO) Flag prices by ratio, not increasing order, magnitude, year, digit placement, etc. (in ENTRY_PRICE) ----