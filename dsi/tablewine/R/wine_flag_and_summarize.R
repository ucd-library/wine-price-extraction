# Various performance-evaluating code that acts on TABLE output of run_wine_database.R 
# May 2019

# 1. Flag prices by ratio, not increasing order, magnitude, year, digit placement, etc. (in ENTRY_PRICE) ----

# Hangshi's functions

# Flag potential prices based on likely being a year
# Input is a vector of detected prices, most likely ENTRY_PRICE$price_new
# Arguments set allowable year range and which column to check defaults to price_new
# Output a vector of Boolean values, where TRUE means flagged.

year_flag = function(prices_to_check, year_lower = 1800, year_upper = 2000){
  flag = lapply(1:length(prices_to_check),function(i){
    price = as.numeric(as.character((prices_to_check[i])))
    #set conditions to filter out error entries, and filtered ones are marked unflagged
    if (length(price)== 1&& !anyNA(price) && !price %in% c('','FALSE') && !grepl("\\.",price)){
      #flag the price that falls into the year range
      if (price >= year_lower && price < year_upper){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    else{
      return(FALSE)
    }
  })
  flag = matrix(unlist(flag))
  return(flag)
}

# Flag potential prices based on wrong numbers of digits before or after the decimal
# Input is a vector of detected prices, most likely ENTRY_PRICE$price_new
# Aguments set allowable digits left and right of the dot
# Output is a vector of Boolean values, where TRUE means flagged.
size_flag = function(prices_to_check, size_left = 4, size_right = 2){
  
  flag = lapply(1:length(prices_to_check),function(i){
    price = as.character((prices_to_check[i]))
    #set conditions to filter errors out, and return error with FALSE
    if (length(price)== 1&& !anyNA(price) && !price %in% c('','FALSE')){
      #split the price by dot
      if (grepl("\\.",price)){
        digit1 = unlist(strsplit(as.character(price),'\\.'))[1]
        digit2 = unlist(strsplit(as.character(price),'\\.'))[2]
        #in case there is nothing after the dot
        if (is.na(digit2)){
          #check the number on the left of the dot
          if(nchar(digit1) > size_left){
            return(TRUE)
          }
          else{
            return(FALSE)
          }
        }
        #in case there are digits on both sides
        else if ((nchar(digit1) > size_left) | nchar(digit2) > size_right){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      }
      else{
        return(FALSE)
      }
    }
    else{
      return(FALSE)
    }
  })
  flag = unlist(flag)
  return(flag)
}

# Flag potential prices based on unlikely last digit (least helpful flag) 
# Input is a vector of detected prices, most likely ENTRY_PRICE$price_new
# Argument "ratio" sets minimum percentage a digit has to represent to not get flagged 
# Output is a vector of Boolean values, where TRUE means flagged.
digit_flag = function(prices_to_check, ratio = .04){
  #collect the second digits after the dot of each price
  flag_train = lapply(1:length(prices_to_check),function(i){
    price = as.character((prices_to_check[i]))
    #set conditions to filter errors out
    if (length(price)== 1&& !anyNA(price) && !price %in% c('','FALSE')){
      #split price with dot and collect the second digits after dot
      if (grepl("\\.",price)){
        digit = unlist(strsplit(as.character(price),'\\.'))[2]
        #in case there is nothing after the dot
        if (is.na(digit)){
          return(NULL)
        }
        #in case the second digit is zero and ignored
        else if (nchar(digit) == 1){
          return(0)
        }
        #in case the second digit is shown
        else if (nchar(digit) == 2){
          return(as.numeric(substr(digit,2,2)))
        }
      }
    }
  })
  flag_train = matrix(unlist(flag_train))
  #convert output to data frame
  flag_df = as.data.frame(table(flag_train))
  #start trainning with the ratio input
  flag_digit = lapply(1:nrow(flag_df),function(i){
    if (flag_df[i,2]/length(flag_train) >= ratio){
      return(as.numeric(as.character(flag_df[i,1])))
    }
  })
  flag_digit = matrix(unlist(flag_digit))
  #flagging where TRUE means flagged
  flag = lapply(1:length(prices_to_check), function(i){
    price = as.character((prices_to_check[i]))
    #set conditions to filter errors out, and return error with FALSE
    if (length(price)== 1&& !anyNA(price) && !price %in% c('','FALSE')){
      if (grepl("\\.",price)){
        digit = unlist(strsplit(as.character(price),'\\.'))[2]
        #in case there is nothing after the dot and 0 is unflagged
        if (is.na(digit) && 0 %in% flag_digit){
          return(FALSE)
        }
        #in case 0 is ignored and unflagged
        else if (nchar(digit) == 1 && 0 %in% flag_digit){
          return(FALSE)
        }
        #in case the second digit falls into flag_digit
        else if (nchar(digit) == 2){
          if (substr(digit,2,2) %in% flag_digit){
            return(FALSE)
          }
          else{
            return(TRUE)
          }
        }
        else{
          return(FALSE)
        }
      }
      else{
        return(FALSE)
      }
    }
    else{
      return(FALSE)
    }
  })
  flag = matrix(unlist(flag))
  return(flag)
}

# One jane added:
# Flag potential prices based on amount being too small or large
# Input is a vector of detected prices, most likely ENTRY_PRICE$price_new
# Arguments sets range (min_price to max_price) that would not be flagged 
# Output is a vector of Boolean values, where TRUE means flagged.
amount_flag = function(prices_to_check, min_price = 0.1, max_price = 2000) {
  numeric_price = as.numeric(prices_to_check)
  sapply( numeric_price, function(x) {
    ifelse( !is.na(x) && (x < min_price | x >= max_price), TRUE, FALSE)
  })
}

# One more jane added (Hangshi made one but for the old output form)
# If bottle, case, use global criteria
# Otherwise, use local ratio
ratio_flag = function(TABLE) {
  TABLE2 = TABLE %>% group_by(file_id, table) %>% 
    select(c("price_new", "col.header", "table", "cluster", "row")) %>% head(300) %>%
    right_join(data.frame(row = unique(row)))
     
     
     %>% 
       filter(n_cluster == 2) 
    
  
  {
    
  }
  
}
  
# David's functin:
# check for nonincreasing values in table price column.
# Takes a table, mostly likely ENTRY_PRICE, as input
  # argument "tocheck" says which column of detected prices to check, defaulting to "price_new"
    # "price_raw" may be useful in some cases.
# Output is a vector of Boolean values where TRUE means the row in TABLE is not in increasing order, 
  # i.e. the selected row had a price that was lower than the row before it

order_flag = function(TABLE, tocheck = "price_new"){

  TABLE = TABLE[c(tocheck,'cluster','table','row','file_number')]
  result = vector(length = nrow(TABLE))
  
  for (i in unique(TABLE$file_number)){
    subsetfile = TABLE[TABLE$file_number == i,]
    for (j in unique(subsetfile$table)){
      subsettable = subsetfile[subsetfile$table == j,]
      for (k in unique(subsettable$cluster)){
        subsetfinal = subsettable[subsettable$cluster == k,]
        tmp = suppressWarnings(as.numeric(as.character(subsetfinal[,1])))
        differ = vector(length = length(tmp)-1)
        for (l in seq(length(differ))) {
          if (is.na(tmp[l+1]) | is.na(tmp[l])){next}
          differ[l] = tmp[l+1]-tmp[l]
          if (differ[l] < 0) {
            result[which(TABLE$file_number==i & TABLE$table ==j & TABLE$cluster ==k & TABLE$row == l+1)] = TRUE
          }
        }
      }
    }
  }
  result
}

# 2. Summarize success of name dictionary matching (in ENTRY_NAME) ----
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
