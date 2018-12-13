###################
# We can easily check two things:
# 1. If price format matches a number
# 2. If bottle-case ratio is around 11
#
# Then following steps to fix the prices can be done:
# 1. group prices to see if same bottle prices have same case prices
# 2. correct prices that have wrong ratio (not sure how)
# 3. correct prices with wrong format (e.g. with letters)



library(plyr)
items = readRDS("C:\\Users\\ssaganowski\\Desktop\\wines\\items\\UCD_Lehmann_0237.RDS");


page_prices = NULL;

# gather all prices
for (i in items$prices) {
  prices = cbind("bottle" = i$prices$Bottle, "case" = i$prices$Case);
  page_prices = rbind(page_prices, prices);
}

# add columns
page_prices = cbind(page_prices, "format_bottle" = FALSE, "format_case" = FALSE, "ratio_value" = as.numeric(0), "ratio" = FALSE, "correct_bottle" = "", "correct_case" = "");

# check bottle & case string format
price_regex = "^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$";
page_prices[,"format_bottle"] = grepl(price_regex, page_prices[,"bottle"]);
page_prices[,"format_case"] = grepl(price_regex, page_prices[,"case"]);


# check ratio: 10 * x < x < 12.2 * x
# TODO: ratio for 6-pack is different
page_prices[,"ratio_value"] = as.numeric(page_prices[,"case"]) / as.numeric(page_prices[,"bottle"]);
page_prices[,"ratio"] = 10 < as.numeric(page_prices[,"ratio_value"]) & as.numeric(page_prices[,"ratio_value"]) < 12.2;


# group prices to see if same bottle prices have same case prices
# TODO: extend this to global dictionary of bottle-case pairs
page_prices = as.data.frame(page_prices, stringsAsFactors = FALSE);
# get unique pairs
unique_pairs = count(page_prices, c("bottle", "case", "ratio"));
# get duplicated bottle price (remove 'freq' column, cause count function uses it to weight frequencies)
same_bottle = count(unique_pairs[,-4], "bottle", wt_var = NULL);
same_bottle = same_bottle[same_bottle$freq > 1,];
nrow(same_bottle);
# do the same for case prices
same_case = count(unique_pairs[,-4], "case", wt_var = NULL);
same_case = same_case[same_case$freq > 1,];
nrow(same_case);

# if same_bottle have rows with freq > 1 it means corresponding case prices can be wrong
# if same_case have rows with freq > 1 it means corresponding bottle prices can be wrong


# set correct price for case
if (nrow(same_bottle) > 0) {
  for (row in 1:nrow(same_bottle)) {
    # see what options we have for this bottle price
    s = unique_pairs[unique_pairs$bottle == same_bottle$bottle[row],];
    # get the one that has correct ratio and highest frequency
    most_common = s$case[s$ratio == TRUE & s$freq == max(s$freq)];
    # update all values that had the wrong price
    page_prices$correct_case[page_prices$bottle == same_bottle$bottle[row] & page_prices$case != most_common] = most_common;
  }
}


# set correct price for bottle
if (nrow(same_case)) {
  for (row in 1:nrow(same_case)) {
    # see what options we have for this case price
    s = unique_pairs[unique_pairs$case == same_case$case[row],];
    # get the one that has correct ratio and highest frequency
    most_common = s$bottle[s$ratio == TRUE & s$freq == max(s$freq)];
    # update all values that had the wrong price
    page_prices$correct_bottle[page_prices$case == same_case$case[row] & page_prices$bottle != most_common] = most_common;
  }
}

page_prices
