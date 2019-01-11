# File to compare wine_price_table output to truth and summarize results
# Jane Carlen
# Created 12-8-2018

# Truth files are created by corrected price_table_extraction output
# Saved in "wine-price-extraction/dsi/Data/price_id_truth/" as of 1/10/19

# FUNCTIONS ####

wine_summarize <- function(saved_RDS) {
  rds = readRDS(paste0("~/Documents/DSI/wine-price-extraction/dsi/Data/",saved_RDS))
  items = try({lapply(rds$prices, function(x) {c(n.ids = length(x[[1]]), n.prices = sapply(x$prices, length))})})
  prices = rds$prices
  return(list(items = items, prices = prices))
}

wine_evaluate <- function(output, truth) {
  # compare number of prices found
  
  # if any missing in output, find the right gaps to align them
  
  # 
}

# CODE ####

# saveRDS(UCD_Lehmann_0011_price_truth, "~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/UCD_Lehmann_0011_price_truth.RDS")

truth1 = "1544"
truthdata = readRDS(paste0("~/Documents/DSI/OCR_SherryLehmann/Truth2/",truth1,"_column2.RDS"))
try({truthdata = data.frame(truthdata)})
truthdata = data.frame(apply(truthdata, 2, as.character), stringsAsFactors = F)

#modify truth?
truthdata$Bottle = sub(pattern = "(\\.[0-9]{1}$)",  "\\10", x = truthdata$Bottle)
truthdata$Bottle = sub(pattern = "^0\\.",  "\\.", x = truthdata$Bottle)
truthdata$Bottle = sub(pattern = "(^[0-9]+$)",  "\\1.00", x = truthdata$Bottle)
truthdata$Case = sub(pattern = "(^[0-9]+$)",  "\\1.00", x = truthdata$Case)
truthdata$Case = sub(pattern = "(\\.[0-9]{1}$)",  "\\10", x = truthdata$Case)
truthdata$CaseArrival = sub(pattern = "(\\.[0-9]{1}$)",  "\\10", x = truthdata$CaseArrival)
truthdata$CaseArrival = sub(pattern = "(^[0-9]+$)",  "\\1.00", x = truthdata$CaseArrival)

truthdata

# insert

truthoutput = readRDS(paste0("~/Documents/DSI/wine-price-extraction/dsi/Data/UCD_Lehmann_",truth1,".RDS"))
# From truthfile
i = 2
truthoutput$prices[[i]]$ids[[1]] = data.frame(row = 1:length(truthdata$Description),
                                              name = as.character(truthdata$Description), stringsAsFactors = F)

truthoutput$prices[[i]]$prices[[4]] = data.frame(row = 1:length(truthdata$QuartCase),
                                                 text.new = as.character(truthdata$QuartCase),
                                                 stringsAsFactors = F)

truthoutput$prices[[i]]$prices[[2]] = data.frame(row = 1:length(truthdata$CaseArrival),
                                                 text.new = as.character(truthdata$CaseArrival),
                                                 stringsAsFactors = F)

truthoutput$prices

# Custom changes
#1,1
truthoutput$prices[[1]]$ids[[1]] = rbind(truthoutput$prices[[1]]$ids[[1]], c(6, "234")) %>% arrange(row)
truthoutput$prices[[1]]$prices[[1]] = rbind(truthoutput$prices[[1]]$prices[[1]], c(8, "2.19")) %>% arrange(row)
truthoutput$prices[[1]]$prices[[1]][1,2] = "1.49"
#1,2
truthoutput$prices[[1]]$prices[[2]] = rbind(truthoutput$prices[[1]]$prices[[2]], c(6, "21.50")) %>% arrange(row)
truthoutput$prices[[1]]$prices[[2]][7,2] = "23.65"

#2,1
truthoutput$prices[[2]]$ids[[1]] = rbind(truthoutput$prices[[2]]$ids[[1]], c(0, "614")) %>% arrange(row)
truthoutput$prices[[2]]$ids[[1]]$row = 1:8
truthoutput$prices[[2]]$prices[[1]] = rbind(truthoutput$prices[[2]]$prices[[1]], c(0, "2.29"))
truthoutput$prices[[2]]$prices[[1]] = truthoutput$prices[[2]]$prices[[1]] %>% arrange(row)
truthoutput$prices[[2]]$prices[[1]]$row = 1:8
truthoutput$prices[[2]]$prices[[1]][6,2] = "2.99"

#2,2
truthoutput$prices[[2]]$prices[[2]] = rbind(truthoutput$prices[[2]]$prices[[2]], c(0, "24.75")) %>% arrange(row)
truthoutput$prices[[2]]$prices[[2]]$row = 1:8

#names
names(truthoutput$prices[[1]]$prices) = c("bottle", "2 bottles")
names(truthoutput$prices[[1]]$prices) = c("Bottle","Case")
names(truthoutput$prices[[2]]$prices) = c("Bottle","Case")
names(truthoutput$prices[[3]]$prices) = c("Bottle")
names(truthoutput$prices[[4]]$prices) = c("Bottle")
names(truthoutput$prices[[1]]$prices) = c("Case NOW","Case Price Upon Arrival","You Save")
names(truthoutput$prices[[2]]$prices) = c("Fifth Bottle","Fifth Case","Quart Bottle","Quart Case")


# save
saveRDS(truthoutput, paste0("~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/UCD_Lehmann_",truth1,"_price_truth.RDS"))



#-----


files = (str_extract(list.files("~/Documents/DSI/wine-price-extraction/dsi/Data"), pattern = "UCD.*\\.RDS"))
files = subset(files, !is.na(files))

results = vector("list", length(files)); names(results) = files
for (i in seq_along(files)) {results[[i]] = wine_summarize(files[i])$items}

ncol = max(sapply(results,  function(x) {y = x[2]; sapply(y, length)}))
results2 = matrix(0, sum(sapply(results,  length)), ncol)
j = 1
for (i in seq_along(results)) {
  m = length(results[[i]])
  for (elem in 1:m) {
    results2[j, 1:length(results[[i]][[elem]])] = results[[i]][[elem]]
    j = j+1
  }
}

#within each table detected, (abs) average difference in number of items in each column from mean number of items
table(round(apply(results2, 1, function(x) {mean( abs(x[x>0] - mean(x[x>0])) )}), 1))
######################  18 tables matched across numbers of ids and prices ###################### 

#ratios for tables with two columns (presumably bottle and case)
results.prices = vector("list", length(files)); names(results) = files
for (i in seq_along(files)) {results.prices[[i]] = wine_summarize(files[i])$prices}

results.prices2 = lapply(results.prices, function(x) {x[[1]]$prices})
tmp1 = lapply(results.prices, function(x) {lapply(x, function(y) {rbind(y[[1]], do.call("rbind", y$prices))})})
tmp2 = lapply(tmp1, function(x) {
  lapply(x, function(y) {
    if(nrow(y) > 2) {as.numeric(y[3,])/as.numeric(y[2,])} else {NULL}
})})
tmp3 = lapply(tmp2, function(x) {
  lapply(x, function(y) {
    mean(y > 12.2 | y < 10, na.rm = T)
  })})

results.prices3 = unlist(tmp3)
sum(results.prices3 == 0, na.rm = T) # only 5 tables all in ratio
mean(results.prices3 == 0, na.rm = T)
table(round(results.prices3, 1)) 
############ 0 or .1 seems ok, which is 11 total -- getting about 11 about of 67 tables well ########### 
