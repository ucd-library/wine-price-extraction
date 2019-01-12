# File to compare wine_price_table output to truth and summarize results
#
# Jane Carlen
# Created 12-8-2018

# Extraction function price_table_exracton in wine_price_tables.R
#
# Output save in "~/Documents/DSI/wine-price-extraction/dsi/Data/"
#
# Truth saved in "wine-price-extraction/dsi/Data/price_id_truth/" as of 1/10/19
# list.files("~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/")
# Truth files are created by corrected price_table_extraction output (see below)

output.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data"
truth.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth"
file.number = "0011"

test.prices = readRDS(file.path(output.directory, paste0("UCD_Lehmann_", file.number,".RDS")))$prices
truth.prices = readRDS(file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))$prices

# Compare output to truth for a single image (list output)
# All differences are test - truth
wine.compare <- function(test.prices, truth.prices) {
  
  # Output of price_table_extraction
  test.stat = list(n.tables = length(test.prices),
                   n.columns.per.table = sapply(test.prices, function(x) {length(x$prices)}),
                   n.entries.per.column = sapply(test.prices, function(x) {sapply(x$prices, nrow)}),
                   n.columns.total = sum(sapply(test.prices, function(x) {length(x$prices)})),
                   n.entries.total = sum(sapply(test.prices, function(x) {sapply(x$prices, nrow)}))) # each column is a table
  
  # Truth
  truth.stat = list(n.tables = length(truth.prices),
                    n.columns.per.table = sapply(truth.prices, function(x) {length(x$prices)}),
                    n.entries.per.column = sapply(truth.prices, function(x) {sapply(x$prices, nrow)}),
                    n.columns.total = sum(sapply(truth.prices, function(x) {length(x$prices)})),
                    n.entries.total = sum(sapply(truth.prices, function(x) {sapply(x$prices, nrow)}))) # each column is a table
  
  
  # Initialize output list
  compare.list = list(test.stat = test.stat,
                      truth.stat = truth.stat,
                      diff.in.tables = truth.stat$n.tables - test.stat$n.tables,
                      diff.in.columns.total = truth.stat$n.columns.total - truth.stat$n.columns.total,
                      diff.in.entries.total = truth.stat$n.entries.total - truth.stat$n.entries.total,
                      # If same number of tables
                      diff.in.columns.bytable = NULL,
                      diff.in.entries.bytable = NULL,
                      # If same number of columns in table
                      diff.in.entries = NULL)
                  
  # Same number of tables?
  if (test.stat$n.tables != truth.stat$n.tables) {
    cat("Truth and output have different numbers of tables.\n")
    cat("Truth has", truth.stat$n.tables)
    cat("Output has", test.stat$n.tables)
    return(compare.list)
  } else {
    
    # Same number of columns in tables?
    diff.in.columns.bytable = truth.stat$n.columns.per.table - test.stat$n.columns.per.table
    compare.list[["diff.in.columns.bytable"]] = diff.in.columns.bytable
    
    compare.list[["diff.in.entries.bytable"]] =  colSums(truth.stat$n.entries.per.column) - colSums(test.stat$n.entries.per.column)
    
    compare.list[["diff.in.entries"]] = lapply(1:length(diff.in.columns.bytable), function(x) {
      
      # Same number of columns in tables:
      if (diff.in.columns.bytable[x] == 0) {
        
        compare.list[["diff.in.entries"]][[x]] = truth.stat$n.entries.per.column[,x] - test.stat$n.entries.per.column[,x]
        
        # Compare entries in columns:
        diff.in.entries = lapply(1:truth.stat$n.columns.per.table[x], function(y) {
            
          inner_join(test.prices[[x]]$prices[[y]], truth.prices[[x]]$prices[[y]], by = "row") %>% 
                     
              #diffs in dollar amounts
              mutate(dollar.diffs = as.numeric(text.new.y)-as.numeric(text.new.x),
                          
              #levenshtein distances       
              lev.diffs = levenshteinDist(text.new.y, text.new.x)) %>% select(dollar.diffs, lev.diffs)
            
        })
        
        
      } else {
        #Different number of columns in table
        compare.list[["diff.in.entries"]][[x]] = NULL
      }
    })
    
    
  }
  return(compare.list)
}

wine.compare.list = wine.compare(test.prices, truth.prices)

wine.summarize = function(wine.compare.list) {
  
  #condense all entrywise comprisons
  all.diffs = do.call("rbind", lapply(wine.compare.list$diff.in.entries, function(x) { do.call("rbind",x) }))
  
  data.frame(detected.tables = wine.compare.list$test.stat$n.tables,
             true.tables = wine.compare.list$truth.stat$n.tables,
             diff.in.tables = wine.compare.list$diff.in.tables,
             
             detected.entries = wine.compare.list$test.stat$n.entries.total,
             true.entries = wine.compare.list$truth.stat$n.entries.total,
             n.entries.percent.error = round(wine.compare.list$truth.stat$n.entries.total/
                                               wine.compare.list$test.stat$n.entries.total, 2),
             mean.dollar.diff = mean(all.diffs$dollar.diffs, na.rm = T),
             median.dollar.diff = median(all.diffs$dollar.diffs, na.rm = T),
             mean.lev.diff = mean(all.diffs$dollar.diffs, na.rm = T),
             median.lev.diff = mean(all.diffs$dollar.diffs, na.rm = T),
             entries.compared = nrow(all.diffs))
          
}

wine.summarize(wine.compare.list)


#ratios for tables with two columns (presumably bottle and case)

# Code from modifying output to create truth ####

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
#saveRDS(truthoutput, paste0("~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/UCD_Lehmann_",truth1,"_price_truth.RDS"))

