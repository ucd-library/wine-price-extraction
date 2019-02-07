# Script to run the evaluation functions written in R/wine_evaluate.R
# Jane Carlen

##################################################################################################################
# 0. Setup
##################################################################################################################

library(reshape2)
library(ggplot2)
library(stringr)
library(dplyr)
library(RecordLinkage)

output.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data"
truth.directory = "~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth"
file.number = "0069"

test.prices = readRDS(file.path(output.directory, paste0("UCD_Lehmann_", file.number,".RDS")))$prices
truth.prices = readRDS(file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))$prices

#if correction necessary to truth, something like:
#tmp = readRDS(file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))
#tmp$prices[[1]]$prices[[2]]$row = 1:8
#saveRDS(tmp, file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))

##################################################################################################################
# 2. Run comparison on fileset
##################################################################################################################

# A. Basic stats for all files ####

fileset1 = str_extract(list.files("~/Documents/DSI/wine-price-extraction/dsi/Data/"), pattern = "UCD.*RDS")
fileset1 = fileset1[!is.na(fileset1)]
evaluate.output = vector("list", length(fileset1))
for (i in 1:length(fileset1)) {
  file1 = file.path(output.directory,fileset1[i])
  rds1 = readRDS(file1)$prices
  eval1 = wine.evaluate(rds1)
  evaluate.output[[i]] = eval1
}

output_summary_internal = data.frame(t(sapply(evaluate.output, function(eval) {
  c("n.tables" = eval$n.tables,
    "n.columns.total" = eval$n.columns.total,
    "n.entries.total" = eval$n.entries.total,
    "n.columns.per.table" = paste(eval$n.columns.per.table, collapse = ","),
    "n.entries.per.column" = paste(unlist(eval$n.entries.per.column), collapse = ","),
    "column.names" = paste(names(unlist(eval$n.entries.per.column)), collapse = ","),
    "column.ratios" = paste(round(as.numeric(unlist(eval$table.ratios.summary), 2)), collapse = ",")
  )
})))
rownames(output_summary_internal) = fileset1

write.csv(output_summary_internal, "~/Documents/DSI/wine-price-extraction/dsi/Data/output_summary_internal.csv")

#plot that
output_summary_internal_singlestat = ggplot(melt(output_summary_internal %>% select("n.tables", "n.columns.total", "n.entries.total"), id.vars = NULL) %>%
                                              mutate(value = as.numeric(value))) + 
  geom_histogram(aes(x = value, group = variable)) + facet_grid(~variable, scales = "free_x")
ggsave(output_summary_singlestat, filename = "~/Documents/DSI/wine-price-extraction/dsi/Data/output_summary_internal_singlestat.png")

# B. For files we have truth for ####

fileset2 = str_extract(list.files("~/Documents/DSI/wine-price-extraction/dsi/Data/price_id_truth/"), "[0-9]{4}")
summary.output = vector("list", length = length(fileset2))
names(summary.output) = paste0("UCD_Lehmann_",fileset2,".jpg")
i=1
for(file.number in fileset2) {
  print(file.number)
  test.prices = readRDS(file.path(output.directory, paste0("UCD_Lehmann_", file.number,".RDS")))$prices
  truth.prices = readRDS(file.path(truth.directory, paste0("UCD_Lehmann_", file.number,"_price_truth.RDS")))$prices
  compare.list = wine.compare(test.prices, truth.prices)
  
  summary.output[[i]] = wine.summarize(compare.list)
  i = i+1
}
summary.output = do.call("rbind", summary.output)

write.csv(summary.output, "~/Documents/DSI/wine-price-extraction/dsi/Data/summary_vs_truth.csv")

##################################################################################################################
# 3. Code from modifying output to create truth (if necessary, would be run before comparison making)
##################################################################################################################


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

