library(readxl)
ids = excel_sheets("Sherry Lehmann Crosswalk Page to Catalog.xlsx")
zz = lapply(ids, function(i) as.data.frame(read_xlsx("Sherry Lehmann Crosswalk Page to Catalog.xlsx", i)))
names(zz) = ids


pages = zz$pages

i = match(pages$page_id , zz$page_xwalk$page_id)
table(is.na(i))
pages$file = zz$page_xwalk$file[i]

table(pages$editable)


marks = zz$marks
pos = strsplit(gsub("[{}]", "", marks$xy), ",")
marks$x = sapply(pos, `[`, 1)
marks$y = sapply(pos, `[`, 2)

i = match(marks$page_id, pages$page_id)
table(is.na(i))
marks$file = pages$file[i]

marksByPage = split(marks, marks$page_id)
sort(sapply(marksByPage, nrow))

pages$numMarks = 0

i = match(names(marksByPage), pages$page_id)
pages$numMarks[i] = sapply(marksByPage, nrow)

i = match(pages$catalog_id, zz$catalogs$catalog_id)
pages = cbind(pages, zz$catalogs[i, c("title", "publisher", "year")])
pages$catalogEditable = zz$catalogs$editable[i]

oo = with(pages, order(year, publisher, page))
pages = pages[oo,]

saveRDS(pages, "PageInfo.rds")


with(pages, table(editable, catalogEditable))


names(marksByPage) = ImagesWithMarks = gsub("UCD_Lehmann_", "", pages$file[ match(names(marksByPage), pages$page_id) ])
saveRDS(marksByPage, "MarksByPage.rds")


ImagesWithMarks = c("0421", "1371", "1885", "1031", "1114", "0723", "1985", "1411", 
"2572", "1019", "3382", "0424", "1560", "1056", "0032", "1289", 
"0045", "0080", "3217", "1307", "0341", "0011", "2266", "0049", 
"3203", "0355", "0657", "0332", "0260", "1004", "0266", "1328", 
"0516", "0940", "2267", "0331", "1005", "3417", "3448", "1561", 
"0684", "0823", "2585", "0431", "2265", "3395", "0028", "0938", 
"1810", "0878", "1369", "0425", "0305", "3383", "0666", "1903", 
"0377", "0415", "0078", "0422", "0405", "0804", "0842", "0036", 
"0983", "0423", "0031", "0870", "0194", "0771", "0089", "0081", 
"0805", "0040", "2268", "1548", "0027", "0016", "0010", "0077", 
"0025", "1622", "0939", "0844", "0426", "0641", "0420", "2047", 
"1637", "1484", "0819", "0079", "0517", "2264", "0015", "3461", 
"0419", "1981", "0748", "0082", "0295", "0006", "2260", "3450", 
"0039", "1329")


MyTruthImages = gsub(".*([0-9]{4}).*", "\\1", list.files("~/DSIProjects/WinePrices/CSV", pattern = "csv$"))
MyTruthImages = c("0008", "0264", "0267", "0267", "0321", "0635", "1544", "1544", 
"1835", "1835", "1835", "1994", "2034", "2034", "2105", "2348", 
"2780", "3224", "3336", "3372", "3377", "3391", "3392", "3443", 
"3483", "3681", "3686", "3705", "3810", "3810", "3913", "4090", 
"0011", "0069", "0237", "0407", "0550", "0551", "0558", "0644", 
"0869", "2940", "3405", "3479", "3794", "4086", "0008", "0114", 
"0772", "0980", "3156", "3429", "3441")


# 0011 is the one we currently have in common.
cs = marksByPage[["0011"]]
pos = strsplit(gsub("[{}]", "", cs$xy), ",")
cs$x = sapply(pos, `[`, 1)
cs$y = sapply(pos, `[`, 2)
cs = cs[order(cs$x, cs$y),]
cs$name = normalizeSpace(cs$name)
cs[, c( 9, 14, 15)]

ours = read.csv(csv[grep("0011", basename(csv))], stringsAsFactors = FALSE)
ours$name = XML:::trim(normalizeSpace(ours$name))

i = match(cs$name, ours$name)
cs$name[is.na(i)]

# cs is missing CHATEAU SIRAN (#2 in column 1 from top down)
# And has CHATEAU BEYCHEVELLE (St. Julien), Mongeard Mugneret
# which is wrong (has extra Mongeard Mugneret) and is also a repeat of
#  the third element
# Chateau CANON - case price is 59.39, but should be 59.30



cs = marksByPage[["0077"]]
cs[order(cs$x, cs$y), c("name", "perprice", "caseprice")]
# This doesn't seem to match image 0077

cs = marksByPage[["0010"]]
cs[order(cs$x, cs$y), c("name", "perprice", "caseprice")]


#==================================

o = list.files('../OCRs', pattern = "rds$", full = TRUE)
ocrs = structure(lapply(o, readRDS), names = basename(o))
names(ocrs) = gsub("\\.rds$", "", names(ocrs))

noprice = pages$file[!is.na(pages$file) & !is.na(pages$editable) & pages$editable == "f"]
source("~/DSIProjects/WinePrices/funs.R")

miss = setdiff(noprice, names(ocrs))
numPrices = sapply(ocrs[intersect(noprice, names(ocrs))], function(x) sum(isPrice(x$text)))




#########



if(FALSE) {
px = read.csv("page_xwalk.csv", stringsAsFactors = FALSE)
tt = sort(table(px$catalog_id))

pc = read.csv("PageToCatalog.csv", stringsAsFactors = FALSE)
pgs = read.csv("pages.csv", stringsAsFactors = FALSE)

mks = read.csv("Marks.csv", stringsAsFactors = FALSE)


i = match(pc$page_id, px$page_id)

pc$file = px$file[i]
}
