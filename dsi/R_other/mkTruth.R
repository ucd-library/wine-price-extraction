o0321 = readRDS(file.path("~/Data/UCD_LEHMANN/OCRs", "UCD_Lehmann_0321.rds"))
vv = readTable(o0321, colPos = c(0, 630, 780, 990, 2990,3375, Inf))
vv = vv[, -2]
vv = vv[-c(1:2, 33:35),]
write.csv(vv, "0321.csv", row.names = FALSE)
# git: 901ffc8



# ================

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_0267.jpg"))
pix2 = pixThresholdToBinary(pixConvertTo8(pix), 180)

bb = GetBoxes(pix2)
p1 = bb[ bb$right < 2900, ]
p2 = bb[ bb$right >= 2950 & bb$left >= 2950, ]

# abline(v = c(3250, 4500, 4700), col = "red")

breaks = c(0, 3000, 3250, 4500, 4700, Inf)
tt2 = as.data.frame(readTable(p2, colPos = breaks), stringsAsFactors = FALSE)

tt2 = tt2[,-1]
tt2[] = lapply(tt2, function(x) { x[x %in% c("-", ":", "- -", "/", ".")] = ""; x})

z = apply(tt2, 1, unique)
tt3 = tt2[ !sapply(z, function(x) length(x) == 1 && x == ""), ]


i = which(tt3[,1] != "" & apply(tt3[,-1], 1, function(x) all(x == "")))
i = i[  tt3[i+1,1] == "" ]
tt3[i+1,1] = tt3[i,1]
tt3 = tt3[-i,]
rownames(tt3) = NULL
names(tt3) = c("Bin", "Description", "Bottle", "Case")

tt3 = tt3[-(1:5),]
write.csv(tt3, "0267_p2.csv", row.names = FALSE)

#------------
breaks = c(0, 700, 1000, 2250, 2500, Inf)
tt2 = as.data.frame(readTable(p1, colPos = breaks), stringsAsFactors = FALSE)
tt2 = tt2[,-1]
tt2[] = lapply(tt2, function(x) { x[x %in% c("-", ":", "- -", "/", ".", "|", "i", "I")] = ""; x})
z = apply(tt2, 1, unique)
tt3 = tt2[ !sapply(z, function(x) length(x) == 1 && x == ""), ]

i = which(tt3[,1] != "" & apply(tt3[,-1], 1, function(x) all(x == "")))
i = i[  tt3[i+1,1] == "" ]
tt3[i+1,1] = tt3[i,1]
tt3 = tt3[-i,]
rownames(tt3) = NULL
names(tt3) = c("Bin", "Description", "Bottle", "Case")

tt3 = tt3[-(1:2),]
write.csv(tt3, "0267_p1.csv", row.names = FALSE)


#######################################



aa = readRDS(file.path("~/Data/UCD_LEHMANN/OCRs", "UCD_Lehmann_2034.rds"))
class(aa) = c("OCRResults", "data.frame")
aa = aa[aa$right > 2050 & aa$left > 2050,]

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_2034.jpg"))
pix2 = pixRotate(pix, -pi*0.001)
plot(pix2)

bb = GetBoxes(pix2)

bb = bb[bb$right > 2050 & bb$left > 2050,]


a = bb[bb$top < 3400 & bb$right < 3600, ]
plot(a, cex = .5)
abline(v = c(2200, 3100, 3350), col = "red")
vv = as.data.frame(readTable(a, colPos = c(0, 2200, 3100, 3350, Inf)), stringsAsFactors = FALSE)
vv = vv[,-1]
vv = vv[!apply(vv, 1, function(x) all(x == "")), ]
vv = vv[-(1:2),]
vv = vv[1:17,]
names(vv) = c("Description", "Bottle", "Case")

vv$yr  = gsub(".*(19[0-9]{2}).*", "\\1", vv[,1])
vv[[1]]  = gsub("(.*)(19[0-9]{2})(.*)", "\\1\\3", vv[,1])

write.csv(vv, "CSV/2034_tbl1.csv", row.names = FALSE)

#
b = bb[bb$top > 3400 & bb$top < 4320 & bb$right < 3600, ]
b = bb[bb$top > 3400 & bb$right < 3600, ]
plot(b, cex = .5, ylim = c(0, 6000))
abline(v = c(2200, 3200, 3350), col = "red")
vv = as.data.frame(readTable(b, colPos = c(0, c(2200, 3200, 3350), Inf)), stringsAsFactors = FALSE)
vv = vv[,-1]
vv = vv[-1,]
vv = vv[!apply(vv, 1, function(x) all(x == "")), ]


write.csv(vv, "CSV/2034_tbl2.csv", row.names = FALSE)

########################


a = readRDS(file.path("~/Data/UCD_LEHMANN/OCRs", "UCD_Lehmann_0008.rds"))
class(a) = c("OCRResults", "data.frame")

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_0008.jpg"))
pix2 = pixRotate(pix, -pi*0.005)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)

a = aa = GetBoxes(pix3)

plot(pix3)
breaks = c(220, 2475, 2750, 3100, 3450)
abline(v = breaks, col = "red")

a = a[ a$top < min(a$top[which(a$text == "TERMS")]) - 20, ]

vv = readTable(a, colPos = c(0, breaks, Inf))
vv = vv[,-1]

write.csv(vv, "CSV/0008.csv", row.names = FALSE)

#####################

a = readRDS(file.path("~/Data/UCD_LEHMANN/OCRs", "UCD_Lehmann_3156.rds"))
class(a) = c("OCRResults", "data.frame")

plot(a, cex = .5)
breaks = c(450, 610, 3050, 3350)
abline(v = breaks, col = "red")

vv = readTable(a, colPos = c(0, breaks, Inf))
vv = vv[,-1]

w = apply(vv, 1, function(x) all(x == ""))
vv = vv[!w,]


###############################


a = readRDS(file.path("~/Data/UCD_LEHMANN/OCRs", "UCD_Lehmann_3377.rds"))
class(a) = c("OCRResults", "data.frame")

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_3377.jpg"))
pix2 = pixRotate(pix, -pi*0.005)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)

a = aa = GetBoxes(pix3)

a = a[ a$top > 1557,]

plot(a, cex = .5)
breaks = c(800, 1025, 2600, 2850, 3200)
abline(v = breaks, col = "red")

vv = readTable(a, colPos = c(0, breaks, Inf))
vv = vv[,-1]

w = apply(vv, 1, function(x) all(x == ""))
vv = vv[!w,]

vv[] = lapply(vv, function(x) gsub("\\.\\.", "", x))
vv[] = lapply(vv, function(x) gsub(" ?\\|", "", x))

vv = vv[-(1:3),]
names(vv) = c("Bin", "Description", "Bottle", "Case", "Future")
rownames(vv) = NULL

i = which(vv[[4]] == "")
i =  i[ vv[[5]][i-1] != "" ]


###########

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_1994.jpg"))
pix2 = pixRotate(pix, -pi*0.005)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)

a = aa = GetBoxes(pix3)


plot(a, cex = .5)
breaks = c(650, 3075, 3300)
abline(v = breaks, col = "red")

vv = readTable(a, colPos = c(0, breaks, Inf))
vv = vv[,-1]

vv = vv[-(1:3), ]

i = grep("19[0-9]{2} (RED|WHITE) WINES", vv[[1]])

vv[[1]][i] = gsub("19[0-9]{2} (RED|WHITE) WINES.*", "", vv[[1]][i])

w = apply(vv, 1, function(x) all(x == ""))
vv = vv[!w,]


write.csv(vv, "CSV/1994.csv", row.names = FALSE)



#############


#a = readRDS(file.path("~/Data/UCD_LEHMANN/OCRs", "UCD_Lehmann_3392.rds"))
#class(a) = c("OCRResults", "data.frame")

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_3392.jpg"))
pix2 = pixRotate(pix, -pi*0.0025)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)

a = aa = GetBoxes(pix3)

plot(a, cex = .5)
breaks = c(505, 805, 3225, 3500)
abline(v = breaks, col = "red")

vv = readTable(a, colPos = c(0, breaks, Inf), offset = 30)
vv = vv[,-1]
vv = vv[!apply(vv, 1, function(x) all(x == "")), ]

# End up with text in price columns that comes from the inter table titles!
vv[nrow(vv), 4] = "80.90"

vv = vv[-(1:2),]

names(vv) = c("Bin", "Description", "Bottle", "Case")
vv  = vv[vv[,3] != "", ]

write.csv(vv, "CSV/3392.csv", row.names = FALSE)



############

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_3391.jpg"))
pix2 = pixRotate(pix, pi*0.0025)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)

a = aa = GetBoxes(pix3)

plot(a, cex = .5)
breaks = c(150, 500, 2875, 3200, 3500)
abline(v = breaks, col = "red")

vv = readTable(a, colPos = c(0, breaks, Inf), offset = 30)
vv = vv[,-c(1,6)]
vv = vv[!apply(vv, 1, function(x) all(x == "")), ]

# End up with text in price columns that comes from the inter table titles!
vv = vv[-c(1, nrow(vv)),]

names(vv) = c("Bin", "Description", "Bottle", "Case")
vv  = vv[vv[,3] != "", ]

vv[3:4] = lapply(vv[3:4], function(x) gsub(",", ".", x))
write.csv(vv, "CSV/3391.csv", row.names = FALSE)



####

#a = readRDS(file.path("~/Data/UCD_LEHMANN/OCRs", "UCD_Lehmann_2105.rds"))
#class(a) = c("OCRResults", "data.frame")class(a) = c("OCRResults", "data.frame")

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_2105.jpg"))
pix2 = pixRotate(pix, pi*0.005)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)


ts = tesseract(pix3)
SetRectangle(ts, 0, 0, 3000, 4000)
a = aa = GetBoxes(ts)

plot(a, cex = .5)
breaks = c(750, 2350, 2625, 2900)
abline(v = breaks, col = "red")

vv = readTable(a, colPos = c(0, breaks, Inf), offset = 30)
vv = vv[,-c(1,5)]
vv = vv[!apply(vv, 1, function(x) all(x == "")), ]

names(vv) = c("Description", "Bottle", "Case")

vv[[1]] = gsub("\\.\\.+.*", "", vv[[1]])

write.csv(vv, "CSV/2105.csv", row.names = FALSE)

##

# 2 columns and subtables with different formats.

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_1835.jpg"))
pix2 = pixRotate(pix, pi*0.005)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)


a = aa = GetBoxes(pix3)

# Do the right column first.
plot(a, cex = .5)
breaks = c(2000, 3225, 3420, 3650)
abline(v = breaks, col = "red")

vv = readTable(a, colPos = c(0, breaks, Inf), offset = 30)
vv = vv[, -c(1, 5)]

vv = vv[-c(1, 2, nrow(vv)),]
vv = vv[!apply(vv, 1, function(x) all(x == "")), ]
vv = vv[ vv[[3]] != "",]

vv[[1]] = gsub("\\.+$", "", vv[[1]])
vv[[1]] = gsub("(oo+|ii*|oe|on|c|=)$", "", vv[[1]])


write.csv(vv, "CSV/1835_column2.csv", row.names = FALSE)


# col1 upper

breaks = c(300, 1525, 1750, 2000)
abline(v = breaks, col = "red")

a[a$text == "VERMOUTH",]
b = a[ a$top < 3887, ]

vv = readTable(b, colPos = c(0, breaks, Inf), offset = 30)
vv = vv[, -c(1, 5)]

vv = vv[-c(1, 2, nrow(vv)),]
vv = vv[!apply(vv, 1, function(x) all(x == "")), ]
vv = vv[ vv[[3]] != "",]

vv[[1]] = gsub("\\.+$", "", vv[[1]])
vv[[1]] = gsub("(oo+|ii*|oe|on|c|=)$", "", vv[[1]])
vv[[1]] = gsub("\\.+$", "", vv[[1]])
vv[[1]] = XML:::trim(vv[[1]])


names(vv) = c("Description", "Bottle", "Case")
vv = vv[-(1),]

write.csv(vv, "CSV/1835_column1_top.csv", row.names = FALSE)


# From Dry Vermouth to the top of Sweet vermouth
breaks = c(320, 920, 1100, 1800, 2000)
abline(v = breaks, col = "green", lwd = 2)

#,
b = a[ a$top > 3900 & a$top < 4570 ,  ]
vv = readTable(b, colPos = c(0, breaks[1:3], Inf), offset = 40)
vv = vv[, -c(1, 4)]
vv = vv[- c(1, 2),]
A = vv

b = a[ a$top > 4585,  ]
vv = readTable(b, colPos = c(0, breaks[1:3], Inf), offset = 40)
vv = vv[, -c(1, 4)]
#vv = vv[- c(1, 2),]
B = vv


b = a[ a$top > 3900 & a$top < 4580 ,  ]
vv = readTable(b, colPos = c(0, breaks[3:5], Inf), offset = 40)
vv = vv[, c(2, 3)]
#vv = vv[- c(1, 2),]
C = vv

b = a[ a$top > 4580 ,  ]
vv = readTable(b, colPos = c(0, breaks[3:5], Inf), offset = 40)
vv = vv[, c(2, 3)]
vv = vv[- c(1, 2, nrow(vv)-1, nrow(vv)), ]
D = vv

XX = list(A, B, C, D)
vv = as.data.frame(lapply(1:2, function(i) unlist(lapply(XX, `[[`, i))), stringsAsFactors = FALSE)


#vv = data.frame(c(vv[[1]], vv[[3]]),  c(vv[[2]], vv[[4]]))
#A = vv[!apply(vv, 1, function(x) all(x == "")), ]
#b = a[ a$top >  4570, ]
#vv = readTable(b, colPos = c(0, breaks, Inf), offset = 40)
#vv = vv[, -c(1, 6)]

write.csv(vv, "CSV/1835_column1_bottom.csv", row.names = FALSE)




####################################



pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_2780.jpg"))
pix2 = pixRotate(pix, -pi*0.005)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)


a = GetBoxes(pix3)

breaks = c(200, 500, 3225, 3515, 3750)
abline(v = breaks, col = "green", lwd = 2)

vv = readTable(a, colPos = c(0, breaks, Inf), offset = 40)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x == "")), ]

# have all the subdescriptions also in separate lines and easy to collect for specific wine.
w = vv[[1]] != ""
vv = vv[w,]
vv = vv[- c(1:7, nrow(vv)), ]

names(vv) = c("Bin", "Description", "Bottle", "Case")

write.csv(vv, "CSV/2780.csv", row.names = FALSE)


##############


pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_3705.jpg"))
pix2 = pixRotate(pix, pi*0.005)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)

plot(pix3)

a = GetBoxes(pix3)

a[a$text == "TAITTINGER", ]

b = a[ a$top < 4000,]

breaks = c(650, 800, 2100, 2450, 2750)
abline(v = breaks, col = "red")

vv = readTable(b, colPos = c(0, breaks, Inf), offset = 40)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x == "")), ]

names(vv) = c("Bin", "Description", "Bottle", "Case")

vv = vv[-1,]

write.csv(vv, "CSV/3705.csv", row.names = FALSE)


###########

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_3686.jpg"))
pix2 = pixRotate(pix, -pi*0.0075)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)
plot(pix3)

a = GetBoxes(pix3)

a[a$text == "189",]
b = a[a$top > 1900,]
breaks = c(440, 650, 2350, 2700, 3200)
abline(v = breaks, col = "green")
vv = readTable(b, colPos = c(0, breaks, Inf), offset = 40)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x == "")), ]

vv = vv[ vv[[1]] != "", ]

# Missing some text we could get but will add it manually.
names(vv) = c("Bin", "Description", "Bottle", "Case")
write.csv(vv, "CSV/3686.csv", row.names = FALSE)



########


pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_3810.jpg"))
pix2 = pixRotate(pix, -pi*0.005)
pix3 = pixThresholdToBinary(pixConvertTo8(pix2), 180)
plot(pix3)

a = GetBoxes(pix3)

a[a$text == "AVAILABLE",]
b = a[a$top > 3139,]
breaks = c(250, 375, 1675, 1950)
abline(v = breaks, col = "green")
vv = readTable(b, colPos = c(0, breaks, Inf), offset = 30)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]

vv = vv[-c(1, seq(nrow(vv)-3, length = 4)), ]

#vv = vv[ vv[[1]] != "", ]

# Missing some text we could get but will add it manually.
names(vv) = c("Bin", "Description", "Case")
write.csv(vv, "CSV/3810_column1.csv", row.names = FALSE)



breaks = c(1950, 2095, 3400, 3700)
abline(v = breaks, col = "red")
vv = readTable(a, colPos = c(0, breaks, Inf), offset = 30)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]

vv = vv[-c(1, seq(nrow(vv)-3, length = 4)), ]

write.csv(vv, "CSV/3810_column2.csv", row.names = FALSE)

#################



a = readRDS(file.path("~/Data/UCD_LEHMANN/OCRs", "UCD_Lehmann_3443.rds"))
class(a) = c("OCRResults", "data.frame")


breaks = c(460, 630, 2400, 2720, 3040, 3300)
abline(v = breaks, col = "red")

vv = readTable(a, colPos = c(0, breaks, Inf), offset = 30)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]

vv = vv[ !(vv[[1]] %in% c( "", "No.")), ]
names(vv) = c("Bin", "Description", "Regular", "Bottle", "Case")

vv[[3]] = gsub("^[^0-9]+", "", vv[[3]])
write.csv(vv, "CSV/3443.csv", row.names = FALSE)


##################################


pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_3483.jpg"))
pix3 = pixThresholdToBinary(pixConvertTo8(pix), 20)
plot(pix3)

a = GetBoxes(pix3)

breaks = c(1000, 1150, 2720, 3100, 3600)
abline(v = breaks, col = "red")
vv = readTable(a, colPos = c(0, breaks, Inf), offset = 30)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]


#########

a = readRDS(file.path("~/Data/UCD_LEHMANN/OCRs", "UCD_Lehmann_3913.rds"))
class(a) = c("OCRResults", "data.frame")


b  = a[a$right < 2000,]

breaks = c(325, 445, 1300, 1550, 1700, 2000)
abline(v = breaks, col = "green")

vv = readTable(b, colPos = c(0, breaks, Inf), offset = 30, splitVar = 3)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]

i = vv[[1]] != "" |  (c(vv[[3]][-1], "") != "") #  & isPrice(c(vv[[3]][-1], "")))
vv = vv[ i,]

g = cumsum(vv[[1]] != "" )
rows = split(vv, g)

vals = lapply(rows[-1], assembleRow)
vv = do.call(rbind, vals)

write.csv(vv, "CSV/3913.csv", row.names = FALSE)



####################

# Need to finish
a = readRDS(file.path("~/Data/UCD_LEHMANN/OCRs", "UCD_Lehmann_3232.rds"))
class(a) = c("OCRResults", "data.frame")

b = a[a$top > 3000,]

b[b$text %in% c("168", "CHATEAU", "2.69", "29.75"),]

breaks = c(940, 2750, 3110, 3450)
vv = readTable(b, colPos = c(0, breaks, Inf), offset = 30, splitVar = 3)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]



#####################

# NEED TO FINISH second column

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_3336.jpg"))
# 180 works, but 200 cuts off the other page.
pix2 = pixConvertTo8(pix)
pix4 = pixRotateAMGray(pix2, angle = pi/2)
pix3 = pixThresholdToBinary(pix4, 200)

plot(pix3)

a = GetBoxes(pix3)


b = a[ a$right < 2050,]

breaks = c(150, 300, 1575, 1750, 2000)
abline(v = breaks, col = "green")

vv = readTable(b, colPos = c(0, breaks, Inf), offset = 30, splitVar = 3)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]
rownames(vv) = NULL

vv = vv[- c(1, 22:25),]

names(vv) = c("Bin", "Description", "Bottle", "Case")
vv$Bottle = gsub("\\.\\.+", "", vv$Bottle)
vv[, 3:4] = lapply(vv[, 3:4], function(x) { i = grep("\\.", x, invert = TRUE); x[i] = gsub("^(.*)([0-9]{2})$", "\\1.\\2", x[i]) ; x})

vv[[2]] = gsub("\\.\\.+", "",  vv[[2]])
vv[[2]] = XML:::trim(vv[[2]])

write.csv(vv, "CSV/3336_coulmn1.csv", row.names = FALSE)

##

b = a[ a$right > 2050,]

breaks = c(2600, 2900, 1575, 1750, 2000)
abline(v = breaks, col = "green")

vv = readTable(b, colPos = c(0, breaks, Inf), offset = 30, splitVar = 3)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]
rownames(vv) = NULL



#################

pix = pixRead(file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_1544.jpg"))
#pix2 = pixRotate(pix, -pi*0.005)
pix3 = pixThresholdToBinary(pixConvertTo8(pix), 150)

plot(pix3)

a = GetBoxes(pix3)

a[a$text == "No.",]

# Find the Bin numbers
bins = a[a$left > 660 & a$left < 760 & grepl("^[0-9]+$", a$text),]

rows = lapply(1:nrow(bins), function(i)
                             a[abs(a$bottom - bins$bottom[i]) < 40 & a$left < 3000, ])


mkRow =
function(x){
    x = x[ order(x$left),]
    p = isPrice(x$text)
    n = length(x$text)
    data.frame(Bin = x$text[1],
               Description = paste(x$text[ - c(1,n-2, n-1, n)], collapse = " "),
               Case = x$text[n-2],
               CaseArrival = x$text[n-1])
}   

v = lapply(rows, mkRow)
vv = do.call(rbind, v)

write.csv(vv, "CSV/1544_coulmn1.csv", row.names = FALSE)


##
a[a$text == "47.88",]

# Find the Case price. Note we miss HARBOR MASTER as there are no Fifth prices for that.
bins = a[a$left > 4475 & a$left < 4580 & isPrice(a$text),]

rows = lapply(1:nrow(bins), function(i)
                             a[abs(a$bottom - bins$bottom[i]) < 40 & a$left > 3000 & a$left < 5400, ])

mkRow2 =
function(x){
    x = x[ order(x$left),]
    n = length(x$text)
    data.frame(
               Description = paste(x$text[ - ((n-3):n)], collapse = " "),
               FifthBottle = x$text[n-3],
               FifthCase = x$text[n-2],               
               QuartBottle = x$text[n-1],
               QuartCase = x$text[n],
               stringsAsFactors = FALSE)
}   

v = lapply(rows, mkRow2)
vv = do.call(rbind, v)

write.csv(vv, "CSV/1544_coulmn2.csv", row.names = FALSE)

######################################


j = file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_3392.jpg")
pix = pixRead(j)
pix3 = pixThresholdToBinary(pixConvertTo8(pix), 200)
a = GetBoxes(pix3)
plot(pix3)
#b  = a[a$right < 2000,]

i = a$text %in% c("No.", "Bottle", "Case") & a$left > 0

breaks = sort(c(a$left[i]- 25, a$right[i] + 25))
breaks[4:5] = mean(breaks[4:5])
breaks = unique(breaks)

abline(v = breaks, col = "green")

vv = readTable(a, colPos = c(0, breaks, Inf), offset = 30, splitVar = 3)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]

i = vv[[1]] != "" # |  (c(vv[[3]][-1], "") != "") #  & isPrice(c(vv[[3]][-1], "")))
vv = vv[ i,]

source("fixPrice.R")
vv[3:4] = lapply(vv[3:4], fixPrice)

idx = 1:(nrow(vv)-1)
j = (vv[[3]][idx] == vv[[3]][idx+1] & vv[[4]][idx] != vv[[4]][idx+1])
which(j)

write.csv(vv, "CSV/3392.csv", row.names = FALSE)

# gps for 325
# 22.35 for 22.95.  But the line above has 22.95 and the same bottle price.
# So should check if the bottle price is the same across rows and the case price
# is different by a character and the confidence


##########################


j = file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_1449.jpg")
pix = pixRead(j)
pix3 = pixThresholdToBinary(pixConvertTo8(pix), 200)
a = GetBoxes(pix3)
plot(pix3)

h = (a$top - a$bottom)
a = a[ h < .5 * nrow(pix),]

h = (a$top - a$bottom)
summary(h)

#b  = a[a$right < 2000,]

i = a$text %in% c("No.", "Bottle", "Case") & a$left > 0

breaks = sort(c(a$left[i]- 25, a$right[i] + 25))
breaks[4:5] = mean(breaks[4:5])
breaks = unique(breaks)

abline(v = breaks, col = "green")

vv = readTable(a, colPos = c(0, breaks, Inf), offset = 30, splitVar = 3)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]

i = vv[[1]] != "" # |  (c(vv[[3]][-1], "") != "") #  & isPrice(c(vv[[3]][-1], "")))
vv = vv[ i,]

source("fixPrice.R")
vv[3:4] = lapply(vv[3:4], fixPrice)

idx = 1:(nrow(vv)-1)
j = (vv[[3]][idx] == vv[[3]][idx+1] & vv[[4]][idx] != vv[[4]][idx+1])
which(j)

write.csv(vv, "CSV/1449.csv", row.names = FALSE)






======================================================

# 0036

j = file.path("~/Data/UCD_LEHMANN/UCD_Lehman JPEGs", "UCD_Lehmann_0036.jpg")
pix = pixRead(j)
pix = pixRotate(pix, -pi/2 * .005)
pix3 = pixThresholdToBinary(pixConvertTo8(pix), 180)
plot(pix3)
a = GetBoxes(pix3)

# Just the first side of the page/
breaks = c(325, 456, 1600, 1775, 2000)
breaks = unique(breaks)
abline(v = breaks, col = "green")

vv = readTable(a, colPos = c(0, breaks, Inf), offset = 30, splitVar = 3)
vv = vv[, - c(1, ncol(vv))]
vv = vv[!apply(vv, 1, function(x) all(x %in% c("", ".", "LL"))) ,]

i = vv[[1]] != "" # |  (c(vv[[3]][-1], "") != "") #  & isPrice(c(vv[[3]][-1], "")))
vv = vv[ i,]

source("~/DSIProjects/WinePrices/fixPrice.R")
vv[3:4] = lapply(vv[3:4], fixPrice)
