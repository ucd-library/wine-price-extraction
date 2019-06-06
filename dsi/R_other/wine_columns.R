#Wine columns
library(changepoint)

#
data1 #output of get boxes

# cdf of left location

# by index
data1.order = data1[order(data1$left),]
plot(data1.order$left, type = "l")

#by position
r = max(data1.order$right)
data1.cum = sapply(1:r, function(x) {sum(data1.order$left <= x)})
data1.lag = diff(data1.cum)
data1.lag[1] = 0

plot(1:r, data1.cum, type = "l")
tmp = cpt.var(data1.lag, method='PELT')
abline(v=tmp@cpts)
round(tmp@param.est$variance, 1)

test.image = tesseract("~/Documents/DSI/OCR_SherryLehmann/SampleCatalogPages/UCD_Lehmann_0011.jpg")
plot(test.image)
abline(v = tmp@cpts, col = "red")
abline(v = tmp@cpts[3], col = "blue")

#we can use this to find one that's after a space, tmp@param.est$variance should be close to zero
tmp@param.est$variance
