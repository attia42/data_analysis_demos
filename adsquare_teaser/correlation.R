library('rjson')
library('psych')
library('Hmisc')
file <- 'adsquare_teaser_dataMay2014.json'
data <- fromJSON(paste(readLines(file), collapse=""))

x1 <- data[[2]]
x2 <- data[[3]]
x3 <- data[[4]]
x4 <- data[[5]]
x5 <- data[[6]]
ern <- data[[7]]
z <- data[[8]]


x1 = sapply(seq(100), function(i) x1[[i]])
x2 = sapply(seq(100), function(i) x2[[i]])
x3 = sapply(seq(100), function(i) x3[[i]])
x4 = sapply(seq(100), function(i) x4[[i]])
x5 = sapply(seq(100), function(i) x5[[i]])
ern = sapply(seq(100), function(i) ern[[i]])

z <- sapply(seq(100), function(i) z[[i]])

z <- lapply(z, as.numeric)
z <- as.numeric(z)


d = data.frame(x1,x2,x3,x4,x5, z, ern)
describe(d)
rcorr(as.matrix(d), type="pearson")
rcorr(as.matrix(d), type="spearman")
plot(d$ern, type='l', col="red")
par(new=T)
plot(d$x4, type='l', col="blue")




zn = z[!is.na(z)]
x4n  = x4[!is.na(z)]
mnX4n = min(x4n)
mxX4n = max(x4n)
mnZn = min(zn)
mxZn = max(zn)
calcZ <- function(xn)(((xn - mnX4n)* (mxZn - mnZn))/ (mxX4n - mnX4n))+mnZn
plot(zn, col="red", type='l')
par(new=T)
#plot(x4n, col="blue", type='l')
par(new=T)
plot(calcZ(x4n), col="green", type='l')

