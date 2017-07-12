#-------------------------------------
#Place to do experiments 

#1-TimeScaling 
#2- Hough Transform
#3- Nifty Stock List Preparation
#4- Quantmod usages
#-------------------------------------


setwd("C:/AMBER/StockFetch/StockDataBase")
db.ref <- read.csv("THOMASCOOK.csv")

# Experiment with moving window to scale time scale axis data
require(zoo)

#standard.data <- c(1,2,3)
#length.std <- length(standard.data)
plot(db.ref$Close, type = "o")

png(filename="C:/AMBER/StockFetch/StockDataBase/name.bmp")
plot(db.ref$Close, type = "o")
dev.off()


length.std = 30

#live.data <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
live.data <- db.ref$Close
length.live <- nrow(db.ref)

N <- length.live%/%length.std
M <- length.live%%length.std

windowsize <- N+M

TS <- zoo(live.data)
output <- rollapply(TS, width = windowsize, by = N, FUN = mean, align = "left")
plot(output, type = "o")




#---------------Hough Transform-----------------------
library(PET)
library(pracma)

a=matrix(rep(0,10000), 100, 100)
for (i in 1:100)
{a[i,60]=1
}

d=matrix(rep(0,10000), 100, 100)
for (i in 1:100)
{d[60,i]=1
}

e=matrix(diag(100), 100)

getLineHough<-function(p){
  abc=hough(p)
  #get the brightest point in the hough tranform
  maxPoint<-which(abc$hData==max(abc$hData),arr.ind=T)
  #if there is only one line, can average the results in case there are several brightest points
  maxPoint<-apply(maxPoint,2,mean)
  
  houghParam<-unlist(abc$Header)
  
  theta=(maxPoint[1]-1)*houghParam["DeltaXY1"]+houghParam["XYmin1"]
  rho=(maxPoint[2]-1)*houghParam["DeltaXY2"]+houghParam["XYmin2"]
  
  a<--cot(theta)
  b<-rho/sin(theta)
  
  
  par(mfrow=c(1,2))
  image(p,main="original")
  #add the predicted lines, also have to change the slope and intercept because
  #the origin of the plot function is not the center of the image the bottom left corner
  if(theta==0){
    abline(v=(rho+50)/100)
  } else{
    abline((b+50-a*50)/100,a)
  }
  image(abc$hData, main="Houghmatrix")
}

getLineHough(a)
getLineHough(d)
getLineHough(e)


#----------------------NIFTY STOCK PREPARATION----------
setwd("C:/AMBER/StockFetch")
Nifty50 <- read.csv("Nifty50.csv")
Nifty500 <- read.csv("Nifty500.csv")
NiftyMid100 <- read.csv("NiftyMid100.csv")
NiftySmall100 <- read.csv("NiftySmall100.csv")
NiftyAll <- read.csv("All-NSE.csv")

NiftyOther <- setdiff(NiftyAll$SYMBOL, Nifty500$Symbol)
NiftyOther <- setdiff(NiftyAll$SYMBOL, Nifty50$Symbol)
NiftyOther <- setdiff(NiftyAll$SYMBOL, NiftyMid100$Symbol)
NiftyOther <- setdiff(NiftyAll$SYMBOL, NiftySmall100$Symbol)


NiftyOther <- as.data.frame(NiftyAll$SYMBOL)

names(NiftyOther) <- c("Symbol")
write.csv(NiftyOther, "C:/AMBER/StockFetch/Nifty-Other.csv",row.names = FALSE)

#--------------------Quantmod usages----------------------
require(quantmod)

find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

data.stock <- read.csv("C:/AMBER/StockFetch/StockDataBase/Experiment/ONGC.csv")
d <- data.stock[0:40,]
plot(d$Close, type = "o")
p <- findPeaks(d$Close, 2)
d2 <- d[p,]
plot(d2$Close, type = "o")

v <- findValleys(d$Close, 2)
d3 <- d[v,]
plot(d3$Close, type = "o")
