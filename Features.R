library(lawstat)
library(psych)
library(car)
library(MASS)
library(ggplot2)
library(graphics)
library(dplyr)
library(WRS)
library(WRS2)
library(clusterSim)

setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/FinalReferenceDataSet")
DayScale = 1

db.ref <- read.csv("db-nse-scaled.csv")
ch1.ref <- read.csv("cons-valid-0.csv")
sym.ref <- read.csv("V-Valid-Symmetrical.csv")
sym.ref$index <-  1:nrow(sym.ref)
sym.ref <- dplyr::select(sym.ref, Close.Price = Close.Price, Index = index)
sym.ref.scale <- data.Normalization (sym.ref,type="n1")

preprocess <- function(data){
  data$index <-  nrow(data):1
  data <- dplyr::select(data, Close = Close.Price, Index = index)
  data <- 100*data.Normalization (data,type="n4",normalization="column")
  data   
}

Vfeature <- function(stock.data){
  sym.test <- stock.data
  sym.test$index <- 1:nrow(sym.test)
  sym.test<- dplyr::select(sym.test, Close.Price = Close.Price, Index = index)
  sym.test.scale <- data.Normalization (sym.test,type="n1")
  
  frow <- nrow(sym.ref)
  trow <- nrow(sym.test)
  ratio <- trow /frow
  
  min.point.min1 <- 8/16
  min.point.max1 <- 10/16
  
  firsthalf.midpoint <- 6/16
  secondhalf.midpoint <- 12/16
  
  index.min.1 <- round(trow * min.point.min1,0)
  index.min.2 <- round(trow * min.point.max1,0)
  
  index.firsthalf <- round(trow * firsthalf.midpoint,0)
  index.secondhalf <- round(trow * secondhalf.midpoint,0)
  
  
  refval1 <- sym.ref.scale$Close.Price[1]
  refval2 <- mean( sym.ref.scale$Close.Price[8:10])
  refval3 <- mean( sym.ref.scale$Close.Price[6])
  refval4 <- mean( sym.ref.scale$Close.Price[12])
  refval5 <- sym.ref.scale$Close.Price[16]
  
  testval1 <- sym.test.scale$Close.Price[1]
  testval2 <- mean( sym.test.scale$Close.Price[index.min.1:index.min.2])
  testval3 <- mean( sym.test.scale$Close.Price[index.firsthalf])
  testval4 <- mean( sym.test.scale$Close.Price[index.secondhalf])
  testval5 <- sym.test.scale$Close.Price[trow]
  
  #Coefficient of variation of bot legs
  m1 <- mean(sym.test.scale$Close.Price[1:index.min.1])
  sd1 <- sd(sym.test.scale$Close.Price[1:index.min.1])
  m2 <- mean(sym.test.scale$Close.Price[index.min.2:trow])
  sd2 <- sd(sym.test.scale$Close.Price[index.min.2:trow])
  
  cv.first <-  m1/sd1
  cv.second <- m2/sd2
  
  diff1 <- Mod(refval1 - testval1)
  diff2 <- Mod(refval2 - testval2)
  diff3 <- Mod(refval3 - testval3)
  diff4 <- Mod(refval4 - testval4)
  diff5 <- Mod(refval5 - testval5)
  
  totaldiff <- diff1 + diff2 + diff3 + diff4 + diff5
  # featureval <- cv.first*cv.second*totaldiff
  #  featureval
  
}

wFeature2 <- function(stock.data){
  
  db.test <- stock.data
  frow <- nrow(db.ref)
  trow <- nrow(db.test)
  ratio <- trow /frow
  
  #  ind <- 1
  #  sapply(sort(db.ref$priceindex, index.return=TRUE), `[`, frow-ind+1)
  
  #min points 6, 9 29 31
  mid.point.min <- 13/40
  mid.point.max <- 19/40
  
  min.point.min1 <- 6/40
  min.point.max1 <- 9/40
  
  min.point.min2 <- 28/40
  min.point.max2 <- 31/40
  
  index.mid.1 <- round(trow * mid.point.min,0)
  index.mid.2 <- round(trow * mid.point.max,0)
  
  index.low1.1 <- round(trow * min.point.min1,0)
  index.low1.2 <- round(trow * min.point.max1,0)
  
  index.low2.1 <- round(trow * min.point.min2,0)
  index.low2.2 <- round(trow * min.point.max2,0)
  
  refval1 <- db.ref$priceindex[1]
  refval2 <- mean( db.ref$priceindex[13:19])
  refval3 <- mean( db.ref$priceindex[6:9])
  refval4 <- mean( db.ref$priceindex[28:31])
  refval5 <- db.ref$priceindex[40]
  
  testval1 <- db.test$priceindex[1]
  testval2 <- mean( db.test$priceindex[index.mid.1:index.mid.2])
  testval3 <- mean( db.test$priceindex[index.low1.1:index.low1.2])
  testval4 <- mean( db.test$priceindex[index.low2.1:index.low2.2])
  testval5 <- db.test$priceindex[trow]
  
  diff1 <- Mod(refval1 - testval1)
  diff2 <- Mod(refval2 - testval2)
  diff3 <- Mod(refval3 - testval3)
  diff4 <- Mod(refval4 - testval4)
  diff5 <- Mod(refval5 - testval5)
  
  totaldiff <- diff1 + diff2 + diff3 + diff4 + diff5
  
}


c1feature <- function(data.in){
  
  stockdata <- data.in
  #Plot : for debugging purpose only
  stockdata$index <-  nrow(stockdata):1
  
  stock.sd <- sd(stockdata$Close.Price)
  stock.mean <- mean(stockdata$Close.Price)
  stock.cv <- round(100 * stock.sd/stock.mean,2)
  
  stock.norm <- data.Normalization(stockdata$Close.Price, type = "n1" )
  stock.index <- data.Normalization(stockdata$index, type = "n1")
  model.lm <- lm( stock.norm~stock.index)
  summary(model.lm)
  model.slope <- round(100* (model.lm$coefficients[2]),3)
  c(stock.cv,model.slope)
}

generateFeatures <- function(stockData){
  testdata <- stockData
  ft1 <- Vfeature(testdata)
  mydata <- stockData
  mydata$index <-  nrow(stockData):1
  maxprice <- max(mydata$Close)
  minPrice <- min(mydata$Close)
  priceRange <- (maxprice - minPrice) 
  NoDays <- nrow(mydata)
  dayRatio = DayScale / NoDays
  scaled.x.axis <- mydata$index * dayRatio
  scaled.y.axis <- (mydata$Close - minPrice)/ priceRange
  mydata <- data.frame(X= mydata$index,dateindex = scaled.x.axis, priceindex = scaled.y.axis)
  ft2 <- wFeature2(mydata)
  mydata <- stockData
  ft3 <- c1feature(mydata)
  ft.all <- c(ft1,ft2,ft3)
}


plotstock <- function(data_in, name){
  
  par(mfrow = c(2,2))
  par(mar=c(1,1,1,1))
  plot(db.ref$priceindex~db.ref$dateindex ,type = "o")
  
  ch1.ref$index <- nrow(ch1.ref):1
  plot(ch1.ref$Close~ch1.ref$index, type = "o")
  
  k <- name
  data_in$index <-  nrow(data_in):1
  plot(data_in$Close~data_in$index, type = "o", main = k)
  
}
