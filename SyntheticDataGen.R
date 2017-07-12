library(lawstat)
library(psych)
library(car)
library(MASS)
library(Rcmdr)
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

synthetic <- function(stock.data, name){

  par(mfrow = c(2,2))
  db.test <- stock.data
  price  <- db.test$Close.Price
  n <- nrow(db.test)
  amp <- .005 * mean(price)
  error <- rnorm(n)
  
  index <- 1:nrow(stock.data)
  plot(index, price, type = "o")
  price2 <- price + error* amp
  plot(index , price2, type = "o")
  
  out <- data.frame(index = index, Close.Price = price2)
  name.synthetic <- paste("synthetic 3-",name)
  write.csv(out, name.synthetic)
  
}

plotstock <- function(data_in, name ){
  
  par(mfrow = c(2,2))
  par(mar=c(1,1,1,1))
  plot(db.ref$priceindex~db.ref$dateindex ,type = "o")
  
  ch1.ref$index <- nrow(ch1.ref):1
  plot(ch1.ref$Close~ch1.ref$index, type = "o")
  
  data_in$index <-  nrow(data_in):1
  k <- name
  plot(data_in$Close~data_in$index, type = "o", main = k)
  
}

setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Syntheticdata")
datalist = list()
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  synthetic(stockdata, listcsv[k])
}

setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Syntheticdata")
datalist = list()
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  plotstock(stockdata, "testing")
  retValue <- generateFeatures(stockdata)
  names(retValue) <- c("V","W","HC1.CV", "HC1.Slope")
  print(retValue)
}
