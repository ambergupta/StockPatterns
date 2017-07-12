setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject")
source("Features.r")
setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject")

load("stock.model.rda")

#----------------------Nifty 50----------------------------------------------
setwd("C:/AMBER/StockFetch/StockDataBase/Nifty50")
feature.signal = list()
listcsv <- dir(pattern = "*.csv")

for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  stockdata$index <- 1:nrow(stockdata)
  stockdata <- dplyr::select(stockdata, Close.Price = Close, Index = index)
  #plot(stockdata$Close.Price, type = "o")
  
  no.of.rows <- nrow(stockdata)
  start <- 10
  end <- no.of.rows - 1
  
  for(windowsize in start: end-2 )
  {
    if(start < 0) next
    print(windowsize)
    data <- stockdata[windowsize:end,]
    ft.all <- generateFeatures(data)
    V = ft.all[1]
    W = ft.all[2]
    HC1.VC = ft.all[3]
    HC1.Slope = ft.all[4]
    
    scale.V <- (V - mean.V)/sd.V
    scale.W <- (W - mean.W)/sd.W
    scale.HFC1.VC <- (HC1.VC - mean.HC1.VC)/sd.HC1.VC
    scale.HF1.Slope <- (HC1.Slope - mean.HC1.Slope)/sd.HC1.Slope
    
    dat <- data.frame(file = listcsv[k],
                      V = scale.V,
                      W = scale.W,
                      HC1.VC = scale.HFC1.VC,
                      HC1.Slope = scale.HF1.Slope)
    
    which.class <- predict (multinomModel, dat)
    if(which.class != 0){
      pat.found <- ifelse(which.class== 1, "C",
                          ifelse(which.class == 2, "V", "W"))
      
      feature.signal[[end - windowsize]] <- c(listcsv[k], (end - windowsize), pat.found)
    }
  }
  
}

signals = do.call(rbind, feature.signal)
colnames(signals) <- c("name","days","pattern")
signals <- data.frame(signals)
require(reshape2)
signals <- dcast(signals, name ~ days)
signals[is.na(signals)] <- 0

my.list <- apply(signals, 1, function(x){ k<- (x != 0); x[k]})
for(i in 1:length(my.list)){
  print( ((my.list[[i]] )))
  text1 <- paste(names(my.list[[1]]), collapse = "             ")
  text2 <- paste(my.list[[1]],collapse = "             ")
}

setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Output")
write.csv(signals, "Patter-Nifty50.csv")

setwd("C:/AMBER/StockFetch/StockDataBase/Nifty500")
feature.signal = list()
listcsv <- dir(pattern = "*.csv")

for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  stockdata$index <- 1:nrow(stockdata)
  stockdata <- dplyr::select(stockdata, Close.Price = Close, Index = index)
  #plot(stockdata$Close.Price, type = "o")
  
  no.of.rows <- nrow(stockdata)
  start <- 10
  end <- no.of.rows - 1
  
  for(windowsize in start: end-2 )
  {
    if(start < 0) next
    print(windowsize)
    data <- stockdata[windowsize:end,]
    ft.all <- generateFeatures(data)
    V = ft.all[1]
    W = ft.all[2]
    HC1.VC = ft.all[3]
    HC1.Slope = ft.all[4]
    
    scale.V <- (V - mean.V)/sd.V
    scale.W <- (W - mean.W)/sd.W
    scale.HFC1.VC <- (HC1.VC - mean.HC1.VC)/sd.HC1.VC
    scale.HF1.Slope <- (HC1.Slope - mean.HC1.Slope)/sd.HC1.Slope
    
    dat <- data.frame(file = listcsv[k],
                      V = scale.V,
                      W = scale.W,
                      HC1.VC = scale.HFC1.VC,
                      HC1.Slope = scale.HF1.Slope)
    
    which.class <- predict (multinomModel, dat)
    if(which.class != 0){
      pat.found <- ifelse(which.class== 1, "C",
                          ifelse(which.class == 2, "V", "W"))
      
      feature.signal[[end - windowsize]] <- c(listcsv[k], (end - windowsize), pat.found)
    }
  }
  
}

signals = do.call(rbind, feature.signal)
colnames(signals) <- c("name","days","pattern")
signals <- data.frame(signals)
require(reshape2)
signals <- dcast(signals, name ~ days)
signals[is.na(signals)] <- 0

my.list <- apply(signals, 1, function(x){ k<- (x != 0); x[k]})
for(i in 1:length(my.list)){
  print( ((my.list[[i]] )))
  text1 <- paste(names(my.list[[1]]), collapse = "             ")
  text2 <- paste(my.list[[1]],collapse = "             ")
}

setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Output")
write.csv(signals, "Patter-Nifty500.csv")


#----------------------NiftyMid100----------------------------------------------
setwd("C:/AMBER/StockFetch/StockDataBase/NiftyMid100")
feature.signal = list()
listcsv <- dir(pattern = "*.csv")

for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  stockdata$index <- 1:nrow(stockdata)
  stockdata <- dplyr::select(stockdata, Close.Price = Close, Index = index)

  no.of.rows <- nrow(stockdata)
  start <- 10
  end <- no.of.rows - 1
  
  for(windowsize in start: end-2 )
  {
    if(start < 0) next
    print(windowsize)
    data <- stockdata[windowsize:end,]
    ft.all <- generateFeatures(data)
    V = ft.all[1]
    W = ft.all[2]
    HC1.VC = ft.all[3]
    HC1.Slope = ft.all[4]
    
    scale.V <- (V - mean.V)/sd.V
    scale.W <- (W - mean.W)/sd.W
    scale.HFC1.VC <- (HC1.VC - mean.HC1.VC)/sd.HC1.VC
    scale.HF1.Slope <- (HC1.Slope - mean.HC1.Slope)/sd.HC1.Slope
    
    dat <- data.frame(file = listcsv[k],
                      V = scale.V,
                      W = scale.W,
                      HC1.VC = scale.HFC1.VC,
                      HC1.Slope = scale.HF1.Slope)
    
    which.class <- predict (multinomModel, dat)
    if(which.class != 0){
      pat.found <- ifelse(which.class== 1, "C",
                          ifelse(which.class == 2, "V", "W"))
      
      feature.signal[[end - windowsize]] <- c(listcsv[k], (end - windowsize), pat.found)
    }
  }
  
}

signals = do.call(rbind, feature.signal)
colnames(signals) <- c("name","days","pattern")
signals <- data.frame(signals)
require(reshape2)
signals <- dcast(signals, name ~ days)
signals[is.na(signals)] <- 0

my.list <- apply(signals, 1, function(x){ k<- (x != 0); x[k]})
for(i in 1:length(my.list)){
  print( ((my.list[[i]] )))
  text1 <- paste(names(my.list[[1]]), collapse = "             ")
  text2 <- paste(my.list[[1]],collapse = "             ")
}

setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Output")
write.csv(signals, "Patter-NiftyMid100.csv")


#----------------------NiftyMid100----------------------------------------------
setwd("C:/AMBER/StockFetch/StockDataBase/NiftySmall100")
feature.signal = list()
listcsv <- dir(pattern = "*.csv")

for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  stockdata$index <- 1:nrow(stockdata)
  stockdata <- dplyr::select(stockdata, Close.Price = Close, Index = index)
  no.of.rows <- nrow(stockdata)
  start <- 10
  end <- no.of.rows - 1
  
  for(windowsize in start: end-2 )
  {
    #start <- (no.of.rows - windowsize)
    if(start < 0) next
    print(windowsize)
    data <- stockdata[windowsize:end,]
    #plot(data$Close.Price, type = "o")
    
    ft.all <- generateFeatures(data)
    V = ft.all[1]
    W = ft.all[2]
    HC1.VC = ft.all[3]
    HC1.Slope = ft.all[4]
    
    scale.V <- (V - mean.V)/sd.V
    scale.W <- (W - mean.W)/sd.W
    scale.HFC1.VC <- (HC1.VC - mean.HC1.VC)/sd.HC1.VC
    scale.HF1.Slope <- (HC1.Slope - mean.HC1.Slope)/sd.HC1.Slope
    
    dat <- data.frame(file = listcsv[k],
                      V = scale.V,
                      W = scale.W,
                      HC1.VC = scale.HFC1.VC,
                      HC1.Slope = scale.HF1.Slope)
    
    which.class <- predict (multinomModel, dat)
    if(which.class != 0){
      pat.found <- ifelse(which.class== 1, "C",
                          ifelse(which.class == 2, "V", "W"))
      
      feature.signal[[end - windowsize]] <- c(listcsv[k], (end - windowsize), pat.found)
    }
  }
  
}

signals = do.call(rbind, feature.signal)
colnames(signals) <- c("name","days","pattern")
signals <- data.frame(signals)
require(reshape2)
signals <- dcast(signals, name ~ days)
signals[is.na(signals)] <- 0

my.list <- apply(signals, 1, function(x){ k<- (x != 0); x[k]})
for(i in 1:length(my.list)){
  print( ((my.list[[i]] )))
  text1 <- paste(names(my.list[[1]]), collapse = "             ")
  text2 <- paste(my.list[[1]],collapse = "             ")
}

setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Output")
write.csv(signals, "Patter-NiftySmall100.csv")

#----------------------NiftyMid100----------------------------------------------
setwd("C:/AMBER/StockFetch/StockDataBase/Nifty-Other")
feature.signal = list()
listcsv <- dir(pattern = "*.csv")

for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  stockdata$index <- 1:nrow(stockdata)
  stockdata <- dplyr::select(stockdata, Close.Price = Close, Index = index)
  #plot(stockdata$Close.Price, type = "o")
  
  no.of.rows <- nrow(stockdata)
  start <- 10
  end <- no.of.rows - 1
  
  for(windowsize in start: end-2 )
  {
    if(start < 0) next
    print(windowsize)
    data <- stockdata[windowsize:end,]
    ft.all <- generateFeatures(data)
    V = ft.all[1]
    W = ft.all[2]
    HC1.VC = ft.all[3]
    HC1.Slope = ft.all[4]
    
    scale.V <- (V - mean.V)/sd.V
    scale.W <- (W - mean.W)/sd.W
    scale.HFC1.VC <- (HC1.VC - mean.HC1.VC)/sd.HC1.VC
    scale.HF1.Slope <- (HC1.Slope - mean.HC1.Slope)/sd.HC1.Slope
    
    dat <- data.frame(file = listcsv[k],
                      V = scale.V,
                      W = scale.W,
                      HC1.VC = scale.HFC1.VC,
                      HC1.Slope = scale.HF1.Slope)
    
    which.class <- predict (multinomModel, dat)
    if(which.class != 0){
      pat.found <- ifelse(which.class== 1, "C",
                          ifelse(which.class == 2, "V", "W"))
      
      feature.signal[[end - windowsize]] <- c(listcsv[k], (end - windowsize), pat.found)
    }
  }
  
}

signals = do.call(rbind, feature.signal)
colnames(signals) <- c("name","days","pattern")
signals <- data.frame(signals)
require(reshape2)
signals <- dcast(signals, name ~ days)
signals[is.na(signals)] <- 0

my.list <- apply(signals, 1, function(x){ k<- (x != 0); x[k]})
for(i in 1:length(my.list)){
  print( ((my.list[[i]] )))
  text1 <- paste(names(my.list[[1]]), collapse = "             ")
  text2 <- paste(my.list[[1]],collapse = "             ")
}

setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Output")
write.csv(signals, "Patter-Nifty-Other.csv")
