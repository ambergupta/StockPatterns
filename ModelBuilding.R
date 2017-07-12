setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject")
source("Features.r")
#----------------------MAIN-----------------------------------
setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Consolidation")
print("creating consolidation dataset")
setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Consolidation")
#Directory Level Pattern test
datalist = list()
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  
  #plotstock(stockdata, listcsv[k])
  ft.all <- generateFeatures(stockdata)
  dat <- data.frame(file = listcsv[k], 
                    V = ft.all[1],
                    W = ft.all[2],
                    HC1.VC = ft.all[3],
                    HC1.Slope = ft.all[4])
  
  datalist[[k]] <- dat  
}

model.data.c = do.call(rbind, datalist)
model.data.c$Class <- 1
rownames(model.data.c) <- c(1:nrow(model.data.c))
model.data.c

setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Vpattern")
datalist = list()
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  #plotstock(stockdata, listcsv[k])
  ft.all <- generateFeatures(stockdata)
  dat <- data.frame(file = listcsv[k], 
                    V = ft.all[1],
                    W = ft.all[2],
                    HC1.VC = ft.all[3],
                    HC1.Slope = ft.all[4])
  
  datalist[[k]] <- dat  
}

model.data.v = do.call(rbind, datalist)
model.data.v$Class <- 2
rownames(model.data.v) <- c(1:nrow(model.data.v))
model.data.v


setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/DoubleBottom")
datalist = list()
ldf <- list() # creates a list
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  #plotstock(stockdata, listcsv[k])
  ft.all <- generateFeatures(stockdata)
  dat <- data.frame(file = listcsv[k], 
                    V = ft.all[1],
                    W = ft.all[2],
                    HC1.VC = ft.all[3],
                    HC1.Slope = ft.all[4])
  
  datalist[[k]] <- dat  
}

model.data.w = do.call(rbind, datalist)
model.data.w$Class <- 3
rownames(model.data.w) <- c(1:nrow(model.data.w))
model.data.w

library(plyr)
setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/Testdataset/Randomdata")
datalist = list()
listcsv <- dir(pattern = "*.csv") # creates the list of all the csv files in the directory
for (k in 1:length(listcsv)){
  stockdata <- read.csv(listcsv[k])
  #plotstock(stockdata, listcsv[k])
  ft.all <- generateFeatures(stockdata)
  dat <- data.frame(file = listcsv[k], 
                    V = ft.all[1],
                    W = ft.all[2],
                    HC1.VC = ft.all[3],
                    HC1.Slope = ft.all[4])
  
  datalist[[k]] <- dat  
}

model.data.random = do.call(rbind, datalist)
model.data.random$Class <- 0
rownames(model.data.random) <- c(1:nrow(model.data.random))
model.data.random


final.dataset <- rbind.fill(model.data.c, model.data.v, model.data.w, model.data.random)
final.dataset 



#----------------Model Building--------------------------
#Descriptive Stats
require(plyr)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)


final.dataset$Class <- as.factor(final.dataset$Class)
final.dataset <- na.omit(final.dataset)

set.seed(100)
trainingRows <- sample(1:nrow(final.dataset), 0.7*nrow(final.dataset))
training <- final.dataset[trainingRows, ]
test <- final.dataset[-trainingRows, ]

mean.V <- mean(final.dataset$V)
sd.V   <- sd(final.dataset$V)
mean.W <- mean(final.dataset$W)
sd.W   <- sd(final.dataset$W)
mean.HC1.VC <- mean(final.dataset$HC1.VC)
sd.HC1.VC   <- sd(final.dataset$HC1.VC)
mean.HC1.Slope <- mean(final.dataset$HC1.Slope)
sd.HC1.Slope   <- sd(final.dataset$HC1.Slope)

training$V <- (training$V - mean.V)/sd.V
training$W <- (training$W - mean.W)/sd.W
training$HC1.VC <- (training$HC1.VC - mean.HC1.VC)/sd.HC1.VC
training$HC1.Slope <- (training$HC1.Slope - mean.HC1.Slope)/sd.HC1.Slope

test$V <- (test$V - mean.V)/sd.V
test$W <- (test$W - mean.W)/sd.W
test$HC1.VC <- (test$HC1.VC - mean.HC1.VC)/sd.HC1.VC
test$HC1.Slope <- (test$HC1.Slope - mean.HC1.Slope)/sd.HC1.Slope


multinomModel <- multinom(Class ~ V + W + HC1.VC + HC1.Slope, data = training)
summary(multinomModel)

predicted.scores <- predict (multinomModel, test, "probs")
predicted.class <- predict (multinomModel, test)

test$predictedClass <- predicted.class
table(test$Class)
table(test$predictedClass)
perf.table <- table(test$predictedClass, test$Class)

true.positve <- diag(perf.table)
false.positive <- colSums(perf.table) - diag(perf.table)
false.negative <- rowSums(perf.table) - diag(perf.table)
true.negative <-   c(sum(perf.table[-1,-1]), sum(perf.table[-2,-2]),
                     sum(perf.table[-3,-3]), sum(perf.table[-4,-4]))

T.P.R <- true.positve/(true.positve + false.negative)
F.P.R <- false.positive/(true.negative + false.positive)

accuracy <- sum(diag(perf.table)) /sum(perf.table)
misclass <- 100 * mean((test$predictedClass != test$Class)) 
data.misclassified <- test[test$Class != test$predictedClass,]
setwd("C:/AMBER/Great Lakes/ProjectWorks/capstoneproject/")
save(multinomModel, file = "stock.model.rda")


  