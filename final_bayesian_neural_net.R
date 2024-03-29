library(dplyr)
library(ggplot2)
library(reshape2)
library(plyr)
library(gdata)
library(sparklyr)
library(tidyr)
library(psych)
library(moments)
library(VIM)
library(cluster)
library(MASS)
library(Hmisc)
library(HH)
library(odbc)
library(lattice)
library(mice)
library(VIM)
library(clusterSim)
library(RODBC)
library(FactoMineR)
library(corrplot)
library(functional)
library(lme4)
library(tidyverse)
library(caret)
library(readxl)
library(brnn)
library(fastDummies)
library(data.table)
library(VIM)
library(jomo)
library(mice)
library(rpart)
library(randomForest)
library(rpart.plot)
library(caTools)
library(brnn)


## load data

# from csv 
data <- read.csv("C:/Users/fully_imputed_data.csv", header=TRUE, sep =  ",")
data <- data[,-c(1,5)]


# rename variables

my_data <- as_tibble(data)

names(my_data)[names(my_data) == "incidence"]               <- "cases"
names(my_data)[names(my_data) == "Number.of.Households.y"]  <- "Number.of.Households"
names(my_data)[names(my_data) == "own.fauc.com.wat.sys.y"]  <- "own.fauc.com.wat.sys"
names(my_data)[names(my_data) == "shar.fauc.com.wat.sys.y"] <- "shar.fauc.com.wat.sys"
names(my_data)[names(my_data) == "own.tub.pip.dep.well.y"]  <- "own.tub.pip.dep.well"
names(my_data)[names(my_data) == "shar.tub.pip.dep.well.y"] <- "shar.tub.pip.dep.well"
names(my_data)[names(my_data) == "tub.pip.shal.well.y"]     <- "tub.pip.shal.well"
names(my_data)[names(my_data) == "dug.well.y"]              <- "dug.well"
names(my_data)[names(my_data) == "protec.spring.y"]         <- "protec.spring"
names(my_data)[names(my_data) == "unprotec.spring.y"]       <- "unprotec.spring"
names(my_data)[names(my_data) == "lake.riv.rain.y"]         <- "lake.riv.rain"
names(my_data)[names(my_data) == "peddler.y"]               <- "peddler"
names(my_data)[names(my_data) == "bottled.wat.y"]           <- "bottled.wat"
names(my_data)[names(my_data) == "others.y"]                <- "others"

data <- my_data


# make scales fit
# make variables not have higher weights
numeric_matrix <- data
mean_vec <- c()
mean_vec <- as.data.frame(colMeans(numeric_matrix, na.rm = T))

new_numeric_matrix <- numeric_matrix

for(i in 1:nrow(mean_vec))
{
  if(mean_vec[i,]>10  && mean_vec[i,]<100)
  {
    new_numeric_matrix[,i] <- numeric_matrix[,i]/1
  }
  else if(mean_vec[i,]>100  && mean_vec[i,]<1000)
  {
    new_numeric_matrix[,i] <- numeric_matrix[,i]/1
  }
  else if(mean_vec[i,]>1000  && mean_vec[i,]<10000)
  {
    new_numeric_matrix[,i] <- numeric_matrix[,i]/1000
  }
  else if(mean_vec[i,]>10000  && mean_vec[i,]<100000)
  {
    new_numeric_matrix[,i] <- numeric_matrix[,i]/10000
  }
  else if(mean_vec[i,]>100000  && mean_vec[i,]<1000000)
  {
    new_numeric_matrix[,i] <- numeric_matrix[,i]/100000
  }
  else if(mean_vec[i,]>1000000  && mean_vec[i,]<10000000)
  {
    new_numeric_matrix[,i] <- numeric_matrix[,i]/1000000
  }
}


# split data into training and test
set.seed(22)
split = sample.split(new_numeric_matrix$cases, SplitRatio = 0.8)
train_matrix = subset(new_numeric_matrix, split==TRUE)
test_matrix = subset(new_numeric_matrix, split==FALSE)

train_x <- as.matrix(train_matrix[,2:39])
train_y <- as.vector(train_matrix$cases)

test_x <- as.matrix(test_matrix[,2:39])
test_y <- as.vector(test_matrix$cases)

## bayesian neural net
bayes_net <- brnn(train_x, train_y, neurons=2)

# prediction
bayes_predict <- predict(bayes_net,test_x)

# rmse
bayes_rmse <- RMSE(bayes_predict, test_y)

# mase
MASE_bayes <- mase(test_y, bayes_predict)
#MASE_bayes_2 <- mase(test_y, bayes_predict, step_size = 12)

# maape
library(TSrepr)
MAAPE_bayes <- maape(test_y, bayes_predict)

# mape 
test_y_zero <- test_y == 0
bayes_predict_nozero <- subset(bayes_predict, test_y_zero == FALSE)
test_y_nozero <- test_y[ test_y != 0 ]
MAPE_bayes_nozero <- mape(test_y_nozero, bayes_predict_nozero)

# mae
test_y_zero <- test_y == 0
bayes_predict_zero <- subset(bayes_predict, test_y_zero == TRUE)
test_y_zero <- test_y[ test_y == 0 ]
MAE_bayes_zero <- mae(test_y_zero, bayes_predict_zero)



# get coefficients
neuron_1 <- bayes_net$theta[[1]]
neuron_2 <- bayes_net$theta[[2]]
variables <- c('weight', 'bias', names(data[2:39]))
bayes_coef <- cbind(variables, neuron_1, neuron_2)

other_coef <- coef.brnn(bayes_net)
View(other_coef)


