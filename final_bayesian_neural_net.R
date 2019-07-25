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
data <- read.csv("C:/Usersfully_imputed_data.csv", header=TRUE, sep =  ",")
data <- data[,-c(1,5)]


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
split = sample.split(new_numeric_matrix$incidence, SplitRatio = 0.8)
train_matrix = subset(new_numeric_matrix, split==TRUE)
test_matrix = subset(new_numeric_matrix, split==FALSE)

train_x <- as.matrix(train_matrix[,2:39])
train_y <- as.vector(train_matrix$incidence)

test_x <- as.matrix(test_matrix[,2:39])
test_y <- as.vector(test_matrix$incidence)

## bayesian neural net
bayes_net <- brnn(incidence ~   rain.val.mm             + rain.int.mm.hr         + wind.spe.max            + air.tem.                + relative.hum.min +          
                    air.pre                 + dew.poi                + relative.hum            + sunshine.dur.hour       + solar.rad.w.m2 +       
                    soil.moi.vwc            + soil.moi.cm            + soil.tem.mean           + soil.tem.mean2          + carabao +                
                    Cattle                  + Goat                   + Hog                     + Number.of.Households.y  + own.fauc.com.wat.sys.y +
                    shar.fauc.com.wat.sys.y + own.tub.pip.dep.well.y + shar.tub.pip.dep.well.y + tub.pip.shal.well.y     + dug.well.y +             
                    protec.spring.y         + unprotec.spring.y      + lake.riv.rain.y         + peddler.y               + bottled.wat.y +          
                    others.y                + pop.dens.km2           + pop.nr.female           + pop.nr.male             + wind.spe.kph +
                    air.tem.max             + air.tem.min            + relative.hum.max                   
                  , data=train_matrix, neurons=2)

# predictive power
bayes_predict <- predict(bayes_net,test_x)
bayes_sse = sum((bayes_predict - test_matrix$incidence)^2)
bayes_sse

# rmse
bayes_rmse <- RMSE(bayes_predict, test_y)

# compare predictions vs original
bayes_nb_orig <- cbind(bayes_predict, test_y)

