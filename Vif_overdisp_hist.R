library(dplyr)
library(caTools)
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
library(mctest)
library(Metrics)
library(AER)

R2logit<- function(y,model){
  R2<- 1-(model$deviance/model$null.deviance)
  return(R2)
}


##setwd
setwd("C:/Users/user/Downloads/school/master thesis/rode kruis/Elena R")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-9.0.1')


# from csv -> not good because character
data <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/Elena R/fully_imputed_data.csv", header=TRUE, sep =  ",")
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

# test overdispersion
fit = glm(cases ~ .
          ,family="poisson",data=new_numeric_matrix) 

dispersiontest(fit, trafo = NULL, alternative = c('greater'))

# test multicollin
library(mctest)

y <- as.vector(data$cases)
x <- data[,c(2:39)]

imcdiag(x,y, na.rm = T, method = "VIF") 

# histograms
cases <- data$cases
hist(cases, xlab = "New Dengue Cases" )

fcs_var <- x[,c(25:37)]
multi.hist(fcs_var)

