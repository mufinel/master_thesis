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

R2logit<- function(y,model){
  R2<- 1-(model$deviance/model$null.deviance)
  return(R2)
}



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

# model poisson
summary(data.quasipois <- glm(cases ~ 
                                pop.nr.male +
                                pop.nr.female +
                                rain.val.mm +
                                air.tem.max +
                                Cattle +
                                own.fauc.com.wat.sys +
                                own.tub.pip.dep.well +
                                unprotec.spring   +    
                                lake.riv.rain  +
                                bottled.wat +
                                pop.dens.km2
                              , family="quasipoisson", train_matrix))

# R^2
r_sq_quasi <-R2logit(train_y,data.quasipois)

# RMSE
predicted_quasipois <- predict(data.quasipois, test_matrix)
rmse_quasipois <- RMSE(predicted_quasipois, test_matrix$cases)

# MASE

MASE_quasi <- mase(test_y, predicted_quasipois)
#MASE_quasi2 <- mase(test_y, predicted_quasipois, step_size = 12)

# maape
library(TSrepr)
MAAPE_qpen <- maape(test_y, predicted_quasipois)


# display
coeff <-  summary(data.quasipois)$coefficients

# check overdisp
fit = glm(cases ~ 
            pop.nr.male +
            pop.nr.female +
            rain.val.mm +
            air.tem.max +
            Cattle +
            own.fauc.com.wat.sys +
            own.tub.pip.dep.well +
            unprotec.spring   +    
            lake.riv.rain  +
            bottled.wat +
            pop.dens.km2
          ,family="poisson",data=train_matrix) 

fit.overdisp = glm(cases ~ 
                     pop.nr.male +
                     pop.nr.female +
                     rain.val.mm +
                     air.tem.max +
                     Cattle +
                     own.fauc.com.wat.sys +
                     own.tub.pip.dep.well +
                     unprotec.spring   +    
                     lake.riv.rain  +
                     bottled.wat +
                     pop.dens.km2
                   ,family="quasipoisson",data=train_matrix) 


summary(fit.overdisp)$dispersion # dispersion coefficient
pchisq(summary(fit.overdisp)$dispersion * fit$df.residual, fit$df.residual, lower = F) # significance for overdispersion

library(AER)
disp_test <- dispersiontest(fit,trafo=NULL)
disp_test$p.value
disp_test$estimate


