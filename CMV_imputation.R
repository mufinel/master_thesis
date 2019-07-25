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



# load data

# from csv 
data <- read.csv("C:/Users/imputed_weather_no_NA_Hog-pop.dens_data.csv", header=TRUE, sep =  ",")
data <- data[,-1]


#separate soil variables
soil <- data[,22:25]
no_soil_data <- data[,-c(1,2,4,22:25)]
model_data <- data[,c(3,8:21,22:25)]
no_model_data <- data[,c(1,2,4:7,26:43)]

# iteratiive model - based imputation 
imputed_model_data <- c()
imputed_model_data <- irmi(model_data, eps = 5, maxit = 100, mixed = NULL, mixed.constant = NULL,
     count = NULL, step = FALSE, robust = FALSE, takeAll = TRUE,
     noise = TRUE, noise.factor = 1, force = FALSE, robMethod = "MM",
     force.mixed = TRUE, mi = 1, addMixedFactors = FALSE, trace = FALSE,
     init.method = "kNN", modelFormulas = NULL, multinom.method = "multinom",
     imp_var = FALSE, imp_suffix = "imp")

# put the matrix back together
imputed_soil_data <- cbind(imputed_model_data, no_model_data)
imputed_soil_data <- imputed_soil_data[,c(20,21,1,22,23:25,2:19,26:43)]

# write csv for data
write.csv(imputed_soil_data, file = "imputed_soil_data.csv")
