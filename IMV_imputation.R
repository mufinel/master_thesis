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


## load data

# from csv 
luzon <- read.csv("C:/Users/Luzon.csv", header=TRUE, sep =  ",")
luzon <- luzon[,-c(1, 9, 28, 33, 34, 35, 36, 49, 50, 51, 37:48 )]

mindanao <- read.csv("C:/Users/Mindanao.csv", header=TRUE, sep =  ",")
mindanao <- mindanao[,-c(1, 9, 28, 33, 34, 35, 36, 49, 50, 51, 37:48 )]

visayas <- read.csv("C:/Users/Visayas.csv", header=TRUE, sep =  ",")
visayas <- visayas[,-c(1, 9, 28, 33, 34, 35, 36, 49, 50, 51, 37:48 )]

new_data <- rbind(luzon,mindanao,visayas)


###############################################################################################################################33
# knn on the weather variables

#numeric_luzon <- dplyr::select_if(luzon, is.numeric)
#numeric_mindanao <- dplyr::select_if(mindanao, is.numeric)
#numeric_visayas <- dplyr::select_if(visayas, is.numeric)

weather_imputed_luzon <- kNN(luzon, variable = colnames(luzon[,8:21]), k=3,imp_var = F)
weather_imputed_mindanao <- kNN(mindanao, variable = colnames(mindanao[,8:21]), k=3,imp_var = F)
weather_imputed_visayas <- kNN(visayas, variable = colnames(visayas[,8:21]), k=3,imp_var = F)

imputed_weather_data <- rbind(weather_imputed_luzon,weather_imputed_mindanao,weather_imputed_visayas)

write.csv(imputed_weather_data, file = "imputed_weather_data.csv")
