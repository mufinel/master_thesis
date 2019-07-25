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

# from csv -> not good because character
data <- read.csv("C:/Users/imputed_weather_data.csv", header=TRUE, sep =  ",")
data <- data[,-1]

# get rid of missing in hog, pop dens and so on
new_data <- subset(data, !is.na(data$pop.dens.km2))
new_data <- subset(new_data, !is.na(new_data$Hog))

#check
perc_NA <- as.vector(colMeans(is.na(new_data)))

# write csv
write.csv(new_data, file = "imputed_weather_no_NA_Hog-pop.dens_data.csv")
