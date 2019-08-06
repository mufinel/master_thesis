##load packages

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



##setwd
setwd("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-9.0.1')


## load data
raw_data <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts/raw_file_elena.csv", header=TRUE, sep =  ",")


## make data nice
manipulate_data <- raw_data


months <- c("jan","feb","mar","apr","may","jun", "jul",
            "aug", "sep", "oct", "nov", "dec")
province <- as.character(unique(manipulate_data$province))
code <- as.character(unique(manipulate_data$code))

nr_months <- as.numeric(length(months))
nr_province <- as.numeric(length(unique(manipulate_data$province)))
nr_code <- as.numeric(length(unique(manipulate_data$code)))

# make list for every month
new_data <- list()
full_data_month <- list()

for(i in 1:12)
{
  new_data[[i]] <- manipulate_data %>% filter(month == months[i])
  miss_prov <- as.character(province[!province %in% as.character(new_data[[i]]$province[new_data[[i]]$month == months[i]])])
  miss_code <- as.character(code[!code %in% as.character( new_data[[i]]$code[ new_data[[i]]$month == months[i]])])
  
  month_data <-  new_data[[i]][ new_data[[i]]$month == months[i],]
  
  month_vec <- rep(months[i], length(miss_prov))
  
  add_data <- matrix(nrow = length(miss_prov),ncol = ncol( new_data[[i]]))
  add_data[,c(2,3,5)] <- c(miss_code, month_vec,miss_prov)
  names = colnames(new_data[[i]])
  colnames(add_data) = names
  
  full_data_month[[i]] <- rbind(month_data, add_data) 
}

final_data <- bind_rows(full_data_month)

write.csv(final_data, file = "final_data_elena.csv")

rm(list=ls())

