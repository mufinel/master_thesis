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

##setwd
setwd("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-9.0.1')


######################################################################################################################
# pattern
######################################################################################################################


## load data

# from csv -> not good because character
data_imputed <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/Elena R/fully_imputed_data.csv", header=TRUE, sep =  ",")
data_imputed_pattern <- data_imputed[,-c(1)]

data_original <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/Elena R/final_data_elena.csv", header=TRUE, sep =  ",")
data_original_pattern <- data_original[,-c(1,2,3,5,9, 28, 33, 34, 35, 36, 37, 50, 51, 38:49 )]

# rename variables data_imputed_pattern
my_data <- as_tibble(data_imputed_pattern)

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

data_imputed_pattern <- my_data

# rename variables data_original_pattern
my_data <- as_tibble(data_original_pattern)

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


data_original_pattern <- my_data

## Geometric diagnostic
# Density plot rain.val.mm
d1 <- density(data_imputed_pattern$rain.val.mm)
d2 <- density(data_original_pattern$rain.val.mm, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "rain.val.mm after pattern imputation density plots", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("right", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))

# Density plot wind.spe.kph

d1 <- density(data_imputed_pattern$wind.spe.kph)
d2 <- density(data_original_pattern$wind.spe.kph, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "wind.spe.kph after pattern imputation density plots", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("right", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))


# Density plot soil.moi.cm 
d1 <- density(data_imputed_pattern$soil.moi.cm)
d2 <- density(data_original_pattern$soil.moi.cm, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "Soil.moi.cm after pattern imputation density plots", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("right", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))

# Density plot soil.tem.mean
d1 <- density(data_imputed_pattern$soil.tem.mean)
d2 <- density(data_original_pattern$soil.tem.mean, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "soil.tem.mean after pattern imputation density plots", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("left", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))



# Density plot Number.of.Households
d1 <- density(data_imputed_pattern$Number.of.Households)
d2 <- density(data_original_pattern$Number.of.Households, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "Number.of.Households after pattern imputation density plots", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("right", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))



# Density plot relative.hum.min
d1 <- density(data_imputed_pattern$relative.hum.min)
d2 <- density(data_original_pattern$relative.hum.min, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "relative.hum.min after pattern imputation density plots", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("right", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))

# Density plot air.tem.
d1 <- density(data_imputed_pattern$air.tem.)
d2 <- density(data_original_pattern$air.tem., na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "air.tem. density plots pattern imputation", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("left", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))

# Density plot soil.moi.vwc
d1 <- density(data_imputed_pattern$soil.moi.vwc)
d2 <- density(data_original_pattern$soil.moi.vwc, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y+0.03), type = "n", main = "soil.moi.vwc density plots pattern imputation", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("right", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))

# Density plot own.fauc.com.wat.sys
d1 <- density(data_imputed_pattern$own.fauc.com.wat.sys)
d2 <- density(data_original_pattern$own.fauc.com.wat.sys, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "own.fauc.com.wat.sys density plots pattern imputation", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("right", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))

# Density plot relative.hum.max
d1 <- density(data_imputed_pattern$relative.hum.max)
d2 <- density(data_original_pattern$relative.hum.max, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "relative.hum.max density plots pattern imputation", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("left", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))


# Density plot wind.spe.max
d1 <- density(data_imputed_pattern$wind.spe.max)
d2 <- density(data_original_pattern$wind.spe.max, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "wind.spe.max density plots pattern imputation", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("right", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))

## Numeric diagnostic

# diff in mean greater than 2 st deviations rain.val.mm
sd_weather_orig <- sd(data_original_pattern$rain.val.mm, na.rm=T)
sd_weather_imp <- sd(data_imputed_pattern$rain.val.mm, na.rm=T)

abs_mean_weather_imp <- mean(data_imputed_pattern$rain.val.mm)
abs_mean_weather_orig <- mean(data_original_pattern$rain.val.mm, na.rm = T)

z.test2sam = function(a, b, var.a, var.b){
  n.a = length(a)
  n.b = length(b)
  zeta = abs((mean(a) - mean(b))) / (sqrt(var.a/n.a + var.b/n.b))
  return(zeta)
}

z.test2sam(abs_mean_weather_imp,abs_mean_weather_orig, sd_weather_imp, sd_weather_orig )

# wind.spe.kph
sd_weather_orig <- sd(data_original_pattern$wind.spe.kph, na.rm=T)
sd_weather_imp <- sd(data_imputed_pattern$wind.spe.kph, na.rm=T)

abs_mean_weather_imp <- mean(data_imputed_pattern$wind.spe.kph)
abs_mean_weather_orig <- mean(data_original_pattern$wind.spe.kph, na.rm = T)

z.test2sam = function(a, b, var.a, var.b){
  n.a = length(a)
  n.b = length(b)
  zeta = abs((mean(a) - mean(b))) / (sqrt(var.a/n.a + var.b/n.b))
  return(zeta)
}

z.test2sam(abs_mean_weather_imp,abs_mean_weather_orig, sd_weather_imp, sd_weather_orig )

# relative.hum.min
sd_weather_orig <- sd(data_original_pattern$relative.hum.min, na.rm=T)
sd_weather_imp <- sd(data_imputed_pattern$relative.hum.min, na.rm=T)

abs_mean_weather_imp <- mean(data_imputed_pattern$relative.hum.min)
abs_mean_weather_orig <- mean(data_original_pattern$relative.hum.min, na.rm = T)

z.test2sam = function(a, b, var.a, var.b){
  n.a = length(a)
  n.b = length(b)
  zeta = abs((mean(a) - mean(b))) / (sqrt(var.a/n.a + var.b/n.b))
  return(zeta)
}

z.test2sam(abs_mean_weather_imp,abs_mean_weather_orig, sd_weather_imp, sd_weather_orig )

# Number.of.Households
sd_weather_orig <- sd(data_original_pattern$Number.of.Households, na.rm=T)
sd_weather_imp <- sd(data_imputed_pattern$Number.of.Households, na.rm=T)

abs_mean_weather_imp <- mean(data_imputed_pattern$Number.of.Households)
abs_mean_weather_orig <- mean(data_original_pattern$Number.of.Households, na.rm = T)

z.test2sam = function(a, b, var.a, var.b){
  n.a = length(a)
  n.b = length(b)
  zeta = abs((mean(a) - mean(b))) / (sqrt(var.a/n.a + var.b/n.b))
  return(zeta)
}

z.test2sam(abs_mean_weather_imp,abs_mean_weather_orig, sd_weather_imp, sd_weather_orig )

# wind.spe.max
sd_weather_orig <- sd(data_original_pattern$wind.spe.max, na.rm=T)
sd_weather_imp <- sd(data_imputed_pattern$wind.spe.max, na.rm=T)

abs_mean_weather_imp <- mean(data_imputed_pattern$wind.spe.max)
abs_mean_weather_orig <- mean(data_original_pattern$wind.spe.max, na.rm = T)

z.test2sam = function(a, b, var.a, var.b){
  n.a = length(a)
  n.b = length(b)
  zeta = abs((mean(a) - mean(b))) / (sqrt(var.a/n.a + var.b/n.b))
  return(zeta)
}

z.test2sam(abs_mean_weather_imp,abs_mean_weather_orig, sd_weather_imp, sd_weather_orig )

# soil.tem.mean
sd_weather_orig <- sd(data_original_pattern$soil.tem.mean, na.rm=T)
sd_weather_imp <- sd(data_imputed_pattern$soil.tem.mean, na.rm=T)

abs_mean_weather_imp <- mean(data_imputed_pattern$soil.tem.mean)
abs_mean_weather_orig <- mean(data_original_pattern$soil.tem.mean, na.rm = T)

z.test2sam = function(a, b, var.a, var.b){
  n.a = length(a)
  n.b = length(b)
  zeta = abs((mean(a) - mean(b))) / (sqrt(var.a/n.a + var.b/n.b))
  return(zeta)
}

z.test2sam(abs_mean_weather_imp,abs_mean_weather_orig, sd_weather_imp, sd_weather_orig )

# soil.moi.cm
sd_weather_orig <- sd(data_original_pattern$soil.moi.cm, na.rm=T)
sd_weather_imp <- sd(data_imputed_pattern$soil.moi.cm, na.rm=T)

abs_mean_weather_imp <- mean(data_imputed_pattern$soil.moi.cm)
abs_mean_weather_orig <- mean(data_original_pattern$soil.moi.cm, na.rm = T)

z.test2sam = function(a, b, var.a, var.b){
  n.a = length(a)
  n.b = length(b)
  zeta = abs((mean(a) - mean(b))) / (sqrt(var.a/n.a + var.b/n.b))
  return(zeta)
}

z.test2sam(abs_mean_weather_imp,abs_mean_weather_orig, sd_weather_imp, sd_weather_orig )



#The value of zeta is smaller than the value of the critical value zeta tabulated for alpha equal to 0.05 
#(z-tabulated = 1.96 for a two-tailed test): then we do not reject the null hypothesis in favor of the alternative 
#hypothesis. We conclude that the two means are not significantly different.

# a ratio of variances of the observed and imputed values that is less than 0.5 or greater than 2

# rain.val.mm
sd_weather_orig <- sd(data_original_pattern$rain.val.mm, na.rm=T)
sd_weather_imp <- sd(data_imputed_pattern$rain.val.mm, na.rm=T)
ratio_1 <- sd_weather_orig/sd_weather_imp
ratio_1
ratio_2 <-  sd_weather_imp/sd_weather_orig
ratio_2

# soil.moi.cm
sd_soil_orig <- sd(data_original_pattern$soil.moi.cm, na.rm=T)
sd_soil_imp <- sd(data_imputed_pattern$soil.moi.cm, na.rm=T)
ratio_1 <- sd_soil_orig/sd_soil_imp
ratio_1
ratio_2 <-  sd_soil_imp/sd_soil_orig
ratio_2

#wind.spe.kph
sd_wind.spe.kph_orig <- sd(data_original_pattern$wind.spe.kph, na.rm=T)
sd_wind.spe.kph_imp <- sd(data_imputed_pattern$wind.spe.kph, na.rm=T)
ratio_1 <- sd_wind.spe.kph_orig/sd_wind.spe.kph_imp
ratio_1
ratio_2 <-  sd_wind.spe.kph_imp/sd_wind.spe.kph_orig
ratio_2

# relative.hum.min
sd_relative.hum.min_orig <- sd(data_original_pattern$relative.hum.min, na.rm=T)
sd_relative.hum.min_imp <- sd(data_imputed_pattern$relative.hum.min, na.rm=T)
ratio_1 <- sd_relative.hum.min_orig/sd_relative.hum.min_imp
ratio_1
ratio_2 <-  sd_relative.hum.min_imp/sd_relative.hum.min_orig
ratio_2

# Number.of.Households
sd_Number.of.Households_orig <- sd(data_original_pattern$Number.of.Households, na.rm=T)
sd_Number.of.Households_imp <- sd(data_imputed_pattern$Number.of.Households, na.rm=T)
ratio_1 <- sd_Number.of.Households_orig/sd_Number.of.Households_imp
ratio_1
ratio_2 <-  sd_Number.of.Households_imp/sd_Number.of.Households_orig
ratio_2

# wind.spe.max
sd_wind.spe.max_orig <- sd(data_original_pattern$wind.spe.max, na.rm=T)
sd_wind.spe.max_imp <- sd(data_imputed_pattern$wind.spe.max, na.rm=T)
ratio_1 <- sd_wind.spe.max_orig/sd_wind.spe.max_imp
ratio_1
ratio_2 <-  sd_wind.spe.max_imp/sd_wind.spe.max_orig
ratio_2

# soil.tem.mean
sd_soil.tem.mean_orig <- sd(data_original_pattern$soil.tem.mean, na.rm=T)
sd_soil.tem.mean_imp <- sd(data_imputed_pattern$soil.tem.mean, na.rm=T)
ratio_1 <- sd_soil.tem.mean_orig/sd_soil.tem.mean_imp
ratio_1
ratio_2 <-  sd_soil.tem.mean_imp/sd_soil.tem.mean_orig
ratio_2


# significant Kolmogorov-Smirnov test when comparing the observed and imputed values 
ks.test(data_original_pattern$rain.val.mm, data_imputed_pattern$rain.val.mm)                         # bad
ks.test(data_original_pattern$rain.int.mm.hr, data_imputed_pattern$rain.int.mm.hr)
ks.test(data_original_pattern$wind.spe.kph, data_imputed_pattern$wind.spe.kph)                       # bad
ks.test(data_original_pattern$wind.spe.max, data_imputed_pattern$wind.spe.max)                       # bad
ks.test(data_original_pattern$air.tem., data_imputed_pattern$air.tem.)
ks.test(data_original_pattern$air.tem.max, data_imputed_pattern$air.tem.max)
ks.test(data_original_pattern$air.tem.min, data_imputed_pattern$air.tem.min)
ks.test(data_original_pattern$air.pre, data_imputed_pattern$air.pre)
ks.test(data_original_pattern$dew.poi, data_imputed_pattern$dew.poi)
ks.test(data_original_pattern$relative.hum, data_imputed_pattern$relative.hum)
ks.test(data_original_pattern$relative.hum.max, data_imputed_pattern$relative.hum.max)
ks.test(data_original_pattern$relative.hum.min, data_imputed_pattern$relative.hum.min)               # bad
ks.test(data_original_pattern$sunshine.dur.hour, data_imputed_pattern$sunshine.dur.hour)
ks.test(data_original_pattern$solar.rad.w.m2, data_imputed_pattern$solar.rad.w.m2)
ks.test(data_original_pattern$soil.moi.vwc, data_imputed_pattern$soil.moi.vwc)
ks.test(data_original_pattern$soil.moi.cm, data_imputed_pattern$soil.moi.cm)                         # bad
ks.test(data_original_pattern$soil.tem.mean, data_imputed_pattern$soil.tem.mean)                     # bad
ks.test(data_original_pattern$soil.tem.mean2, data_imputed_pattern$soil.tem.mean2)
ks.test(data_original_pattern$Cattle, data_imputed_pattern$Cattle)
ks.test(data_original_pattern$Number.of.Households, data_imputed_pattern$Number.of.Households)   # bad
ks.test(data_original_pattern$own.fauc.com.wat.sys, data_imputed_pattern$own.fauc.com.wat.sys)
ks.test(data_original_pattern$shar.fauc.com.wat.sys, data_imputed_pattern$shar.fauc.com.wat.sys)
ks.test(data_original_pattern$own.tub.pip.dep.well, data_imputed_pattern$own.tub.pip.dep.well)
ks.test(data_original_pattern$shar.tub.pip.dep.well, data_imputed_pattern$shar.tub.pip.dep.well)
ks.test(data_original_pattern$tub.pip.shal.well, data_imputed_pattern$tub.pip.shal.well)
ks.test(data_original_pattern$dug.well, data_imputed_pattern$dug.well)
ks.test(data_original_pattern$protec.spring, data_imputed_pattern$protec.spring)
ks.test(data_original_pattern$unprotec.spring, data_imputed_pattern$unprotec.spring)
ks.test(data_original_pattern$lake.riv.rain, data_imputed_pattern$lake.riv.rain)
ks.test(data_original_pattern$peddler, data_imputed_pattern$peddler)
ks.test(data_original_pattern$bottled.wat, data_imputed_pattern$bottled.wat)
ks.test(data_original_pattern$others, data_imputed_pattern$others)
ks.test(data_original_pattern$pop.dens.km2, data_imputed_pattern$pop.dens.km2)

######################################################################################################################
# knn
######################################################################################################################

## load data

# from csv -> not good because character
data1 <- read.csv("C:/Users/final_data.csv", header=TRUE, sep =  ",")
data1 <- data1[,-1]

knn_only <- kNN(data1, k=3,imp_var = F)

data_imputed_knn <- knn_only
data_original_knn <- data1

# rename variables data_imputed_knn
my_data <- as_tibble(data_imputed_knn)

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

data_imputed_knn <- my_data

# rename variables data_original_knn
my_data <- as_tibble(data_original_knn)

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

data_original_knn <- my_data


# diagnostics
# significant Kolmogorov-Smirnov test when comparing the observed and imputed values 
ks.test(data_original_knn$rain.val.mm, data_imputed_knn$rain.val.mm)                         # knn bad
ks.test(data_original_knn$rain.int.mm.hr, data_imputed_knn$rain.int.mm.hr)                   # knn bad
ks.test(data_original_knn$wind.spe.kph, data_imputed_knn$wind.spe.kph)                       # knn bad
ks.test(data_original_knn$wind.spe.max, data_imputed_knn$wind.spe.max)                       # knn bad
ks.test(data_original_knn$air.tem., data_imputed_knn$air.tem.)                               
ks.test(data_original_knn$air.tem.max, data_imputed_knn$air.tem.max)                         
ks.test(data_original_knn$air.tem.min, data_imputed_knn$air.tem.min)                         
ks.test(data_original_knn$air.pre, data_imputed_knn$air.pre)
ks.test(data_original_knn$dew.poi, data_imputed_knn$dew.poi)                                 
ks.test(data_original_knn$relative.hum, data_imputed_knn$relative.hum)                       
ks.test(data_original_knn$relative.hum.max, data_imputed_knn$relative.hum.max)               
ks.test(data_original_knn$relative.hum.min, data_imputed_knn$relative.hum.min)               # knn bad
ks.test(data_original_knn$sunshine.dur.hour, data_imputed_knn$sunshine.dur.hour)
ks.test(data_original_knn$solar.rad.w.m2, data_imputed_knn$solar.rad.w.m2)                   
ks.test(data_original_knn$soil.moi.vwc, data_imputed_knn$soil.moi.vwc)                       # knn bad
ks.test(data_original_knn$soil.moi.cm, data_imputed_knn$soil.moi.cm)                         # bad 
ks.test(data_original_knn$soil.tem.mean, data_imputed_knn$soil.tem.mean)                     # bad 
ks.test(data_original_knn$soil.tem.mean2, data_imputed_knn$soil.tem.mean2)                   
ks.test(data_original_knn$Cattle, data_imputed_knn$Cattle)                                   
ks.test(data_original_knn$Number.of.Households, data_imputed_knn$Number.of.Households)   # knn bad
ks.test(data_original_knn$own.fauc.com.wat.sys, data_imputed_knn$own.fauc.com.wat.sys)   # knn bad
ks.test(data_original_knn$shar.fauc.com.wat.sys, data_imputed_knn$shar.fauc.com.wat.sys) # knn bad
ks.test(data_original_knn$own.tub.pip.dep.well, data_imputed_knn$own.tub.pip.dep.well)   # knn bad
ks.test(data_original_knn$shar.tub.pip.dep.well, data_imputed_knn$shar.tub.pip.dep.well) # knn bad
ks.test(data_original_knn$tub.pip.shal.well, data_imputed_knn$tub.pip.shal.well)         # knn bad
ks.test(data_original_knn$dug.well, data_imputed_knn$dug.well)                           # knn bad
ks.test(data_original_knn$protec.spring, data_imputed_knn$protec.spring)                 # knn bad
ks.test(data_original_knn$unprotec.spring, data_imputed_knn$unprotec.spring)             # knn bad
ks.test(data_original_knn$lake.riv.rain, data_imputed_knn$lake.riv.rain)                 # knn bad
ks.test(data_original_knn$peddler, data_imputed_knn$peddler)                             # knn bad
ks.test(data_original_knn$bottled.wat, data_imputed_knn$bottled.wat)                     # knn bad
ks.test(data_original_knn$others, data_imputed_knn$others)                               # knn bad
ks.test(data_original_knn$pop.dens.km2, data_imputed_knn$pop.dens.km2)                       

## Geometric diagnostic

# Density plot soil.moi.vwc
d1 <- density(data_imputed_knn$soil.moi.vwc)
d2 <- density(data_original_knn$soil.moi.vwc, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "soil.moi.vwc density plots knn imputation", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("right", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))


# Density plot air.tem.
d1 <- density(data_imputed_knn$air.tem.)
d2 <- density(data_original_knn$air.tem., na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "air.tem. density plots knn imputation", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("left", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))


# Density plot relative.hum.max
d1 <- density(data_imputed_knn$relative.hum.max)
d2 <- density(data_original_knn$relative.hum.max, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "relative.hum.max density plots knn imputation", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("left", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))


# Density plot own.fauc.com.wat.sys
d1 <- density(data_imputed_knn$own.fauc.com.wat.sys)
d2 <- density(data_original_knn$own.fauc.com.wat.sys, na.rm = TRUE)

plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", main = "own.fauc.com.wat.sys density plots knn imputation", xlab = "x",
     ylab = "Density")
lines(d1, col = "red")
lines(d2, col = "blue",lty = 2)
legend("right", col = c("red", "blue"), lwd = c(1,1), lty = c(1,2),
       legend = c("Imputed Variable", "Original Variable"))





