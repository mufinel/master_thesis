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


## load data

# from csv 
data <- read.csv("C:/Users/imputed_soil_data.csv", header=TRUE, sep =  ",")
data <- data[,-c(1,2,3,5)]


## FCS

# create before MI data set
df_before <- data

# extract all variable names
allVars <- names(df_before)

# extract names of vars with missing
missVars <- names(df_before)[colSums(is.na(df_before)) > 0]

## mice predictorMatrix
## A square matrix of size 'ncol(data)' containing 0/1
## data specifying the set of predictors to be used for each
## target column. Rows correspond to target variables (i.e.
## variables to be imputed), in the sequence as they appear in
## data. A value of '1' means that the column variable is used
## as a predictor for the target variable (in the rows). The
## diagonal of 'predictorMatrix' must be zero. The default for
## 'predictorMatrix' is that all other columns are used as
## predictors (sometimes called massive imputation). Note: For
## two-level imputation codes '2' and '-2' are also allowed.
##
predictorMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
rownames(predictorMatrix) <- allVars
colnames(predictorMatrix) <- allVars

# specify variables informing imputation
imputerVars <- c( "incidence",               "pop.nr.male"        ,     "pop.nr.female"  ,         "pop.nr.tot"  ,            "rain.val.mm"    ,        
                  "rain.int.mm.hr",          "wind.spe.kph"        ,    "wind.spe.max"    ,        "air.tem."     ,           "air.tem.max"     ,       
                  "air.tem.min"    ,         "air.pre"              ,   "dew.poi"          ,       "relative.hum"  ,          "relative.hum.max" ,      
                  "relative.hum.min",        "sunshine.dur.hour"     ,  "solar.rad.w.m2"    ,      "soil.moi.vwc"   ,         "soil.moi.cm"       ,     
                  "soil.tem.mean"    ,       "soil.tem.mean2"         , "carabao"            ,     "Cattle"          ,        "Goat"               ,    
                  "Hog"   ,                  "pop.dens.km2"                  )
imputerVars <- intersect(unique(imputerVars), allVars)
imputerVars
imputerMatrix <- predictorMatrix
imputerMatrix[,imputerVars] <- 1
imputerMatrix

# specify var to be imputed
imputedOnlyVars <- c(  "Number.of.Households.y",  "own.fauc.com.wat.sys.y" , "shar.fauc.com.wat.sys.y", "own.tub.pip.dep.well.y", 
                       "shar.tub.pip.dep.well.y", "tub.pip.shal.well.y"     ,"dug.well.y"           ,   "protec.spring.y"        , "unprotec.spring.y"  ,    
                       "lake.riv.rain.y"         ,"peddler.y"               ,"bottled.wat.y"         ,  "others.y"                 )
imputedVars <- intersect(unique(c(imputedOnlyVars, imputerVars)), missVars)
imputedVars
imputedMatrix <- predictorMatrix
imputedMatrix[imputedVars,] <- 1
imputedMatrix

# construct full predictor matrix
predictorMatrix <- imputerMatrix * imputedMatrix
diag(predictorMatrix) <- 0
predictorMatrix

###  Dry-run mice for imputation methods\n")
dryMice <- mice(data = df_before, m = 1, predictorMatrix = predictorMatrix, maxit = 0, method='cart')
## Update predictor matrix
predictorMatrix <- dryMice$predictorMatrix
cat("###   Imputers (non-zero columns of predictorMatrix)\n")
imputerVars <- colnames(predictorMatrix)[colSums(predictorMatrix) > 0]
imputerVars
cat("###   Imputed (non-zero rows of predictorMatrix)\n")
imputedVars <- rownames(predictorMatrix)[rowSums(predictorMatrix) > 0]
imputedVars
cat("###   Imputers that are complete\n")
setdiff(imputerVars, imputedVars)
cat("###   Imputers with missingness\n")
intersect(imputerVars, imputedVars)
cat("###   Imputed-only variables without being imputers\n")
setdiff(imputedVars, imputerVars)
cat("###   Variables with missingness that are not imputed\n")
setdiff(missVars, imputedVars)
cat("###   Relevant part of predictorMatrix\n")
predictorMatrix[rowSums(predictorMatrix) > 0, colSums(predictorMatrix) > 0]


## Empty imputation method to really exclude variables
## http://www.stefvanbuuren.nl/publications/MICE%20in%20R%20-%20Draft.pdf
##
## MICE will automatically skip imputation of variables that are complete.
## One of the problems in previous versions of MICE was that all incomplete
## data needed to be imputed. In MICE 2.0 it is possible to skip imputation
## of selected incomplete variables by specifying the empty method "".
## This works as long as the incomplete variable that is skipped is not being
## used as a predictor for imputing other variables.
## Note: puttting zeros in the predictorMatrix alone is NOT enough!
##
dryMice$method[setdiff(allVars, imputedVars)] <- ""
cat("###   Methods used for imputation\n")
dryMice$method[sapply(dryMice$method, nchar) > 0]


cat("
###  Run mice\n")
M <- 5
cat("### Imputing", M, "times\n")

## Set seed for reproducibility
set.seed(3561126)

## Parallelized execution
library(doRNG)

miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
  cat("### Started iteration", i, "\n")
  miceout <- mice(data = df_before, m = 1, print = TRUE,
                  predictorMatrix = predictorMatrix, method = dryMice$method,
                  MaxNWts = 2000)
  cat("### Completed iteration", i, "\n")
  ## Make sure to return the output
  miceout
}


cat("
###  Show mice results\n")
## mice object ifself
miceout
## Variables that no longer have missingness after imputation
cat("###   Variables actually imputed\n")
actuallyImputedVars <-
  setdiff(names(df_before)[colSums(is.na(df_before)) > 0],
          names(complete(miceout, action = 1))[colSums(is.na(complete(miceout, action = 1))) > 0])
actuallyImputedVars

## Examine discrepancies
cat("###   Variables that were unexpectedly imputed\n")
setdiff(actuallyImputedVars, imputedVars)
cat("###   Variables that were planned for MI but not imputed\n")
setdiff(imputedVars, actuallyImputedVars)

## Still missing variables
cat("###   Variables still having missing values\n")
names(complete(miceout, action = 1))[colSums(is.na(complete(miceout, action = 1))) > 0]

fcs_imp <- c()
fcs_imp<-complete(miceout, action = 5)

# write csv
write.csv(fcs_imp, file = "fully_imputed_data.csv")
