#### packages installeren
##install.packages("xlsx")
##install.packages("ggplot2")
##install.packages("reshape2")
##install.packages("plyr")
##install.packages("gdata")
##install.packages("sparklyr")
##install.packages("tidyr")
##install.packages("psych")
##install.packages("moments")
##install.packages("VIM")
##install.packages("cluster")
##install.packages("MASS")
##install.packages("Hmisc")
##install.packages("HH")
##install.packages("tidyverse")
##install.packages("odbc")
##install.packages("lattice")
##install.packages("mice")
##install.packages("VIM")
##install.packages("clusterSim") 
##install.packages("RODBC")
##install.packages("FactoMineR")
##install.packages("corrplot")
##install.packages("functional")

##inladen packages
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

##setwd
setwd("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts")

##inlezen data
data <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts/basis excel.csv", header=TRUE, sep =  ";")      
dengue <-read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts/DengueLoc2015.csv", header=TRUE, sep =  ";")
weather <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts/combine1.csv", header = TRUE, sep = ";", na.strings = c( "", " "))
ovitrap_2015 <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts/Philippines_Dengue_Jan_2015_Dec_2015_Lat_Lng_Coord_Final.csv", header = TRUE, sep = ";")
admin <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts/170926_philippines-population-admin-1-to-4_bysex_2015.csv", header = TRUE, sep = ";") 
animal <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts/Livestock Inventory by Animal Type Farm Type Provinces Year and Period (2016).csv", header = TRUE, sep = ";")
pop_dens <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts/pop_dens_2015.csv", header = TRUE, sep = ";")
sanitation <-read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts/household-toilet-type-by-municipality.csv", header = TRUE, sep = ";")
drink_wat <- read.csv("C:/Users/user/Downloads/school/master thesis/rode kruis/R scripts/household-water-source-by-municipality.csv", header = TRUE, sep = ";")

##aanmaken new data frame voor alle maanden
data1 <- data[rep(seq_len(nrow(data)), each=12),]
data1$month <- NA
data1$month <- matrix( c("jan","feb","mar","apr","may","jun", "jul",
                         "aug", "sep", "oct", "nov", "dec"), nrow(data1))

##nieuw verticaal dataframe voor dengue
dengue <- rename.vars(dengue, from = "Dec", to = "dec")
dengue1 <- dengue
dengue1 <- melt(dengue, id.vars = c("location_id", "population_2015", "location_name", "Region", "Province", "code"))

#drop variables, change name and to lower case
dengue1$population_2015 <- NULL
dengue1$location_id <- NULL
dengue1$location_name <- NULL
dengue1$Region <- NULL
dengue1 <- rename.vars(dengue1, from = "value", to = "incidence")
dengue1 <- rename.vars(dengue1, from = "variable", to = "month")

data1 <- rename.vars(data1, from = "m2_code" , to = "code")
dengue1$code <- as.character(dengue1$code)
data1$code <- as.character(data1$code)

#merge data 1 met dengue1
base_data <- left_join(dengue1, data1, by = c("month", "code"))

##opruimen base_data
##verwijderen onnodige variabelen
base_data$adm1_code <- NULL
base_data$adm1_name <- NULL
base_data$location_name <- NULL
base_data$Province <-NULL

##aanpassen namen
base_data <- rename.vars(base_data, from = "Male" , to = "pop.nr.male")
base_data <- rename.vars(base_data, from = "Female" , to = "pop.nr.female")
base_data <- rename.vars(base_data, from = "Total" , to = "pop.nr.tot")
base_data <- rename.vars(base_data, from = "adm2_name", to = "province")


##subset van weer dataset om 2016 te skippen
str(weather)
weather$year <- as.numeric(weather$year)
weather1 <- subset(weather, weather$year < 2016) 
weather1$month <- revalue(weather1$month, c("January"="jan", "February"="feb", "March" = "mar",
                                            "April" = "apr", "May" = "may", "June" = "jun", 
                                            "July" = "jul", "August" = "aug", "September" = "sep",
                                            "October" = "oct", "November" = "nov", "December" = "dec"))

# get rid of repeating variables
weather2 <- weather1[-c(2,5)]

#join weather with dengue indcidence and data
base_data1 <- left_join(base_data, weather2, by = c("month", "code"))


###aan de slag met pop_dens en koppeling
pop_dens$normalized<- NULL
pop_dens$normalized.10<- NULL
pop_dens$lin.min.max <- NULL

###pop dens maanden toevoegen voor iedere provincie
pop_dens1 <- pop_dens[rep(seq_len(nrow(pop_dens)), each=12),]
pop_dens1$month <- NA
pop_dens1$month <- matrix( c("jan","feb","mar","apr","may","jun", "jul",
                             "aug", "sep", "oct", "nov", "dec"), nrow(pop_dens1))

##aanpassen naam
pop_dens1 <- rename.vars(pop_dens1, from = "PROVINCE_CODE", to= "code")
pop_dens1 <- rename.vars(pop_dens1, from = "Population_Density_persons_square_km", to = "pop.dens.km2")

###animal subset for commercial
animal_comm <- subset(animal, Type.of.farm == "commercial")

##animal subset for backyard
animal_back <- subset(animal, Type.of.farm == "backyard")

##animal maanden toevoegen
animal_comm1 <- animal_comm[rep(seq_len(nrow(animal_comm)), each=12),]
animal_comm1$month <- NA
animal_comm1$month <- matrix( c("jan","feb","mar","apr","may","jun", "jul",
                                "aug", "sep", "oct", "nov", "dec"), nrow(animal_comm1))
##remove PH van code
##animal_comm1$Pcode <- gsub("[^0-9]", "", animal_comm1$Pcode) 

animal_back1 <- animal_back[rep(seq_len(nrow(animal_back)), each=12),]
animal_back1$month <- NA
animal_back1$month <- matrix( c("jan","feb","mar","apr","may","jun", "jul",
                                "aug", "sep", "oct", "nov", "dec"), nrow(animal_back1))

##hernoemen variabelen voor merge
animal_back1 <- rename.vars(animal_back1, from = "Pcode" , to = "code")
animal_comm1 <- rename.vars(animal_comm1, from = "Pcode" , to = "code")

#sanitation code hernoemen
sanitation <- rename.vars(sanitation, from = "Province.Code", to = "code")

##aggregeren op basis van code
sanitation1 <- aggregate( .~ code, sanitation, sum)

##ook maanden toevoegen aan sanitation 
sanitation2 <- sanitation1[rep(seq_len(nrow(sanitation1)), each=12),]
sanitation2$month <- NA
sanitation2$month <- matrix( c("jan","feb","mar","apr","may","jun", "jul",
                               "aug", "sep", "oct", "nov", "dec"), nrow(sanitation2))

##drinking water aggregeren op prov code
drink_wat1 <- aggregate( .~ code, drink_wat, sum)

##maanden toevoegen aan drink_wat1
drink_wat2 <- drink_wat1[rep(seq_len(nrow(drink_wat1)), each=12),]
drink_wat2$month <- NA
drink_wat2$month <- matrix( c("jan","feb","mar","apr","may","jun", "jul",
                              "aug", "sep", "oct", "nov", "dec"), nrow(drink_wat2))



base_data2 <- left_join(base_data1, animal_back1, by = c("month", "code"))
base_data2$Carabao <- NULL
base_data2 <- rename.vars(base_data2, from = "Carabao.1", to = "carabao")

base_data3 <- left_join(base_data2, sanitation2, by = c("month", "code"))

base_data4 <- left_join(base_data3, drink_wat2, by = c( "month", "code"))

base_data5 <- left_join(base_data4, pop_dens1, by = c("month", "code"))

base_data6 <- base_data5  %>% distinct(month,province,incidence, .keep_all = TRUE)
base_data7 <- base_data6  %>% distinct(province,month, .keep_all = TRUE)
base_data8 <- base_data7[which(base_data7$code != "PH133900000"), ]


##write raw data without imputations
write.csv(base_data7, file = "raw_file_elena_withmanila.csv")
write.csv(base_data8, file = "raw_file_elena.csv")

rm(list=ls())


