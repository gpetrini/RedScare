#Data frame - Red Scare
#Author: Lorena Dourado

#loading packages
library(tidyverse)
library(countrycode)

#importing data
setwd("~/Desktop/Monografia/Base de dados")
one_percent_income <- read.csv2("WID_Data_11042021-185939.csv")

#subseting 
year <- one_percent_income[,1, drop = FALSE]
income <- one_percent_income [,2:19]

#cleaning data
#gsub() or nchar() as additional options 
colnames(income) <- substring(colnames(income), 52)

#transforming in ISO3
colnames(income) <- countrycode(colnames(income), origin = 'country.name', destination = 'iso3c')

#joining subsets again
income_concentration <- cbind(year,income)

#exporting to csv
write.csv(income_concentration, "income_concentration.csv")
