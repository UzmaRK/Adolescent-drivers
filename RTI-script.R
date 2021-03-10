## Statistical analysis script
##Underage driving and road traffic injuries
## Authors:
## Uzma Rahim Khan
## Junaid Razzak
## Martin Gerdin Wärnberg

## Fixing the working director
setwd("C:/Users/uzma.khan/OneDrive - Aga Khan University/Documents/RTIRPC data")

#install.packages("data.table")
library(data.table)
#install.packages("tableone")
library(tableone)
#install.packages("survival")
library(survival)
#install.packages("ISLR")
library(ISLR)
#install.packages("labelled")
library(labelled)
#install.packages("knitr")
library(knitr)
#install.packages("rmarkdown")
library(rmarkdown)

## Reading the data from the above mentioned directory
data <- data.table::fread("Data_Final-RTI.csv", data.table = FALSE)

#describe data
names(data)

## Description of attribute of the variable
str(data)

## Frequency of variables
table(data$age)
table(data$gender)
table(data$year)
table(data$road_user)
table(data$vehicle_1)
table(data$vehicle_2)
table(data$helmet_use)
table(data$underpass_bridge)
table(data$type_collision)

