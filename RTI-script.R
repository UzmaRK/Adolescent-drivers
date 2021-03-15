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
head(data)
str(data)
tail(data)

## Categorizing age
data$age_cat <- as.factor(ifelse(data$age < 18, 1, ifelse(data$age > 19, 3, 2)))
levels(data$age_cat) <- c("13 to 17","18 to 19","20 to 24")
summary(data$age_cat)

## Give  new levels to some variables
data$gender <- as.character(data$gender)
data$gender[data$gender == "1"] <- "Male"
data$gender[data$gender == "2"] <- "Female"
data$gender <- as.factor(data$gender)
table(data$gender)

data$road_user <- as.character(data$road_user)
data$road_user[data$road_user == "1"] <- "Rider of two wheelers"
data$road_user[data$road_user == "2"] <- "Pillion Passengers"
data$road_user[data$road_user == "3"] <- "Driver of four wheelers"
data$road_user[data$road_user == "4"] <- "Passengers"
data$road_user[data$road_user == "5"] <- "Pedestrians"
data$road_user[data$road_user == "6"] <- "Driver of three wheelers"
data$road_user[data$road_user == "7"] <- "others"
data$road_user[data$road_user == "8"] <- "Driver more than four"
data$road_user <- as.factor(data$road_user)
table(data$road_user)

data$vehicle_1 <- as.character(data$vehicle_1)
data$vehicle_1[data$vehicle_1 == "1"] <- "Motorcycle"
data$vehicle_1[data$vehicle_1 == "2"] <- "MiniVan/Coaster"
data$vehicle_1[data$vehicle_1 == "3"] <- "Bus/Minibus/Coach"
data$vehicle_1[data$vehicle_1 == "4"] <- "Truck"
data$vehicle_1[data$vehicle_1 == "5"] <- "Taxi"
data$vehicle_1[data$vehicle_1 == "6"] <- "Bicycle"
data$vehicle_1[data$vehicle_1 == "7"] <- "Car"
data$vehicle_1[data$vehicle_1 == "8"] <- "Water/oiltanker"
data$vehicle_1[data$vehicle_1 == "9"] <- "Rickshaw"
data$vehicle_1[data$vehicle_1 == "10"] <- "Dumper"
data$vehicle_1[data$vehicle_1 == "11"] <- "Trailor"
data$vehicle_1[data$vehicle_1 == "12"] <- "Loading Pick up"
data$vehicle_1[data$vehicle_1 == "13"] <- "Others"
data$vehicle_1[data$vehicle_1 == "14"] <- "If loaded Vehicle then"
data$vehicle_1 <- as.factor(data$vehicle_1)
table(data$vehicle_1)  

data$vehicle_2 <- as.character(data$vehicle_2)
data$vehicle_2[data$vehicle_2 == "1"] <- "Motorcycle"
data$vehicle_2[data$vehicle_2 == "2"] <- "MiniVan/Coaster"
data$vehicle_2[data$vehicle_2 == "3"] <- "Bus/Minibus/Coach"
data$vehicle_2[data$vehicle_2 == "4"] <- "Truck"
data$vehicle_2[data$vehicle_2 == "5"] <- "Taxi"
data$vehicle_2[data$vehicle_2 == "6"] <- "Bicycle"
data$vehicle_2[data$vehicle_2 == "7"] <- "Car"
data$vehicle_2[data$vehicle_2 == "8"] <- "Water/oiltanker"
data$vehicle_2[data$vehicle_2 == "9"] <- "Rickshaw"
data$vehicle_2[data$vehicle_2 == "10"] <- "Dumper"
data$vehicle_2[data$vehicle_2 == "11"] <- "Trailor"
data$vehicle_2[data$vehicle_2 == "12"] <- "Loading Pick up"
data$vehicle_2[data$vehicle_2 == "13"] <- "Others"
data$vehicle_2[data$vehicle_2 == "14"] <- "If loaded Vehicle then"
data$vehicle_2 <- as.factor(data$vehicle_2)
table(data$vehicle_2) 

data$location_detail <- as.character(data$location_detail)
data$location_detail[data$location_detail == "1"] <- "Intersection"
data$location_detail[data$location_detail == "2"] <- "Midblock"
data$location_detail <- as.factor(data$location_detail)
table(data$location_detail)

data$type_collision <- as.character(data$type_collision)
data$type_collision[data$type_collision == "1"] <- "Head on"
data$type_collision[data$type_collision == "2"] <- "Rear end"
data$type_collision[data$type_collision == "3"] <- "Hit Object"
data$type_collision[data$type_collision == "4"] <- "Merging"
data$type_collision[data$type_collision == "5"] <- "Side Swipe"
data$type_collision[data$type_collision == "6"] <- "Right Angle"
data$type_collision[data$type_collision == "7"] <- "Unknown"
data$type_collision <- as.factor(data$type_collision)
table(data$type_collision)

data$arrival_vehicle <- as.character(data$arrival_vehicle)
data$arrival_vehicle[data$arrival_vehicle == "1"] <- "Ambulance"
data$arrival_vehicle[data$arrival_vehicle == "2"] <- "Police"
data$arrival_vehicle[data$arrival_vehicle == "3"] <- "Private"
data$arrival_vehicle[data$arrival_vehicle == "4"] <- "Public"
data$arrival_vehicle[data$arrival_vehicle == "5"] <- "Others"
data$arrival_vehicle <- as.factor(data$arrival_vehicle)
table(data$arrival_vehicle)

data$disposition <- as.character(data$disposition)
data$disposition[data$disposition == "1"] <- "Discharge"
data$disposition[data$disposition == "2"] <- "Admitted to ward"
data$disposition[data$disposition == "3"] <- "Detained"
data$disposition[data$disposition == "4"] <- "Death"
data$disposition[data$disposition == "5"] <- "Transferred"
data$disposition[data$disposition == "1"] <- "LAMA"
data$disposition[data$disposition == "1"] <- "Referred from"
data$disposition <- as.factor(data$disposition)
table(data$disposition)

## Frequency of variables
table(data$age)
table(data$gender)
table(data$year)
table(data$road_user)
table(data$week_days)
table(data$month)
table(data$incidence_time)
table(data$arrival_vehicle)
table(data$location_detail)
table(data$vehicle_1)
table(data$vehicle_2)
table(data$helmet_use)
table(data$underpass_bridge)
table(data$type_collision)
table(data$disposition)



