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
data <- data.table::fread("Main-Data-Final-2.csv", data.table = FALSE)


#describe data
names(data)
head(data)
str(data)
tail(data)

## Categorizing age
summary(data$age)
sd(data$age)
data$age_cat <- as.factor(ifelse(data$age < 18, 1, ifelse(data$age > 19, 3, 2)))
levels(data$age_cat) <- c("13 to 17","18 to 19","20 to 24")
summary(data$age_cat)


data$road_user <- as.character(data$road_user)
data$road_user[data$road_user == "1"] <- "Rider of two wheelers"
data$road_user[data$road_user == "3"] <- "Driver of four wheelers"
data$road_user[data$road_user == "6"] <- "Driver of three wheelers"
data$road_user[data$road_user == "8"] <- "Driver more than four"
data$road_user <- as.factor(data$road_user)
table(data$road_user)



#### weekday and Weekend #####
data$days_weekend <- as.character(data$days_weekend)
data$days_weekend[data$days_weekend == "1"] <- "weekday"
data$days_weekend[data$days_weekend == "2"] <- "weekend"
table(data$days_weekend)

####   light and Dark #####
data$day_night <- as.factor(data$day_night )
levels(data$day_night ) <- c("light","dark")
table(data$day_night)


## Give  new levels to Gender Distribution
data$gender <- as.factor(data$gender)
levels(data$gender) <- c("Male","Female")
table(data$gender)


### Helmet use variable ###
data$helmet_use[is.na(data$helmet_use)] <- 99
data$helmet_use <- as.factor(data$helmet_use)
levels(data$helmet_use) <- c("Yes","No","Unknown","Missing")
table(data$helmet_use)

##### Vehicle 1 ####

str(data$vehicle_1)
data$vehicle_1[data$vehicle_1 == 1.1] <- NA
data$vehicle_1[data$vehicle_1 == 1.12] <- NA
data$vehicle_1[data$vehicle_1 == 1.15] <- NA
data$vehicle_1[data$vehicle_1 == 2.3] <- NA
data$vehicle_1[data$vehicle_1 == 3.7] <- NA
data$vehicle_1[data$vehicle_1 == 99] <- NA
data$vehicle_1[is.na(data$vehicle_1)] <- 99

data$vehicle_1 <- as.factor(data$vehicle_1)
levels(data$vehicle_1) <- c("Motorcyle","MiniVan/Coaster","Bus/Minibus/Coach","Truck","Taxi",
                            "Bicycle", "Car","Water/oiltanker","Rickshaw","Dumper","Trailor",
                            "Loading Pick up", "Others", "If loaded Vehicle then","Animal Cart",
                            "Push Cart","Train","Missing")
table(data$vehicle_1) 

##### vehicle 2 #########

str(data$vehicle_2)
data$vehicle_2[data$vehicle_2 == 1.11] <- NA
data$vehicle_2[data$vehicle_2 == 1.12] <- NA
data$vehicle_2[data$vehicle_2 == 1.18] <- NA
data$vehicle_2[data$vehicle_2 == 1.3] <- NA
data$vehicle_2[data$vehicle_2 == 1.7] <- NA
data$vehicle_2[data$vehicle_2 == 1.9] <- NA
data$vehicle_2[data$vehicle_2 == 2.15] <- NA
data$vehicle_2[data$vehicle_2 == 3.7] <- NA
data$vehicle_2[data$vehicle_2 == 3.9] <- NA
data$vehicle_2[data$vehicle_2 == 4.7] <- NA
data$vehicle_2[data$vehicle_2 == 5.7] <- NA
data$vehicle_2[data$vehicle_2 == 7.11] <- NA
data$vehicle_2[data$vehicle_2 == 7.12] <- NA
data$vehicle_2[data$vehicle_2 == 7.7] <- NA
data$vehicle_2[data$vehicle_2 == 7.9] <- NA
data$vehicle_2[data$vehicle_2 == 9.12] <- NA
data$vehicle_2[data$vehicle_2 == 10.1] <- NA
data$vehicle_2[data$vehicle_2 == 12.9] <- NA
data$vehicle_2[data$vehicle_2 == 99] <- NA
data$vehicle_2[is.na(data$vehicle_2)] <- 99

data$vehicle_2 <- as.factor(data$vehicle_2)
levels(data$vehicle_2) <- c("Motorcyle","MiniVan/Coaster","Bus/Minibus/Coach","Truck","Taxi",
                            "Bicycle", "Car","Water/oiltanker","Rickshaw","Dumper","Trailor",
                            "Loading Pick up", "Others", "If loaded Vehicle then","Animal Cart",
                            "Push Cart","Train","Missing")
table(data$vehicle_2)  

###### Disposition ######
table(data$disposition)
str(data$disposition)
data$disposition[is.na(data$disposition)] <- 99
data$disposition <- as.factor(data$disposition)
levels(data$disposition) <- c("Discharge","Admitted to ward","Detained","Death","Transferred","LAMA","Referred from","Missing")
table(data$disposition)


### Arrival Vehicle ###

table(data$arrival_vehicle)
str(data$arrival_vehicle)
data$arrival_vehicle[data$arrival_vehicle == 1.2] <- NA
data$arrival_vehicle <- as.factor(data$arrival_vehicle)
levels(data$arrival_vehicle) <- c("Ambulance","Police","Private","Public","Others","Missing")
table(data$arrival_vehicle)
#Abdominal score
table(data$abdominal_final)
#will work further on scores next time

