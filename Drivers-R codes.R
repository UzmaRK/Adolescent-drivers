

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
data <- data.table::fread("Fianl-Cleaned.csv", data.table = FALSE)

#describe data
names(data)
head(data)
str(data)
tail(data)

## Categorizing age
data$age_cat <- as.factor(ifelse(data$age < 18, 1, ifelse(data$age > 19, 3, 2)))
levels(data$age_cat) <- c("13 to 17","18 to 19","20 to 24")
summary(data$age_cat)


####  Days weekday and Weekend #####
data$days_weekend <- as.character(data$days_weekend)
data$days_weekend[data$days_weekend == "1"] <- "weekday"
data$days_weekend[data$days_weekend == "2"] <- "weekend"
table(data$days_weekend)

####  Days Day light and Darknight #####
data$day_night <- as.factor(data$day_night )
levels(data$day_night ) <- c("Daylight","Dark")
table(data$day_night )
summary(data$day_night)

### Arrival Vehicle ###

str(data$arrival_vehicle)
data$arrival_vehicle <- as.factor(data$arrival_vehicle)
levels(data$arrival_vehicle) <- c("Ambulance","Police","Private","Public","Others")
table(data$arrival_vehicle)

## Give  new levels to Gender Distribution
data$gender <- as.factor(data$gender)
levels(data$gender) <- c("Male","Female")
table(data$gender)


### Helmet use variable ###
str(data$helmet_use)
data$helmet_use[data$helmet_use == "99"] <- NA
data$helmet_use[data$helmet_use == "3"] <- NA
data$helmet_use <- as.factor(data$helmet_use)
levels(data$helmet_use) <- c("Yes","No")
table(data$helmet_use)


#### Location Detailed ####

str(data$location_detail)
data$location_detail <- as.factor(data$location_detail)
levels(data$location_detail) <- c("Intersection","Midblock")
table(data$location_detail)

 ######## Severe versus non- severe AIS scores 3 or more #######

## head injury ##

str(data$cleaned_head)
data$head_Cat <-as.factor(ifelse(is.na(data$cleaned_head),
                                 NA,ifelse(data$cleaned_head<3,1,2)))
levels(data$head_Cat) <-c("<3",">=3")
table(data$head_Cat)
      
### face injury ##
str(data$Cleaned_face)
data$face_Cat <-as.factor(ifelse(is.na(data$Cleaned_face),
                                 NA,ifelse(data$Cleaned_face<3,1,2)))
levels(data$face_Cat) <-c("<3",">=3")
table(data$face_Cat)

## Spine injury ##
str(data$spine_cleaned)
data$spine_Cat <-as.factor(ifelse(is.na(data$spine_cleaned),
                                  NA,ifelse(data$spine_cleaned<3,1,2)))
levels(data$spine_Cat) <-c("<3",">=3")
table(data$spine_Cat)

### Upper Lower Extrimity ####
str(data$Fianl_U_L_Ext)
data$extrimity_Cat <-as.factor(ifelse(is.na(data$Fianl_U_L_Ext),
                                      NA,ifelse(data$Fianl_U_L_Ext<3,1,2)))
levels(data$extrimity_Cat) <-c("<3",">=3")
table(data$extrimity_Cat)


##  abdominal injury ##
str(data$abdominal_cleaned)
data$abdominal_Cat <-as.factor(ifelse(is.na(data$abdominal_cleaned),
                                 NA,ifelse(data$abdominal_cleaned<3,1,2)))
levels(data$abdominal_Cat) <-c("<3",">=3")
table(data$abdominal_Cat)


##  chest thorax injury ##
str(data$chest_cleaned)
data$chest_Cat <-as.factor(ifelse(is.na(data$chest_thorax_cleaned),
                                      NA,ifelse(data$chest_thorax_cleaned<3,1,2)))
levels(data$chest_Cat) <-c("<3",">=3")
table(data$chest_Cat)

## External Injury ###
str(data$external_cleaned)
data$external_Cat <-as.factor(ifelse(is.na(data$externer_cleaned),
                                  NA,ifelse(data$externer_cleaned<3,1,2)))
levels(data$external_Cat) <-c("<3",">=3")
table(data$external_Cat)

### Road Users ####
str(data$road_user)
data$road_user <- as.factor(data$road_user)
##data$road_user[is.na(data$road_user)] <- 99
levels(data$road_user) <- c("Rider of two wheelers","Driver of four wheelers","Driver of three wheelers","Driver more than four")
table(data$road_user)


###### Disposition ######

str(data$disposition)
data$disposition[data$disposition == "99"] <- NA
data$disposition <- as.factor(data$disposition)
levels(data$disposition) <- c("Discharged","Admitted to ward","Detained","Death","Transferred","LAMA","Referred from")
table(data$disposition)

### outcome ##
table(data$outcome, useNA = "always")
data$outcome[data$outcome == "99"] <- NA
data$outcome <- as.factor(data$outcome)
levels(data$outcome) <- c("Survive","Non-Survive")
table(data$outcome)

#headAIS3ormore
table(data$head_outcome)

## Categorical variables in the data
catVars <- c( "days_weekend")
data[catVars] <- lapply(data[catVars], as.factor)

## Create a label list
label.list <-list(age_cat = "Age group",
                  days_weekend= "Weekdays",
                  gender = "Gender",
                  day_night = "Day Night",
                  arrival_vehicle = "Arrival Vehicle",
                  location_detail = "Crash Location",
                  road_user = "Road User",
                  helmet_use = "Helmet use",
                  head_Cat = "Head Injury",
                  face_Cat = "Face Injury",
                  spine_Cat = "Spine Injury",
                  extrimity_Cat = "Upper Lower Extrimity",
                  abdominal_Cat = "Abdominal Injury",
                  chest_Cat ="Chest Injury",
                  external_Cat = "External Injury",
                  disposition = "Hospital Disposition",
                  outcome = "Final Disposition",
                  head_outcome = "Severe head injuries")

## Keep only relevant variables in the data
data <- data[, names(label.list)]

## Label data
labelled::var_label(data) <- label.list

## Create Table 1
table1 <- tableone::CreateTableOne(data = data)
pretty.table1 <- tableone:::print.TableOne(table1, varLabels = TRUE, showAllLevels = TRUE, test = FALSE)
colnames(pretty.table1)[colnames(pretty.table1) == "level"] <- "Level"
prettier.table <- knitr::kable(pretty.table1)
tmp.file <- tempfile(tmpdir = ".", fileext = "md")
write(prettier.table, tmp.file)
rmarkdown::render(tmp.file, output_file = "Table 1.docx", output_format = "word_document")
file.remove(tmp.file)

## Univariate Logistic regression of age groups with Outcome death
data$age_cat <- relevel(data$age_cat, ref = "20 to 24")
labelled::var_label(data$age_cat) <- "Age group"
model1 <- glm(outcome ~ age_cat, family = "binomial", data = data)
model1
summary(model1)

## Create function that returns a pretty regression table
pretty_regression_table <- function(model.object){
  cis <- confint(model.object)
  or.ci <- round(exp(cbind(OR = coef(model.object), cis)), digits = 2)
  reg.table.names <- c("Parameter", "OR", "95% CI")
  reg.table <- as.data.frame(cbind(rownames(or.ci),
                                   or.ci[, "OR"],
                                   paste0(or.ci[, "2.5 %"], ", ", or.ci[, "97.5 %"])),
                             stringsAsFactors = FALSE)
  reg.table <- reg.table[-grep("(Intercept)", rownames(reg.table)), ]
  colnames(reg.table) <- reg.table.names
  ivar.name <- attr(model.object$terms, "term.labels")
  ivar <- as.character(model.object$model[, ivar.name])
  reg.table$Parameter <- gsub(ivar.name, "", reg.table$Parameter)
  reference <- unique(ivar)[!(unique(ivar) %in% reg.table$Parameter)]
  reference.row <- c(paste0("Reference: ", reference), "1", "", "")
  variable.label <- attr(model.object$model[, ivar.name], "label")
  if (is.null(variable.label))
    variable.label <- ivar.name
  variable.row <- c(variable.label, "", "", "")
  pretty.table <- rbind(variable.row, reference.row, reg.table)
  rownames(pretty.table) <- NULL
  prettier.table <- knitr::kable(pretty.table)
  return (prettier.table)
}
pretty_regression_table(model1)


## Univariate Logistic regression of weekend with Outcome
model2 <- glm(outcome ~ days_weekend, family = "binomial", data = data)
model2
summary(model2)
pretty_regression_table(model2)

## Univariate Logistic regression of daylight vs night with Outcome
model3 <- glm(outcome ~ day_night, family = "binomial", data = data)
model3
summary(model3)
pretty_regression_table(model3)

## Univariate Logistic regression of gender with Outcome
model4 <- glm(outcome ~ gender, family = "binomial", data = data)
model4
summary(model4)
pretty_regression_table(model4)

## Univariate Logistic regression of helmet use with Outcome
model5 <- glm(outcome ~ helmet_use, family = "binomial", data = data)
model5
summary(model5)
pretty_regression_table(model5)

## Univariate Logistic regression of location with Outcome
model6 <- glm(outcome ~ data$location_detail, family = "binomial", data = data)
model6
summary(model6)
pretty_regression_table(model6)

## Univariate Logistic regression of arrival-Vehicle with Outcome
model7 <- glm(outcome ~ arrival_vehicle, family = "binomial", data = data)
model7
summary(model7)
pretty_regression_table(model7)

## Univariate Logistic regression of AIS_head with Outcome
model8 <- glm(outcome ~ head_Cat, family = "binomial", data = data)
model8
summary(model8)
pretty_regression_table(model8)

## Univariate Logistic regression of roaduser with Outcome
model9 <- glm(outcome ~ road_user, family = "binomial", data = data)
model9
summary(model9)
pretty_regression_table(model9)

## Univariate Logistic regression of AIS_face with Outcome
model10 <- glm(outcome ~ face_Cat, family = "binomial", data = data)
model10
summary(model10)
pretty_regression_table(model10)

## Univariate Logistic regression of AIS_spine with Outcome
model11 <- glm(outcome ~ spine_Cat, family = "binomial", data = data)
model11
summary(model11)
pretty_regression_table(model11)

## Univariate Logistic regression of thorax_ais with Outcome
model12 <- glm(outcome ~ chest_Cat, family = "binomial", data = data)
model12
summary(model12)
pretty_regression_table(model12)

## Univariate Logistic regression of abdomen_ais with Outcome
model13 <- glm(outcome ~ abdominal_Cat, family = "binomial", data = data)
model13
summary(model13)
pretty_regression_table(model13)

## Univariate Logistic regression of extrimity_face with Outcome
model14 <- glm(outcome ~ extrimity_Cat, family = "binomial", data = data)
model14
summary(model14)
pretty_regression_table(model14)

## Univariate Logistic regression of external_ais with Outcome
model15 <- glm(outcome ~ chest_Cat, family = "binomial", data = data)
model15
summary(model15)
pretty_regression_table(model15)

#Multivariate Logistic reg
model16 <- glm(outcome ~  age_cat + days_weekend + day_night + location_detail +
                 arrival_vehicle + helmet_use ,
               family = "binomial", data = data)
summary(model16)
or.ci<- exp(cbind(OR = coef(model16), confint(model16)))
round(or.ci, digits=2)

#Outcome 2 Severe head injuries

# Univariate Logistic regression of age groups with Outcome death
data$age_cat <- relevel(data$age_cat, ref = "20 to 24")
labelled::var_label(data$age_cat) <- "Age group"
model17 <- glm(head_outcome ~ age_cat, family = "binomial", data = data)
model17
summary(model7)

## Create function that returns a pretty regression table
pretty_regression_table <- function(model.object){
  cis <- confint(model.object)
  or.ci <- round(exp(cbind(OR = coef(model.object), cis)), digits = 2)
  reg.table.names <- c("Parameter", "OR", "95% CI")
  reg.table <- as.data.frame(cbind(rownames(or.ci),
                                   or.ci[, "OR"],
                                   paste0(or.ci[, "2.5 %"], ", ", or.ci[, "97.5 %"])),
                             stringsAsFactors = FALSE)
  reg.table <- reg.table[-grep("(Intercept)", rownames(reg.table)), ]
  colnames(reg.table) <- reg.table.names
  ivar.name <- attr(model.object$terms, "term.labels")
  ivar <- as.character(model.object$model[, ivar.name])
  reg.table$Parameter <- gsub(ivar.name, "", reg.table$Parameter)
  reference <- unique(ivar)[!(unique(ivar) %in% reg.table$Parameter)]
  reference.row <- c(paste0("Reference: ", reference), "1", "", "")
  variable.label <- attr(model.object$model[, ivar.name], "label")
  if (is.null(variable.label))
    variable.label <- ivar.name
  variable.row <- c(variable.label, "", "", "")
  pretty.table <- rbind(variable.row, reference.row, reg.table)
  rownames(pretty.table) <- NULL
  prettier.table <- knitr::kable(pretty.table)
  return (prettier.table)
}
pretty_regression_table(model17)


## Univariate Logistic regression of weekend with Outcome
model18 <- glm(head_outcome ~ days_weekend, family = "binomial", data = data)
model8
summary(model8)
pretty_regression_table(model18)

## Univariate Logistic regression of daylight vs night with Outcome
model19 <- glm(head_outcome ~ day_night, family = "binomial", data = data)
model19
summary(model19)
pretty_regression_table(model19)

## Univariate Logistic regression of gender with Outcome
model20 <- glm(head_outcome ~ gender, family = "binomial", data = data)
model20
summary(model20)
pretty_regression_table(model20)

## Univariate Logistic regression of helmet use with Outcome
model21 <- glm(head_outcome ~ helmet_use, family = "binomial", data = data)
model21
summary(model21)
pretty_regression_table(model21)

## Univariate Logistic regression of location with Outcome
model22 <- glm(head_outcome ~ data$location_detail, family = "binomial", data = data)
model22
summary(model22)
pretty_regression_table(model22)

## Univariate Logistic regression of arrival-Vehicle with Outcome
model23 <- glm(head_outcome ~ arrival_vehicle, family = "binomial", data = data)
model23
summary(model23)
pretty_regression_table(model23)



#Multivariate Logistic reg
model24 <- glm(head_outcome ~  age_cat + days_weekend + day_night + location_detail +
                  outcome + helmet_use ,
               family = "binomial", data = data)
summary(model24)
or.ci<- exp(cbind(OR = coef(model24), confint(model24)))
round(or.ci, digits=2)




