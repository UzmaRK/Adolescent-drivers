
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
#install.packages("forecast")
library(forecast)
#install.packages("tseries")
library(tseries)
#install.packages("janitor")
library(janitor)
#install.packages("vtree")
library(vtree)

## Reading the data from the above mentioned directory
data <- data.table::fread("Young motorcyclists RTIs.csv", data.table = FALSE)

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

#### Winter/Summer Season months ####

data$season <- as.factor(data$season)
levels(data$season) <- c("Winter months","Summer months")
table(data$season)

#Professional
data$profession <- as.factor(data$profession)
levels(data$profession) <- c("students","professionals")
table(data$profession)



####   Day light and night #####
data$daylight_night <- as.factor(data$daylight_night )
levels(data$daylight_night ) <- c("Daylight","Dark")
table(data$daylight_night )
summary(data$daylight_night)

### Hospital Category ###
data$hospital <- as.factor(data$hospital)
levels(data$hospital) <- c("JPMC","LNH","AKU","ASH","Civil")
table(data$hospital)
summary(data$hospital)

### Profession ###
data$profession <- as.factor(data$profession)
levels(data$profession) <- c("student","Others")
table(data$profession)

##district
#### district #####
data$district <- as.character(data$district)
data$district[data$district == "1"] <- "Central"
data$district[data$district == "2"] <- "East"
data$district[data$district == "3"] <- "South"
data$district[data$district == "4"] <- "West"
data$district[data$district == "5"] <- "Kemari"
data$district[data$district == "6"] <- "Korangi"
data$district[data$district == "7"] <- "Malir"
data$district[data$district == "8"] <- "Out of city"
table(data$district)


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


#location_ crash
str(data$location_crash)
data$location_crash <- as.factor(data$location_crash)
levels(data$location_crash) <- c("Intersection","Midblock")
table(data$location_crash)


##### GCS #######
data$gcs_cat <- as.factor(data$gcs)
levels(data$gcs_cat) <- c("13 to 15","9 to 12","6 to 8", "4 to 5", "3")
table(data$gcs_cat)
summary(data$gcs_cat)


### outcome death ##
table(data$outcome, useNA = "always")
data$outcome[data$outcome == "99"] <- NA
data$outcome <- as.factor(data$outcome)
levels(data$outcome) <- c("Survive","Non-Survive")
table (data$outcome)

#ISS cat

## categorizing ISS
data$iss_score_final <- as.character(data$iss_score_final)
data$iss_score_final[data$iss_score_final == "0"] <- "<16"
data$iss_score_final[data$iss_score_final == "1"] <- ">=16"
data$iss_score_final <- as.factor(data$iss_score_final)
table(data$iss_score_final)

names(data)


##### Head injuries ######
data$head_cat <-as.factor(ifelse(is.na(data$head_merged_final),0,1))
levels(data$head_cat) <-c("No","Yes")
table(data$head_cat)


##### Face ######
data$face_cat <-as.factor(ifelse(is.na(data$face_merged_final),0,1))
levels(data$face_cat) <-c("No","Yes")
table(data$face_cat)


##### Chest ######
data$chest_cat <-as.factor(ifelse(is.na(data$thorax_chest_merged_final),0,1))
levels(data$chest_cat) <-c("No","Yes")
table(data$chest_cat)



##### Abdomen ######

data$abdomen_cat <-as.factor(ifelse(is.na(data$merged_abdomen),0,1))
levels(data$abdomen_cat) <-c("No","Yes")
table(data$abdomen_cat)

## Spine injury ##

data$spine_cat <-as.factor(ifelse(is.na(data$spine_merged_final),0,1))

levels(data$spine_cat) <-c("No","Yes")
table(data$spine_cat)

##### Extremities ######

data$extremity_cat <-as.factor(ifelse(is.na(data$extrimity_merged_final),0,1))
levels(data$extremity_cat) <-c("No","Yes")
table(data$extremity_cat)

##### External ######

data$external_cat <-as.factor(ifelse(is.na(data$external_merged_final),0,1))
levels(data$external_cat) <-c("No","Yes")
table(data$external_cat)


names(data)

## Categorical variables in the data
catVars <- c("age_cat",
             "gender",
             "profession",
             "daylight_night",
             "days_weekend",
             "season",
             "location_crash",
             "district",
             "helmet_use",
             "arrival_vehicle",
             "head_cat", 
             "face_cat",
             "extremity_cat",  
             "abdomen_cat",
             "chest_cat",
             "spine_cat",
             "external_cat",
             "hospital",
             "gcs_cat",
             "iss_score_final",
             "outcome")
data[catVars] <- lapply(data[catVars], as.factor)

## Categorical variables in the data
catVars1 <- c("age_cat",
              "gender",
              "profession",
              "daylight_night",
              "days_weekend",
              "season",
              "location_crash",
              "district",
              "helmet_use",
              "arrival_vehicle",
              "head_cat", 
              "face_cat",
              "extremity_cat",  
              "abdomen_cat",
              "chest_cat",
              "spine_cat",
              "external_cat",
              "hospital",
              "gcs_cat",
              "iss_score_final",
              "outcome")
data[catVars1] <- lapply(data[catVars1], as.factor)


## Create a label list
label.list <-list(age_cat = "Age in Years",
                  gender = "Gender",
                  profession = "Profession",
                  daylight_night = "Daylight Night",
                  days_weekend= "Weekdays",
                  season = "Winters Summers",
                  location_crash = "Crash location",
                  district ="District",
                  helmet_use = "Helmet use",
                  arrival_vehicle = "Arrival Vehicle",
                  hospital = "Hospital",
                  gcs_cat = "GCS Score",
                  head_cat = "Head Injury",
                  face_cat = "Face Injury",
                  extremity_cat = "Upper Lower Extrimity",
                  abdomen_cat = "Abdominal Injury",  
                  chest_cat ="Chest Injury",
                  spine_cat ="spine Injury",
                  external_cat = "External Injury",
                  iss_score_final = "ISS-Score",
                  outcome = "Death Survived")

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

## Create Table 2
table2 <- tableone::CreateTableOne(strata = "age_cat", data = data, factorVar = catVars1)
pretty.table2 <- tableone:::print.TableOne(table2, varLabels = TRUE, showAllLevels = TRUE, test = FALSE)
colnames(pretty.table2)[colnames(pretty.table2) == "level"] <- "Level"
prettier.table <- knitr::kable(pretty.table2)
tmp.file <- tempfile(tmpdir = ".", fileext = "md")
write(prettier.table, tmp.file)
rmarkdown::render(tmp.file, output_file = "Table 2.docx", output_format = "word_document")
file.remove(tmp.file)

### Create Table 3
table3 <- tableone::CreateTableOne(strata = "outcome", data = data, factorVar = catVars1)
pretty.table3 <- tableone:::print.TableOne(table3, varLabels = TRUE, showAllLevels = TRUE, test = FALSE)
colnames(pretty.table3)[colnames(pretty.table3) == "level"] <- "Level"
prettier.table <- knitr::kable(pretty.table3)
tmp.file <- tempfile(tmpdir = ".", fileext = "md")
write(prettier.table, tmp.file)
rmarkdown::render(tmp.file, output_file = "Table 3.docx", output_format = "word_document")
file.remove(tmp.file)

##### Create Table 4
table4 <- tableone::CreateTableOne(strata = "iss_score_final", data = data, factorVar = catVars1)
pretty.table4 <- tableone:::print.TableOne(table4, varLabels = TRUE, showAllLevels = TRUE, test = FALSE)
colnames(pretty.table4)[colnames(pretty.table4) == "level"] <- "Level"
prettier.table <- knitr::kable(pretty.table4)
tmp.file <- tempfile(tmpdir = ".", fileext = "md")
write(prettier.table, tmp.file)
rmarkdown::render(tmp.file, output_file = "Table 4.docx", output_format = "word_document")
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

## Univariate Logistic regression of weekend with Outcome death
data$profession <- relevel(data$profession, ref = "Student")
labelled::var_label(data$profession) <- "Profession"
model2 <- glm(outcome ~ profession, family = "binomial", data = data)
model2
summary(model2)
pretty_regression_table(model2)

## Univariate Logistic regression of daylight vs night with Outcome death
data$daylight_night <- relevel(data$daylight_night, ref = "Daylight")
labelled::var_label(data$daylight_night) <- "Day and Night"
model3 <- glm(outcome ~ daylight_night, family = "binomial", data = data)
model3
summary(model3)
pretty_regression_table(model3)

## Univariate Logistic regression of gender with Outcome detah
model4 <- glm(outcome ~ gender, family = "binomial", data = data)
model4
summary(model4)
pretty_regression_table(model4)

## Univariate Logistic regression of helmet use with Outcome death
model5 <- glm(outcome ~ helmet_use, family = "binomial", data = data)
model5
summary(model5)
pretty_regression_table(model5)

## Univariate Logistic regression of location_crash with Outcome
model6 <- glm(outcome ~ location_crash, family = "binomial", data = data)
model6
summary(model6)
pretty_regression_table(model6)

## Univariate Logistic regression of arrival-Vehicle with Outcome death
model7 <- glm(outcome ~ arrival_vehicle, family = "binomial", data = data)
model7
summary(model7)
pretty_regression_table(model7)

## Univariate Logistic regression of ISS with Outcome death
model8 <- glm(outcome ~ iss_score_final, family = "binomial", data = data)
model8
summary(model8)
pretty_regression_table(model8)

###### Univariate logistic regression of GCS category with death ####
model9 <- glm(outcome ~ gcs_cat, family = "binomial", data = data)
model9
summary(model9)
pretty_regression_table(model9)

#Univariate of Season with death
model10 <- glm(outcome ~ season, family = "binomial", data = data)
model10
summary(model10)
pretty_regression_table(model10)

#Univariate of  hospital with death
data$hospital <- relevel(data$hospital, ref = "JPMC")
labelled::var_label(data$hospital) <- "Hospital Category"
model11 <- glm(outcome ~ hospital, family = "binomial", data = data)
model11
summary(model11)
pretty_regression_table(model11)

#Univariate of district with death
model12 <- glm(outcome ~ district, family = "binomial", data = data)
model12
summary(model12)
pretty_regression_table(model12)

#Univariate of town with death
model13 <- glm(outcome ~ days_weekend, family = "binomial", data = data)
model13
summary(model13)
pretty_regression_table(model13)



#Multivariate Logistic reg with district
model14 <- glm(outcome ~  age_cat +gender  + profession +  daylight_night + days_weekend +   season + location_crash + district + helmet_use +
                 arrival_vehicle + hospital  + gcs_cat + iss_score_final ,
               family = "binomial", data = data)
summary(model14)
or.ci<- exp(cbind(OR = coef(model14), confint(model14)))
round(or.ci, digits=2)

#MUlti 2 
model144 <- glm(outcome ~  age_cat + gender  + daylight_night + days_weekend + season  + district + helmet_use +
                  arrival_vehicle + hospital  + gcs_cat + iss_score_final + ,
               family = "binomial", data = data)
summary(model144)
or.ci<- exp(cbind(OR = coef(model144), confint(model144)))
round(or.ci, digits=2)



#Outcome 2 ISS score 16 or more

# Univariate Logistic regression of age groups with ISS
data$age_cat <- relevel(data$age_cat, ref = "20 to 24")

labelled::var_label(data$age_cat) <- "Age group"
model16 <- glm(iss_score_final ~ age_cat, family = "binomial", data = data)
model16
summary(model16)

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
pretty_regression_table(model16)


## Univariate Logistic regression of weekend with ISS
model17<- glm(iss_score_final ~ days_weekend, family = "binomial", data = data)
model17
summary(model17)
pretty_regression_table(model17)

## Univariate Logistic regression of daylight vs night with ISS
model18 <- glm(iss_score_final ~ daylight_night, family = "binomial", data = data)
model18
summary(model18)
pretty_regression_table(model18)

## Univariate Logistic regression of gender with ISS
model19 <- glm(iss_score_final ~ gender, family = "binomial", data = data)
model19
summary(model19)
pretty_regression_table(model19)

## Univariate Logistic regression of helmet use with ISS
model20 <- glm(iss_score_final ~ helmet_use, family = "binomial", data = data)
model20
summary(model20)
pretty_regression_table(model20)

## Univariate Logistic regression of location with ISS
model21 <- glm(iss_score_final ~ location_crash, family = "binomial", data = data)
model21
summary(model21)
pretty_regression_table(model21)

## Univariate Logistic regression of arrival-Vehicle with ISS
model22 <- glm(iss_score_final ~ arrival_vehicle, family = "binomial", data = data)
model22
summary(model22)
pretty_regression_table(model22)

#Univariate of season with ISS
model23 <- glm(iss_score_final ~ season, family = "binomial", data = data)
model23
summary(model23)
pretty_regression_table(model23)

#Univariate  of hospital  with ISS
model24 <- glm(iss_score_final ~ hospital, family = "binomial", data = data)
model24
summary(model24)
pretty_regression_table(model24)


#Univariate  of district  with ISS
model25 <- glm(iss_score_final ~ district, family = "binomial", data = data)
model25
summary(model25)
pretty_regression_table(model25)

#Univariate  of town  with ISS
model26 <- glm(iss_score_final ~ profession, family = "binomial", data = data)
model26
summary(model26)
pretty_regression_table(model26)

#Multivariate Logistic reg with district in model
model27 <- glm(iss_score_final ~  age_cat + gender + profession + daylight_night + days_weekend + season +location_crash + district + helmet_use + arrival_vehicle + hospital + gcs_cat,
               family = "binomial", data = data)
summary(model27)
or.ci<- exp(cbind(OR = coef(model27), confint(model27)))
round(or.ci, digits=2)

#Multivariate Logistic reg without crash_location and profession
model28 <- glm(iss_score_final ~  age_cat + gender +  days_weekend  + day_night  + helmet_use + arrival_vehicle + district + season + gcs_cat + hospital,
               family = "binomial", data = data)
summary(model28)
or.ci<- exp(cbind(OR = coef(model28), confint(model28)))
round(or.ci, digits=2)



