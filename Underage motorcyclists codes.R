
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
data <- data.table::fread("Underage RTIs1.csv", data.table = FALSE)

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

####   Day light and night #####
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

#Vehicle
str(data$vehicle_1)
data$vehicle_1 <- as.factor(data$vehicle_1)
levels(data$vehicle_1) <- c("Motorcyle","MiniVan/Coaster","Bus/Minibus/Coach","Truck","Taxi",
                            "Bicycle", "Car","Water/oiltanker","Rickshaw","Dumper","Trailor",
                            "Loading Pick up", "Others", "If loaded Vehicle then","Animal Cart",
                            "Push Cart","Train","Missing")
table(data$vehicle_1)

#Vehicle
str(data$vehicle_2)
data$vehicle_2 <- as.factor(data$vehicle_2)
levels(data$vehicle_2) <- c("Motorcyle","MiniVan/Coaster","Bus/Minibus/Coach","Truck","Taxi",
                            "Bicycle", "Car","Water/oiltanker","Rickshaw","Dumper","Trailor",
                            "Loading Pick up", "Others", "If loaded Vehicle then","Animal Cart",
                            "Push Cart","Train","Missing")
table(data$vehicle_2)

#Intersection vs midblock crash
str(data$location_detail)
data$location_detail <- as.factor(data$location_detail)
levels(data$location_detail) <- c("Intersection","Midblock")
table(data$location_detail)

##### Head injuries ######
data$head_cat <-as.factor(ifelse(is.na(data$cleaned_head),0,1))
levels(data$head_cat) <-c("No","Yes")
table(data$head_cat)


##### Face ######
data$face_cat <-as.factor(ifelse(is.na(data$cleaned_face),0,1))
levels(data$face_cat) <-c("No","Yes")
table(data$face_cat)


##### Chest ######
data$chest_cat <-as.factor(ifelse(is.na(data$chest_thorax_cleaned),0,1))
levels(data$chest_cat) <-c("No","Yes")
table(data$chest_cat)



##### Abdomen ######

data$abdomen_cat <-as.factor(ifelse(is.na(data$abdomen_cleaned),0,1))
levels(data$abdomen_cat) <-c("No","Yes")
table(data$abdomen_cat)

## Spine injury ##

data$spine_cat <-as.factor(ifelse(is.na(data$spine_cleaned),0,1))
                                 
levels(data$spine_cat) <-c("No","Yes")
table(data$spine_cat)

##### Extremities ######

data$extremity_cat <-as.factor(ifelse(is.na(data$final_extremity),0,1))
levels(data$extremity_cat) <-c("No","Yes")
table(data$extremity_cat)

##### External ######

data$external_cat <-as.factor(ifelse(is.na(data$external_cleaned),0,1))
levels(data$external_cat) <-c("No","Yes")
table(data$external_cat)

###### Disposition ######
str(data$disposition)
data$disposition[data$disposition == "99"] <- NA
data$disposition <- as.factor(data$disposition)
levels(data$disposition) <- c("Discharged","Admitted to ward","Detained","Death","Transferred","LAMA","Referred from")
table(data$disposition)

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



### Cross table between age and injuries in body parts
#tabyl(data, road_user, vehicle_1)
#tabyl(data, road_user, vehicle_1) %>%
 # adorn_percentages("col")%>%
  #adorn_pct_formatting(digits = 1)

### Cross table between Vehicle 1  and vehicle 2
#tabyl(data,vehicle_1, vehicle_2)
#tabyl(data, vehicle_1, vehicle_2) %>%
#adorn_percentages("col")%>%
#adorn_pct_formatting(digits = 1)

#tabyl(data, vehicle_1, vehicle_2) %>%
#  adorn_percentages("row")%>%
# adorn_pct_formatting(digits = 1)


tabyl(data, vehicle_1, vehicle_2) %>%
 adorn_percentages("col")%>%
adorn_pct_formatting(digits = 1)

#tabyl(data, vehicle_1, vehicle_2) %>%
#  adorn_percentages("row")%>%
 # adorn_pct_formatting(digits = 1)

#tabyl(data, vehicle_2, vehicle_1) %>%
 # adorn_percentages("col")%>%
  #adorn_pct_formatting(digits = 1)

#tabyl(data, vehicle_2, vehicle_1) %>%
 # adorn_percentages("row")%>%
  #adorn_pct_formatting(digits = 1)

#tabyl(data, vehicle_1, vehicle_2, age_cat) %>%
 # adorn_percentages("col")%>%
  #adorn_pct_formatting(digits = 1)

#tabyl(data, vehicle_1, vehicle_2, age_cat) %>%
 #       adorn_percentages("col")%>%
  #      adorn_pct_formatting(digits = 1)
      
   #   tabyl(data, vehicle_1, vehicle_2, age_cat) %>%
    #    adorn_percentages("row")%>%
     #   adorn_pct_formatting(digits = 1) 
        
## Categorical variables in the data
catVars <- c("age_cat",
             "gender",
             "days_weekend",
             "day_night",
             "arrival_vehicle",
             "vehicle_1",
             "vehicle_2",
             "location_detail",
             "helmet_use",
            "head_cat", 
            "face_cat",
            "extremity_cat",  
            "abdomen_cat",
            "chest_cat",
            "spine_cat",
            "external_cat", 
             "disposition",  
             "outcome",  
             "iss_score_final")
data[catVars] <- lapply(data[catVars], as.factor)

## Create a label list
label.list <-list(age_cat = "Age in Years",
                  gender = "Gender",
                  days_weekend= "Weekdays",
                  day_night = "Day Night",
                  arrival_vehicle = "Arrival Vehicle",
                  location_detail = "Crash Location",
                  helmet_use = "Helmet use",
                  head_cat = "Head Injury",
                  face_cat = "Face Injury",
                  extremity_cat = "Upper Lower Extrimity",
                  abdomen_cat = "Abdominal Injury",  
                  chest_cat ="Chest Injury",
                  spine_cat ="spine Injury",
                  external_cat = "External Injury",
                  disposition = "Hospital Disposition",
                  outcome = "Final Disposition",
                  vehicle_1= "Crash Vehicle 1",
                  vehicle_2= "Crash Vehicle 2",
                iss_score_final = "ISS-Score")

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
table2 <- tableone::CreateTableOne(strata = "age_cat", data = data, factorVar = catVars)
pretty.table2 <- tableone:::print.TableOne(table2, varLabels = TRUE, showAllLevels = TRUE, test = FALSE)
colnames(pretty.table2)[colnames(pretty.table2) == "level"] <- "Level"
prettier.table <- knitr::kable(pretty.table2)
tmp.file <- tempfile(tmpdir = ".", fileext = "md")
write(prettier.table, tmp.file)
rmarkdown::render(tmp.file, output_file = "Table 2.docx", output_format = "word_document")
file.remove(tmp.file)

### Create Table 3
table3 <- tableone::CreateTableOne(strata = "outcome", data = data, factorVar = catVars)
pretty.table3 <- tableone:::print.TableOne(table3, varLabels = TRUE, showAllLevels = TRUE, test = FALSE)
colnames(pretty.table3)[colnames(pretty.table3) == "level"] <- "Level"
prettier.table <- knitr::kable(pretty.table3)
tmp.file <- tempfile(tmpdir = ".", fileext = "md")
write(prettier.table, tmp.file)
rmarkdown::render(tmp.file, output_file = "Table 3.docx", output_format = "word_document")
file.remove(tmp.file)

##### Create Table 4
table4 <- tableone::CreateTableOne(strata = "iss_score_final", data = data, factorVar = catVars)
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
model6 <- glm(outcome ~ location_detail, family = "binomial", data = data)
model6
summary(model6)
pretty_regression_table(model6)

## Univariate Logistic regression of arrival-Vehicle with Outcome
model7 <- glm(outcome ~ arrival_vehicle, family = "binomial", data = data)
model7
summary(model7)
pretty_regression_table(model7)

## Univariate Logistic regression of ISS with Outcome
model11 <- glm(outcome ~ iss_score_final, family = "binomial", data = data)
model11
summary(model11)
pretty_regression_table(model11)




#Multivariate Logistic reg
model16 <- glm(outcome ~  age_cat +gender + days_weekend + day_night + location_detail +
                 arrival_vehicle + helmet_use + iss_score_final,
               family = "binomial", data = data)
summary(model16)
or.ci<- exp(cbind(OR = coef(model16), confint(model16)))
round(or.ci, digits=2)




#Outcome 2 ISS score 16

# Univariate Logistic regression of age groups with ISS
data$age_cat <- relevel(data$age_cat, ref = "20 to 24")

labelled::var_label(data$age_cat) <- "Age group"
model18 <- glm(iss_score_final ~ age_cat, family = "binomial", data = data)
model18
summary(model18)

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
pretty_regression_table(model18)


## Univariate Logistic regression of weekend with ISS
model19<- glm(iss_score_final ~ days_weekend, family = "binomial", data = data)
model19
summary(model19)
pretty_regression_table(model19)

## Univariate Logistic regression of daylight vs night with ISS
model20 <- glm(iss_score_final ~ day_night, family = "binomial", data = data)
model20
summary(model20)
pretty_regression_table(model20)

## Univariate Logistic regression of gender with ISS
model21 <- glm(iss_score_final ~ gender, family = "binomial", data = data)
model21
summary(model21)
pretty_regression_table(model21)

## Univariate Logistic regression of helmet use with ISS
model22 <- glm(iss_score_final ~ helmet_use, family = "binomial", data = data)
model22
summary(model22)
pretty_regression_table(model22)

## Univariate Logistic regression of location with Outcome
model23 <- glm(iss_score_final ~ location_detail, family = "binomial", data = data)
model23
summary(model23)
pretty_regression_table(model23)

## Univariate Logistic regression of arrival-Vehicle with ISS
model24 <- glm(iss_score_final ~ arrival_vehicle, family = "binomial", data = data)
model24
summary(model24)
pretty_regression_table(model24)

# 


#Univariate death with ISS
model26 <- glm(iss_score_final ~ outcome, family = "binomial", data = data)
model26
summary(model26)
pretty_regression_table(model26)

#Multivariate Logistic reg
model27 <- glm(iss_score_final ~  age_cat + gender +days_weekend  + day_night +location_detail + helmet_use + arrival_vehicle + outcome,
               family = "binomial", data = data)
summary(model27)
or.ci<- exp(cbind(OR = coef(model27), confint(model27)))
round(or.ci, digits=2)



