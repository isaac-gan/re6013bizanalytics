library(data.table)
library(eeptools)
library(lubridate)
library(readr)
library(tidyverse)
library(ROSE)
library(nnet)
library(caTools)
setwd('~/Documents/re6013bizanalytics/vehicle')

VehicleLoan.dt <- fread('VehicleLoan.csv', na.strings = c("NA", "missing", "N/A", -99, "", "m", "M", "na", "."))

#remove irrelevant data columns
VehicleLoan.dt[, c("branch_id","supplier_id","manufacturer_id","Current_pincode_ID","State_ID","Employee_code_ID","NO.OF_INQUIRIES","UniqueID"):=NULL]

#convert Date.of.Birth column to age
dates <- as.Date(parse_date_time(VehicleLoan.dt$Date.of.Birth,"dmy"))
VehicleLoan.dt$age <- floor(age_calc(dates, units = "years"))
VehicleLoan.dt[, "Date.of.Birth":=NULL]

#convert NAs in employment.type to unemployed
sum(is.na(VehicleLoan.dt))
sum(is.na(VehicleLoan.dt$Employment.Type))

VehicleLoan.dt[is.na(Employment.Type), employment:='unemployed']
VehicleLoan.dt[Employment.Type == 'Salaried', employment:='salaried']
VehicleLoan.dt[Employment.Type == 'Self employed', employment:='self employed']
VehicleLoan.dt$employment <- factor(VehicleLoan.dt$employment, levels = c('unemployed', 'salaried', 'self employed'))
VehicleLoan.dt[, "Employment.Type":=NULL]

#convert Disbursal Date column to Disbursal Month column
dates <- as.Date(parse_date_time(VehicleLoan.dt$DisbursalDate,"dmy"))
VehicleLoan.dt$DisbursalMonth <- month(dates)
VehicleLoan.dt[, "DisbursalDate":=NULL]

#remove all flags columns as assume that information can be requested from customer
VehicleLoan.dt[, c("MobileNo_Avl_Flag","Aadhar_flag","PAN_flag","VoterID_flag","Driving_flag","Passport_flag"):=NULL]

#remove rows with PERFORM_CNS.SCORE == 0
#VehicleLoan.dt[PERFORM_CNS.SCORE ==0] <- NA
#VehicleLoan.dt<-VehicleLoan.dt[complete.cases(VehicleLoan.dt),]

#Remove CNS score description column
VehicleLoan.dt[,"PERFORM_CNS.SCORE.DESCRIPTION":=NULL]

#change average account age and credit history length columns to be in terms of months
year = parse_number( word(VehicleLoan.dt$AVERAGE.ACCT.AGE,1) )
month = parse_number( word(VehicleLoan.dt$AVERAGE.ACCT.AGE,2) )
VehicleLoan.dt$AVERAGE.ACCT.AGE <- year*12 + month

year = parse_number( word(VehicleLoan.dt$CREDIT.HISTORY.LENGTH,1) )
month = parse_number( word(VehicleLoan.dt$CREDIT.HISTORY.LENGTH,2) )
VehicleLoan.dt$CREDIT.HISTORY.LENGTH <- year*12 + month

#Combining primary and secondary accounts
VehicleLoan.dt$NO.OF.ACCTS = VehicleLoan.dt$PRI.NO.OF.ACCTS + VehicleLoan.dt$SEC.NO.OF.ACCTS  
VehicleLoan.dt$ACTIVE.ACCTS = VehicleLoan.dt$PRI.ACTIVE.ACCTS + VehicleLoan.dt$SEC.ACTIVE.ACCTS
VehicleLoan.dt$OVERDUE.ACCTS = VehicleLoan.dt$PRI.OVERDUE.ACCTS + VehicleLoan.dt$SEC.OVERDUE.ACCTS
VehicleLoan.dt$ACCTS.CURRENT.BALANCE = VehicleLoan.dt$PRI.CURRENT.BALANCE + VehicleLoan.dt$SEC.CURRENT.BALANCE
VehicleLoan.dt$ACCTS.SANCTIONED.AMOUNT = VehicleLoan.dt$PRI.SANCTIONED.AMOUNT + VehicleLoan.dt$SEC.SANCTIONED.AMOUNT
VehicleLoan.dt$ACCTS.DISBURSED.AMOUNT = VehicleLoan.dt$PRI.DISBURSED.AMOUNT + VehicleLoan.dt$SEC.DISBURSED.AMOUNT  
VehicleLoan.dt$ACCTS.INSTAL.AMT = VehicleLoan.dt$PRIMARY.INSTAL.AMT + VehicleLoan.dt$SEC.INSTAL.AMT

VehicleLoan.dt[, c("PRI.NO.OF.ACCTS","SEC.NO.OF.ACCTS","PRI.ACTIVE.ACCTS","SEC.ACTIVE.ACCTS","PRI.OVERDUE.ACCTS","SEC.OVERDUE.ACCTS"):=NULL]
VehicleLoan.dt[, c("PRI.CURRENT.BALANCE","SEC.CURRENT.BALANCE","PRI.SANCTIONED.AMOUNT","SEC.SANCTIONED.AMOUNT","PRI.DISBURSED.AMOUNT","SEC.DISBURSED.AMOUNT","PRIMARY.INSTAL.AMT","SEC.INSTAL.AMT"):=NULL]

#factorise columns
VehicleLoan.dt$loan_default <- factor(VehicleLoan.dt$loan_default)
VehicleLoan.dt$DisbursalMonth <- factor(VehicleLoan.dt$DisbursalMonth, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))

#logistic regression
levels(VehicleLoan.dt$loan_default)
m1 <- glm(loan_default ~ . , family = binomial, data = VehicleLoan.dt)

summary(m1)
#ACCTS.SANCTIONED.AMOUNT, ACCTS.DISBURSED.AMOUNT and ACCTS.INSTAL.AMT have p-values > 0.5, hence remove these variables

m2 <- glm(loan_default ~ . -ACCTS.SANCTIONED.AMOUNT -ACCTS.DISBURSED.AMOUNT -ACCTS.INSTAL.AMT, family = binomial, data = VehicleLoan.dt)
summary(m2)

OR.m2 <- exp(coef(m2))
OR.m2


prob <- predict(m2, type = 'response')

# If Threshold = 0.5 ---------------------------------------------
threshold1 <- 0.5

default.hat.1 <- ifelse(prob > threshold1, 1, 0)

table1 <- table(VehicleLoan.dt$loan_default, default.hat.1)
table1
prop.table(table1)

# Overall Accuracy if same misclassification costs
mean(default.hat.1 == VehicleLoan.dt$loan_default)

# Train-Test split ---------------------------------------------------
set.seed(2014)
train <- sample.split(Y = VehicleLoan.dt$loan_default, SplitRatio = 0.7)
trainset <- subset(VehicleLoan.dt, train == T)
testset <- subset(VehicleLoan.dt, train == F)

m3 <- glm(loan_default ~ . -ACCTS.SANCTIONED.AMOUNT -ACCTS.DISBURSED.AMOUNT -ACCTS.INSTAL.AMT, family = binomial, data = trainset)

summary(m3)

#p-value of NO.OF.ACCTS > 0.5, remove variable
m4 <- glm(loan_default ~ . -NO.OF.ACCTS -ACCTS.SANCTIONED.AMOUNT -ACCTS.DISBURSED.AMOUNT -ACCTS.INSTAL.AMT, family = binomial, data = trainset)

summary(m4)

OR <- exp(coef(m4))
OR

# Confusion Matrix on Trainset
prob.train <- predict(m4, type = 'response')
predict.default.train <- ifelse(prob.train > threshold1, "1", "0")
table3 <- table(trainset$loan_default, predict.default.train)
table3
prop.table(table3)
# Overall Accuracy
mean(predict.default.train == trainset$loan_default)

# Confusion Matrix on Testset
prob.test <- predict(m4, newdata = testset, type = 'response')
predict.default.test <- ifelse(prob.test > threshold1, 1, 0)
table4 <- table(testset$loan_default, predict.default.test)
table4
prop.table(table4)

# Overall Accuracy
mean(predict.default.test == testset$loan_default)


# Sample the majority to address imbalanced data & use same testset to test ----
# Random sample from majority class Default = No and combine with Default = Yes to form new trainset -----
majority <- trainset[loan_default == 0]

minority <- trainset[loan_default == 1]

# Randomly sample the row numbers to be in trainset. Same sample size as minority cases. 
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority))

# Subset the original trainset based on randomly chosen row numbers.
majority.chosen <- majority[chosen]

# Combine two data tables by appending the rows
trainset.bal <- rbind(majority.chosen, minority)
summary(trainset.bal)
## Check trainset is balanced.

# Logistic Reg on balanced data --------------------------------------------------
m5 <- glm(loan_default ~ . -NO.OF.ACCTS -ACCTS.SANCTIONED.AMOUNT -ACCTS.DISBURSED.AMOUNT -ACCTS.INSTAL.AMT, family = binomial, data = trainset.bal)

# Confusion Matrix on balanced Trainset
prob.train <- predict(m5, type = 'response')
predict.default.train <- ifelse(prob.train > threshold1, 1, 0)
table5 <- table(trainset.bal$loan_default, predict.default.train)
table5
prop.table(table5)
# Overall Accuracy
mean(predict.default.train == trainset.bal$loan_default)

# Confusion Matrix on balanced Testset
prob.test <- predict(m5, newdata = testset, type = 'response')
predict.default.test <- ifelse(prob.test > threshold1, 1, 0)
table6 <- table(testset$loan_default, predict.default.test)
table6
prop.table(table6)

# Overall Accuracy
mean(predict.default.test == testset$loan_default)

## True positive increased but true negative decreased, as expected on balanced data.

saveRDS(m5, "./vehicle_eligibility_LOGREG.rds")
