library(data.table)
library(rpart)
library(rpart.plot)
library(eeptools)
library(lubridate)
library(readr)
library(tidyverse)
library(ROSE)
library(nnet)
library(caTools)
library(caret)
library(MLmetrics)
library(cvms) # install.packages("cvms") for confustion matrix
library(tibble) # for evaluation of confustion matrix
library(corrplot)
library(DMwR2)
library(e1071)


setwd('~/Documents/re6013bizanalytics')

# Customer Data VEHICLE
CUSTOMER.name <- "Jane Doe"
CUSTOMER.vehicle_cost <- 10000
CUSTOMER.employment <- "salaried"
CUSTOMER.current_month <- 8
CUSTOMER.bureau_score <- 700
CUSTOMER.active_accounts <- 0
CUSTOMER.overdue_accounts <- 0
CUSTOMER.oustanding_loans <- 0
CUSTOMER.new_loans_6months <- 0
CUSTOMER.defaulted_loans_6months <- 0
CUSTOMER.avrg_loan_tenure <- 1
CUSTOMER.credit_history <- 0
CUSTOMER.age <- 40

# Customer Data HOME
CUSTOMER.monthly_bracket <- 180
CUSTOMER.income <- 20000
CUSTOMER.gender <- "F"





# Models
model.vehicle.eligibility <- readRDS("./vehicle/vehicle_eligibility_LOGREG.rds")
model.vehicle.amount <- readRDS("./vehicle/vehicle_amount_CART.rds")
model.home.eligibility <- readRDS("./home/home_eligibility_CART.rds")
model.home.amount <- readRDS("./home/home_amount_CART.rds")

# Check that all models are loaded
printcp(model.vehicle.amount, digits = 3)
summary(model.vehicle.eligibility)
printcp(model.home.eligibility, digits = 3)
printcp(model.home.amount, digits = 3)

# create datatable for vehicle models
data.vehicle = data.table(
  asset_cost = CUSTOMER.vehicle_cost,
  employment = CUSTOMER.employment,
  DisbursalMonth = CUSTOMER.current_month ,
  PERFORM_CNS.SCORE = CUSTOMER.bureau_score ,
  ACTIVE.ACCTS = CUSTOMER.active_accounts ,
  OVERDUE.ACCTS = CUSTOMER.overdue_accounts ,
  ACCTS.CURRENT.BALANCE = CUSTOMER.oustanding_loans ,
  NEW.ACCTS.IN.LAST.SIX.MONTHS = CUSTOMER.new_loans_6months ,
  DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS = CUSTOMER.defaulted_loans_6months ,
  AVERAGE.ACCT.AGE = CUSTOMER.avrg_loan_tenure ,
  CREDIT.HISTORY.LENGTH = CUSTOMER.credit_history ,
  age = CUSTOMER.age ,
  NO.OF.ACCTS = 1,
  ACCTS.SANCTIONED.AMOUNT =2, 
  ACCTS.DISBURSED.AMOUNT = 3,
  ACCTS.INSTAL.AMT = 1
)
data.vehicle$employment <- factor(data.vehicle$employment, levels = c('unemployed', 'salaried', 'self employed'))
data.vehicle$DisbursalMonth <- factor(data.vehicle$DisbursalMonth, levels = c(8,9,10))

# vehicle amount predictions
data.vehicle.amount <- data.vehicle[,.(asset_cost, DisbursalMonth, age, employment, ACCTS.CURRENT.BALANCE)]
result.vehicle.amount <- predict(model.vehicle.amount,newdata= data.vehicle.amount)
data.vehicle[, disbursed_amount := result.vehicle.amount]
data.vehicle[, ltv := (result.vehicle.amount / CUSTOMER.vehicle_cost) *100 ]

# vehicle eligibility predictions
prob.vehicle.eligibility <- predict(model.vehicle.eligibility, newdata = data.vehicle, type = 'response')
result.vehicle.eligibility <- ifelse(prob.vehicle.eligibility > 0.5, 0, 1)

# create datatable for home models
data.home = data.table(
          Gender=c("Male"),
           Married=c("Yes"),
           Dependents=c("0"),
           Education=c("Graduate"),
           Self_Employed=c("No"),
           ApplicantIncome=c(5005),
           CoapplicantIncome=c(1960),
           LoanAmount=c(146),
           Loan_Amount_Term=c(343.4),
           Credit_History=c("1"),
           Property_Area=c("Urban"),
           Loan_Status=c("Y"),
           Income=c(6894),
           LoanMonthly = c(13.942))
  
# home predictions
result.home.amount <- predict(model.home.amount, newdata=data.home, type="class")
result.home.eligibility <- predict(model.home.eligibility, newdata=data.home, type="class")
