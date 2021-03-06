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
CUSTOMER.vehicle_cost <- 100
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
CUSTOMER.gender <- "Female"
CUSTOMER.married <- "Yes"
CUSTOMER.dependents <- "0"
CUSTOMER.education <-"Graduate"
CUSTOMER.income <- 20000
CUSTOMER.co_applicant_income <- 20000
CUSTOMER.loan_term <- 146
CUSTOMER.Property_Area <- "Urban"

# Models
model.vehicle.eligibility <- readRDS("./vehicle/vehicle_eligibility_LOGREG.rds")
model.vehicle.amount <- readRDS("./vehicle/vehicle_amount_CART.rds")
model.home.eligibility <- readRDS("./home/home_eligibility_CART.rds")
model.home.amount <- readRDS("./home/home_amount_CART_continuous.rds")

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
data.vehicle$DisbursalMonth <- factor(data.vehicle$DisbursalMonth, levels = c(1,2,3,4,5,6,7,8,9,10,11,12))

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
          Gender=CUSTOMER.gender,
           Married=CUSTOMER.married,
           Dependents=CUSTOMER.dependents,
           Education=CUSTOMER.education,
           Self_Employed= ifelse(CUSTOMER.employment == "salaried", "No", "Yes"),
           CoapplicantIncome=CUSTOMER.co_applicant_income,
           Loan_Amount_Term=CUSTOMER.loan_term,
           Credit_History= toString(CUSTOMER.credit_history %/% 150),
           Property_Area=CUSTOMER.Property_Area,
           ApplicantIncome=CUSTOMER.income,
          Income = CUSTOMER.income + CUSTOMER.co_applicant_income)

# home predictions
result.home.amount <- predict(model.home.amount, newdata=data.home)
data.home[, LoanMonthly := result.home.amount / (CUSTOMER.loan_term %/% 30) ]
data.home[, LoanAmount := result.home.amount]

prob.home.eligibility <- predict(model.home.eligibility, newdata=data.home, type="class")
result.home.eligibility <- ifelse(prob.home.eligibility == "N", 0, 1)

print(paste(CUSTOMER.name , "Loan Eligibility Results"))
print(paste("=== FOR VEHICLE ==="))
print(paste("Loan Amount:", result.vehicle.amount))
print(paste("Low Default Possibility?:", ifelse(result.vehicle.eligibility == 0, "No", "Yes") ))

print(paste("=== FOR HOME ==="))
print(paste("Loan Amount:", result.home.amount * 1000))
print(paste("Low Default Possibility?:", ifelse(result.home.eligibility== 0, "No", "Yes") ))
