library(data.table)
library(rpart)
library(rpart.plot)
setwd('~/Documents/re6013bizanalytics')

# Customer Data
FIRST_NAME <- "Jane"


# Models
model.vehicle_eligibility <- readRDS("./home/vehicle_eligibility_LOGREG.rds")
model.vehicle_amount <- readRDS("./vehicle/vehicle_amount_CART.rds")
model.home_eligibility <- readRDS("./home/home_eligibility_CART.rds")
printcp(model.home_eligibility, digits = 3)
