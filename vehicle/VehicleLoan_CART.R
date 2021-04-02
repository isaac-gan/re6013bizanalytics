library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(MLmetrics)

set.seed(2014)
setwd('/Users/stephen/Desktop/REP Acad/RE6013 Biz Ana/Project WhiteRock/VehicleLoan')
VehicleLoan.dt <- fread('VehicleLoan_cleaned.csv')

# Data cleaning ----------------------------------------------------------
# Predict only on customers that did not default
VehicleLoan.dt <- VehicleLoan.dt[loan_default == 0]
# remove loan_default as we would not know whether the customer is defaulting
# at the point of disbursement
VehicleLoan.dt[, c('loan_default') := NULL]
summary(VehicleLoan.dt)

# Train test split --------------------------------------------------------
training.samples <- VehicleLoan.dt$disbursed_amount %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- VehicleLoan.dt[training.samples,]
test.data <- VehicleLoan.dt[-training.samples,]

# Maximal Tree -----------------------------------------------------------
# Takes pretty long due to number of feature columns
cart1 <-
  rpart(
    disbursed_amount ~ .,
    data = train.data,
    method = 'anova',
    control = rpart.control(minsplit = 20, cp = 0)
  )
printcp(cart1)
plotcp(cart1)
print(cart1)

# Pruning -----------------------------------------------------------
# Find the optimal cp as shown in lectures
CVerror.cap <-
  cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xstd"]
i <- 1
j <- 4
while (cart1$cptable[i, j] > CVerror.cap) {
  i <- i + 1
}
cp1 = ifelse(i > 1, sqrt(cart1$cptable[i, 1] * cart1$cptable[i - 1, 1]), 1)
# Prune
cart2 <- prune(cart1, cp = cp1)
printcp(cart2, digits = 3)

# Visualise -----------------------------------------------------------
print(cart2)
rpart.plot(cart2, nn = T, main = "Optimal Tree in Vehicle Loan")
cart2$variable.importance
summary(cart2)

# Test -----------------------------------------------------------
predicted.amount <- cart2 %>%
  predict(test.data)
RMSE(predicted.amount, test.data$disbursed_amount) # RMSE
R2_Score(predicted.amount, test.data$disbursed_amount) #R2
