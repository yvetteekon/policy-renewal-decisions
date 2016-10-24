# Initialize libraries
library(C50)
library(gmodels)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)

# Load data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Model 1 : Decision Tree (C5.0) Model (Multivariate Decision Tree Model)
# Train the model
model_dtree <- C50::C5.0(train[-13], train$action, rules = TRUE)
summary(model_dtree)

# Test the model
predict_dtree <- predict(model_dtree, test)

# Contingency table
CrossTable(test$action, predict_dtree, prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE, dnn = c('Actual Decision', 'Predicted Decision'))
# Accuracy rate is :
round(((845 + 418 + 826)/2093) * 100,2) # 99.81%

# Clean data
# Combine data
combi <- rbind(train, test)

# Check for Missing values
any(is.na(combi)) # no missing values

# Remove variables loss ratio and policy_id
summary(combi$loss_ratio)
combi <- combi[-c(1, 8)]

# Feature Engineering
# Coverage type
levels(combi$coverage)
combi$coverage_type <- NA
combi$coverage_type[combi$coverage %in% c("auto", "home", "contents")] <- "personal"
combi$coverage_type[combi$coverage %in% c("products", "building")] <- "commercial"

# Season
combi$season <- NA
combi$season[combi$month %in% c("Mar", "Apr", "May")] <- "spring"
combi$season[combi$month %in% c("Jun", "Jul", "Aug")] <- "summer"
combi$season[combi$month %in% c("Sep", "Oct", "Nov")] <- "fall"
combi$season[combi$month %in% c("Dec", "Jan", "Feb")] <- "winter"

# Quarter
combi$quarter <- NA
combi$quarter[combi$month %in% c("Jan", "Feb", "Mar")] <- "first"
combi$quarter[combi$month %in% c("Apr", "May", "Jun")] <- "second"
combi$quarter[combi$month %in% c("Jul", "Aug", "Sep")] <- "third"
combi$quarter[combi$month %in% c("Oct", "Nov", "Dec")] <- "fourth"

# Region
combi$region <- NA
combi$region[combi$state %in% 
               c("FL", "GA", "KY", "MD", "NC", "SC", "TN", "TX", "VA")] <- "south"
combi$region[combi$state %in% c("IL", "MN", "WI")] <- "midwest"
combi$region[combi$state %in% c("NJ", "NY", "PA")] <- "northeast"

# Risk category
bin <- rpart(irpm_status ~ irpm_value, data = combi)
summary(bin)
fancyRpartPlot(bin)

# Encode character variables as factors
data.frame(sort(sapply(combi, class), decreasing = T))
for (i in c(12:15)){
  combi[, i] <- as.factor(combi[, i])
}

# Split data into train and test data sets
# 80% of the sample size
smp_size <- floor(0.8* nrow(combi))

# Set seed to make my partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(combi)), size = smp_size)

train <- combi[train_ind,]
test <- combi[-train_ind,]

# Check if the distribution is similar for both train and test data sets
round(prop.table(table(train$action))*100,1)
round(prop.table(table(test$action))*100,1)

# Model 2 : Random Forest Model
model_rf <- randomForest(action ~ ., data = train, ntree = 501, 
                         importance = TRUE, do.trace = TRUE)
plot(model_rf)

options("digits" = 2)
model_rf$confusion[, "class.error"]

# Overall Accuracy rate
((1 - 0.00802) + (1 - 0.59311) + (1 - 0.20292)) / 3 * 100 # 73%

# Improve Model Predictive Accuracy using Ensemble Methods
# Model 3 : Adaptive Boosting (AdaBoost)
model_boost <- C50::C5.0(train[-11], train$action, trials = 10) # trial = 10 means I want to build 10 decision trees instead of the initial 210 that were built. This will reduce high variance
summary(model_boost)

predict_boost <- predict(model_boost, test)

CrossTable(test$action, predict_boost, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Decision', 'Predicted Decision'))

# Accuracy rate
round(((836 + 200 + 628)/ 2093 * 100), 2) # 79.50

# Penalty and Cost Matrix
contrasts(train$action)
# relevel(train$action, "add credit")

# Create cost matrix
error_cost <- matrix(c(0,40,20,-5,0,10,5,30,0), nrow=3) 
dimnames(error_cost) <- list("Actual" = c("add credit", "add debit", "no action"),
                             "Predicted" = c("add credit", "add debit", "no action"))
error_cost

# Apply the cost matrix to the tree
model_cost <- C50::C5.0(train[-11], train$action, costs = error_cost) 
predit_cost <- predict(model_cost, test)

CrossTable(test$action, predit_cost, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual', 'Predicted'))

((836 + 85 + 760)/2093 * 100)  # 80%.
