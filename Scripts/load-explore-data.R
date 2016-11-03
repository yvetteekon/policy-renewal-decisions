# Initialize libraries
library(data.table)
library(ggplot2)
library(gridExtra)
library(cowplot)

# Load data
data <- read.csv("Raw_Data.csv", sep = ',', header = TRUE)

# Look at data structure
str(data)
summary(data)

# Encode some variables as different data types
data.frame(sort(sapply(data, class), decreasing = T))
data$policyeffyr <- as.factor(data$policyeffyr)
data$loss.ratio <- as.numeric(sub("%", "", data$loss.ratio))

# Rename some variables
names(data)[names(data) == "action.selected"] <- "action"
names(data)[names(data) == "Policyid"] <- "policy_id"
names(data)[names(data) == "loss.ratio"] <- "loss_ratio"
names(data)[names(data) == "sales.manager"] <- "sales_manager"
names(data)[names(data) == "policyeffyr"] <- "policy_effective_year"
names(data)[names(data) == "reason1"] <- "loss_ratio_category"
names(data)[names(data) == "reason2"] <- "irpm_status"
names(data)[names(data) == "irpm"] <- "irpm_value"

# Rename blanks of variables reason1 and reason2
sapply(data, function(x) levels(unique(x)))
levels(data$loss_ratio_category)[levels(data$loss_ratio_category) == ""] <-
  "loss ratio < 40%"
levels(data$irpm_status)[levels(data$irpm_status) == ""] <- "has no irpm"

# Reorder levels of variable month
levels(data$month)
data$month = factor(data$month, levels=month.abb)

# Split data into train and test data sets
# 80% of the sample size
smp_size <- floor(0.8* nrow(data))

# Set seed to make my partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind,]
test <- data[-train_ind,]

# Check if the distribution is similar for both train and test data sets
round(prop.table(table(train$action))*100,1)
round(prop.table(table(test$action))*100,1)

# Check target variable
unique(train$action)
round(prop.table(table(train$action))*100,2)

# Split into catergorical and numerical data
# numerical data
num_train <- train[,c(1,5,7,8)]

# categorical data
cat_train <- train[,-c(1,5,7,8)]

# Univariate Visual Analysis
# numerical variable plot function
hist <- function(a){
  ggplot(data = num_train, aes(x = a, y=..density..)) +
    geom_histogram(fill="blue",color="red",alpha = 0.2,bins =100) +
    geom_density()
}

# numerical variable plots
p1 <- hist(num_train$premium)
p2 <- hist(num_train$irpm_value)
p3 <- hist(num_train$loss_ratio)
ph <- plot_grid(p1, p2, p3, labels = c("Premium", "IRPM Value", "Loss Ratio"))
title <- ggdraw() + draw_label("Univariate Visual Analysis: Numerical Variables",
                               fontface = "bold")
plot_grid(title, ph, ncol = 1, rel_heights = c(0.1, 1))

# categorical variables plot function
bar <- function(b){
  ggplot(data = cat_train, aes(x = b)) +
    geom_bar(fill = "steelblue", color = "green", width = 0.8, stat = "count") +
    guides(fill = FALSE)
}

# categorical variable
p4 <- bar(cat_train$policy_effective_year)
p5 <- bar(cat_train$month)
p6 <- bar(cat_train$coverage)
p7 <- bar(cat_train$sales_manager)
p8 <- bar(cat_train$state)
p9 <- bar(cat_train$loss_ratio_category)
p10 <- bar(cat_train$irpm_status)
p11 <- bar(cat_train$action)
p12 <- bar(cat_train$underwriter)
pb <- plot_grid(p4, p5, p6, p7, p8, p9, p10,p11, p12,
                labels = c("Policy Effective Year", "Month", "Coverage",
                           "Sales Manager", "State", "Loss Ratio Category",
                           "IRPM Value", "Action", "Underwriter"),
                label_size = 12, vjust = 1.5)
title <- ggdraw() + draw_label("Univariate Visual Analysis: Categorical Variables",
                               fontface = "bold")
plot_grid(title, pb, ncol = 1, rel_heights = c(0.1, 1) )

# Bivariate Analysis
# correlation
library(corrgram)
corrgram (num_train, order=TRUE, lower.panel=panel.shade,
          upper.panel=panel.pie, text.panel=panel.txt,
          main="Correlation of Numerical Predictors") # very weak correlation

# Stack bar plot function
stacked_bar <- function(i){
  ggplot(cat_train, aes(x= i, fill=action)) +
    geom_bar(color="black", width = 0.8, stat = "count")
}

# Stack bar plots
p14 <- stacked_bar(cat_train$policy_effective_year)
p15 <- stacked_bar(cat_train$month)
p16 <- stacked_bar(cat_train$coverage)
p17 <- stacked_bar(cat_train$sales_manager)
p18 <- stacked_bar(cat_train$state)
p19 <- stacked_bar(cat_train$loss_ratio_category)
p20 <- stacked_bar(cat_train$irpm_status)
ps <- plot_grid(p14, p15, p16, p17, p18, p19, p20, 
                labels = c("Policy Effective Year", "Month", "Coverage",
                           "Sales Manager", "State", "Loss Ratio Category",
                           "IRPM Value", "Action", "Underwriting"), 
                label_size = 12, vjust = 1.5)
title <- ggdraw() + draw_label("Bivariate Visual Analysis: Categorical Variables",
                               fontface = "bold")
plot_grid(title, ps, ncol = 1, rel_heights = c(0.1, 1) )

# Multivariate Visual Analysis
# Scatterplot function
scatter_plot <- function(i, j){
  ggplot(train, aes(x = i, y = j, color = action)) + geom_point()
}

# Scatterplot
with(train, scatter_plot(loss_ratio, irpm_value)) + 
  xlab("Loss Ratio") + ylab("IRPM Value")

# Export data sets
write.csv(train, file = "train.csv", row.names = FALSE)
write.csv(test, file = "test.csv", row.names = FALSE)

# Remove unwanted files
rm(num_train, cat_train)

