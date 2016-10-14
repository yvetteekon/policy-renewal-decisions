# Load data
data <- read.csv("Raw_Data.csv", sep = ',', header = TRUE)

# Look at structure of data
str(data)
summary(data)
data.frame(sort(sapply(data, class), decreasing = T))
data$policyeffyr <- as.factor(data$policyeffyr)
