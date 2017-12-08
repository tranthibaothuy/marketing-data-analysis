# ------------------------
# Import dataset
# ------------------------
datasubset <- read.csv("datasubset.csv")
datasubset <- datasubset[,2:10]

# ------------------------
# Run Libraries
# ------------------------
library(boot) #
library(lattice)  # multivariate data visualization
library(vcd)  # data visualization for categorical variables
library(ROCR)  # evaluation of binary classifiers
library(gains)

# ------------------------
# Select, Prepare Data
# ------------------------

# examine the structure of the bank data frame
print(str(datasubset))

# look at the first few rows of the bank data frame
print(head(datasubset))

# look at the list of column names for the variables
print(names(datasubset))

# summarize the data grouping by variable "y" 
by(datasubset, datasubset$y, summary)

# statitics such as the mean...
sapply(datasubset, mean)
print(margin.table(datasubset$y))

# examine the frequency tables for categorical/factor variables
# showing the number of observations with missing data (if any)
print(table(datasubset$job, useNA = c("always")))
print(table(datasubset$marital, useNA = c("always")))
print(table(datasubset$education, useNA = c("always")))
print(table(datasubset$default, useNA = c("always")))
print(table(datasubset$housing, useNA = c("always")))
print(table(datasubset$loan, useNA = c("always")))

# Calculate the frequency and contingency of categorical variables
my_contigency_table <- xtabs(~y+jobtype+marital+education+default+housing+loan, data=datasubset)
ftable(addmargins(prop.table(my_contigency_table, c(1,2)),3))*100


# Encode Caterogical data
datasubset$response <- factor(datasubset$y, 
                              levels =c("No", "Yes"),
                              labels = c(0,1))

# ------------------------
# Select, Transform Variables
# ------------------------

# Chi-square test of Category Variable to a two-way table
mytable <- xtabs(~response+jobtype, data=datasubset)
chisq.test(mytable)

mytable <- xtabs(~response+marital, data=datasubset)
chisq.test(mytable)

mytable <- xtabs(~response+education, data=datasubset)
chisq.test(mytable)

mytable <- xtabs(~response+default, data=datasubset)
chisq.test(mytable)

mytable <- xtabs(~response+housing, data=datasubset)
chisq.test(mytable)

mytable <- xtabs(~response+loan, data=datasubset)
chisq.test(mytable)

# ------------------------
# Process Model
# ------------------------
# MODEL 1
# specify predictive model
bank_spec1 <- {response ~ age + jobtype + marital + education + default + 
    balance + housing + loan}

# fit logistic regression model
Classification1 <- glm(bank_spec1, family="binomial", data=datasubset)
summary(Classification1)

#-------------------------
# MODEL 2
# specify predictive model
bank_spec2 <- {response ~ jobtype + marital + housing + loan}

# fit logistic regression model
Classification2 <- glm(bank_spec2, family="binomial", data=datasubset)
summary(Classification2)

# ------------------------
# Validate Model
# ------------------------
# Cross Validation with 10 folders for Generalized Linear Model
# The first argument to cost should correspond to the observed responses and 
# the second argument should correspond to the predicted or fitted responses 
# Cost must return a non-negative scalar value. 
cv.err.10 <- cv.glm(datasubset, Classification2, K = 10)$delta
print(cv.err.10)

## Prediction
datasubset$Predict_Prob_Response <- predict(Classification2, datasubset,type = "response") 
prediction <- prediction(datasubset$Predict_Prob_Response,datasubset$response)
performance(prediction,"auc")@y.values
coef <- coef(Classification2)
#---------------------
## ROCR 
ROCR_Curve <- performance(prediction,measure = "tpr", x.measure = "fpr")
Perfo <- performance(prediction,"lift","rpp")

## PLOT ROC CURVE
par(bg ="white", fg = "gray70")
plot(ROCR_Curve, col="mediumseagreen",lwd=2, lty = 1, pch= 6)
abline(0,1, lwd = 1, col = "gray70")
legend("bottomright",legend = "model1",
       col="mediumseagreen", text.col = "mediumseagreen",
       lwd =4, lty=1)
grid(col="lightgray")

## PLOT LIFT CHART
par(bg ="white", fg = "gray70")
plot(Perfo, col="mediumseagreen",lwd=2, lty = 1, pch= 6)
abline(1,0,col="grey")
legend("topright",legend = "model1", col= "mediumseagreen", 
       text.col = "forestgreen", lwd =4,lty=1)
grid(col="lightgray")

## Confuse Matrix
## (50 percent cut-off)
datasubset$Predict_Response <-
  ifelse((datasubset$Predict_Prob_Response > 0.5), 2, 1)
datasubset$Predict_Response <- factor(datasubset$Predict_Response,
                                      levels = c(1, 2), labels = c("No", "Yes"))
confusion_matrix <- table(datasubset$Predict_Response, datasubset$y)
cat("\nConfusion Matrix (rows=Predicted Response, columns=Actual Choice\n")


predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
  sum(confusion_matrix)
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, digits = 1))

addmargins(confusion_matrix)

## (10 percent cut-off)
datasubset$Predict_Response <-
  ifelse((datasubset$Predict_Prob_Response > 0.1), 2, 1)
datasubset$Predict_Response <- factor(datasubset$Predict_Response,
                                      levels = c(1, 2), labels = c("No", "Yes"))
confusion_matrix <- table(datasubset$Predict_Response, datasubset$y)
cat("\nConfusion Matrix (rows=Predicted Response, columns=Actual Choice\n")


predictive_accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2])/
  sum(confusion_matrix)
cat("\nPercent Accuracy: ", round(predictive_accuracy * 100, digits = 1))

addmargins(confusion_matrix)


# ------------------------
# Implement Model
# ------------------------

# Gain Table
datasubset$response <- as.numeric(datasubset$response)
datasubset$Predict_Response_number<- as.numeric(datasubset$Predict_Response)
gains.table <- gains(actual=datasubset$response, predicted=datasubset$Predict_Response_number, groups=10, percents = T)
print(gains.table)
plot.gains(gains.table)

# direct calculation of lift
baseline_response_rate <-
  as.numeric(table(datasubset$y)[2])/nrow(datasubset)
prediction_deciles <- quantile(datasubset$Predict_Prob_Response,
                               probs = seq(0, 1, 0.10), na.rm = FALSE)


# reverse the deciles from highest to lowest
reordered_probability_deciles <- rev(as.numeric(prediction_deciles))
lift_values <- reordered_probability_deciles / baseline_response_rate
cat("\nLift Chart Values by Decile:", lift_values, "\n")
print(lift_values)
