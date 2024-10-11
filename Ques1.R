install.packages("pROC")
rm(list = ls())

path <- "C:/Users/HP/OneDrive/Desktop/Semester-7/MBA786M/Project/Credit.csv"
###########################################################
######################### PART-A ##########################
###########################################################
content <- read.csv(path)
content$Class <- as.factor(content$Class)
# Assuming Good as 1 and Bad as 0 because the Bad data is available less and the good data is more and these 2 things are mutually exclusive.

content[, 10] <- ifelse(content[, 10] == "Good", 1, 0)

model <- glm(Class ~ ., data = content, family = binomial)
summary(model)

new_model <- glm(Class ~ Duration + Amount + InstallmentRatePercentage + ForeignWorker + CheckingAccountStatus.lt.0 + CheckingAccountStatus.0.to.200 + CreditHistory.NoCredit.AllPaid
                 + CreditHistory.ThisBank.AllPaid + CreditHistory.PaidDuly + SavingsAccountBonds.lt.100 + OtherDebtorsGuarantors.None + OtherDebtorsGuarantors.CoApplicant
                 + OtherInstallmentPlans.Bank, data = content, family = binomial)
summary(new_model)

predicted_probability <- predict(new_model, type = "response")
predicted_probability[1:10]

class_prediction <- rep("Bad", 1000)
class_prediction[predicted_probability > .5] = "Good"
confusion_matrix <- table(class_prediction, content$Class)
print(confusion_matrix)
TP <- confusion_matrix[1, 1]  # True Positives
TN <- confusion_matrix[2, 2]  # True Negatives
FP <- confusion_matrix[1, 2]  # False Positives
FN <- confusion_matrix[2, 1]  # False Negatives
accuracy <- (TP + TN) / sum(confusion_matrix)
print(paste("Accuracy =", accuracy))
TPR <- TP / (TP + FN)
print(paste("TPR =", TPR))
FNR <- FN/ (FN + TP)
print(paste("FNR =", FNR))
FPR <- FP/ (FP + TN)
print(paste("FPR =", FPR))
TNR <- TN/ (TN + FP)
print(paste("TNR =", TNR))


class_prediction <- rep("Bad", 1000)
class_prediction[predicted_probability > 0.65] = "Good"
confusion_matrix <- table(class_prediction, content$Class)
print(confusion_matrix)
TP <- confusion_matrix[1, 1]  # True Positives
TN <- confusion_matrix[2, 2]  # True Negatives
FP <- confusion_matrix[1, 2]  # False Positives
FN <- confusion_matrix[2, 1]  # False Negatives
accuracy <- (TP + TN) / sum(confusion_matrix)
print(paste("Accuracy =", accuracy))
TPR <- TP / (TP + FN)
print(paste("TPR =", TPR))
FNR <- FN/ (FN + TP)
print(paste("FNR =", FNR))
FPR <- FP/ (FP + TN)
print(paste("FPR =", FPR))
TNR <- TN/ (TN + FP)
print(paste("TNR =", TNR))


class_prediction <- rep("Bad", 1000)
class_prediction[predicted_probability > 0.8] = "Good"
confusion_matrix <- table(class_prediction, content$Class)
print(confusion_matrix)
TP <- confusion_matrix[1, 1]  # True Positives
TN <- confusion_matrix[2, 2]  # True Negatives
FP <- confusion_matrix[1, 2]  # False Positives
FN <- confusion_matrix[2, 1]  # False Negatives
accuracy <- (TP + TN) / sum(confusion_matrix)
print(paste("Accuracy =", accuracy))
TPR <- TP / (TP + FN)
print(paste("TPR =", TPR))
FNR <- FN/ (FN + TP)
print(paste("FNR =", FNR))
FPR <- FP/ (FP + TN)
print(paste("FPR =", FPR))
TNR <- TN/ (TN + FP)
print(paste("TNR =", TNR))
###########################################################
######################PART-B###############################
###########################################################

set.seed(1)
train <- sample(1:nrow(content), nrow(content)*0.7)
new_model <- glm(Class ~ Duration + Amount + InstallmentRatePercentage + ForeignWorker + CheckingAccountStatus.lt.0 + CheckingAccountStatus.0.to.200 + CreditHistory.NoCredit.AllPaid
                 + CreditHistory.ThisBank.AllPaid + CreditHistory.PaidDuly + SavingsAccountBonds.lt.100 + OtherDebtorsGuarantors.None + OtherDebtorsGuarantors.CoApplicant
                 + OtherInstallmentPlans.Bank, content, subset = train,family = binomial)
predictions_prob <- predict(new_model, content[-train, ], type = "response")
## View(predictions_prob)
class_prediction <- rep("Bad", 300)
class_prediction[predictions_prob > .5] = "Good"
confusion_matrix <- table(class_prediction, content[-train, "Class"])
print(confusion_matrix)
TP <- confusion_matrix[1, 1]  # True Positives
TN <- confusion_matrix[2, 2]  # True Negatives
FP <- confusion_matrix[1, 2]  # False Positives
FN <- confusion_matrix[2, 1]  # False Negatives
accuracy <- (TP + TN) / sum(confusion_matrix)
print(paste("Accuracy =", accuracy))
TPR <- TP / (TP + FN)
print(paste("TPR =", TPR))
FNR <- FN/ (FN + TP)
print(paste("FNR =", FNR))
FPR <- FP/ (FP + TN)
print(paste("FPR =", FPR))
TNR <- TN/ (TN + FP)
print(paste("TNR =", TNR))


class_prediction <- rep("Bad", 300)
class_prediction[predictions_prob > 0.65] = "Good"
confusion_matrix <- table(class_prediction, content[-train, "Class"])
print(confusion_matrix)
TP <- confusion_matrix[1, 1]  # True Positives
TN <- confusion_matrix[2, 2]  # True Negatives
FP <- confusion_matrix[1, 2]  # False Positives
FN <- confusion_matrix[2, 1]  # False Negatives
accuracy <- (TP + TN) / sum(confusion_matrix)
print(paste("Accuracy =", accuracy))
TPR <- TP / (TP + FN)
print(paste("TPR =", TPR))
FNR <- FN/ (FN + TP)
print(paste("FNR =", FNR))
FPR <- FP/ (FP + TN)
print(paste("FPR =", FPR))
TNR <- TN/ (TN + FP)
print(paste("TNR =", TNR))


class_prediction <- rep("Bad", 300)
class_prediction[predictions_prob > 0.8] = "Good"
confusion_matrix <- table(class_prediction, content[-train, "Class"])
print(confusion_matrix)
TP <- confusion_matrix[1, 1]  # True Positives
TN <- confusion_matrix[2, 2]  # True Negatives
FP <- confusion_matrix[1, 2]  # False Positives
FN <- confusion_matrix[2, 1]  # False Negatives
accuracy <- (TP + TN) / sum(confusion_matrix)
print(paste("Accuracy =", accuracy))
TPR <- TP / (TP + FN)
print(paste("TPR =", TPR))
FNR <- FN/ (FN + TP)
print(paste("FNR =", FNR))
FPR <- FP/ (FP + TN)
print(paste("FPR =", FPR))
TNR <- TN/ (TN + FP)
print(paste("TNR =", TNR))


###########################################################
######################PART-C###############################
###########################################################
library(pROC)
roc_curve <- roc(content$Class[-train], predictions_prob)

plot(roc_curve, col = "blue", main = "ROC Curve")

auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
