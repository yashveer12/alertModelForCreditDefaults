rm(list = ls())
install.packages("e1071")
set.seed(2)
path <- "C:/Users/HP/OneDrive/Desktop/Semester-7/MBA786M/Project/Credit.csv"
content <- read.csv(path)
content$Class <- as.factor(content$Class)
# Remove constant columns
content <- content[, sapply(content, function(col) length(unique(col)) > 1)]
library(e1071)

#Trying a SVM with linear Kernel
##tune.out <- tune(svm, Class~., data = content, 
##                kernel = "linear",
##                ranges = list(cost = c(0.01, 0.1, 1, 10, 100)),
##)
##summary(tune.out)
#Reached the maximum number of iteration. Unable to tune
train <- sample(1:nrow(content), nrow(content)*0.7)
test <- content[-train,]
##Trying Radial Kernel Support Vector Machines
tune.out <- tune(svm, Class~., data = content[train,],
                 kernel= "radial",
                 ranges = list(
                   cost = c(0.1,1,10,100,1000),
                   gamma = c(0.5,1,2,3,4)
                 )
)
summary(tune.out)

table(
  true = content[-train, "Class"], 
  pred = predict(
    tune.out$best.model, newdata = content[-train, ]
  )
)

