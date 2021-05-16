# Clear the workspace
rm(list=ls())
cat("\014")

# 1 (a) read in bank.csv into data.frame
setwd
b <- read.csv("bank.csv")
# examine b
str(b)  

# 1 (b) convert to factors
b$job <- as.factor(b$job)
b$marital <- as.factor(b$marital)
b$education <- as.factor(b$education)
b$default <- as.factor(b$default)
b$housing <- as.factor(b$housing)
b$loan <- as.factor(b$loan)
b$contact <- as.factor(b$contact)
b$month <- as.factor(b$month)
b$campaign <- as.factor(b$campaign)
b$poutcome <- as.factor(b$poutcome)
b$y <- as.factor(b$y)

# 1 (c) split into training and test
set.seed(123) # for reproducible results
# write the code to create b.train and b.test below
train <- sample(1:nrow(b),nrow(b)*(2/3))
b.train <- b[train,]
b.test <- b[-train,]


# start with loading the library
library(rpart)
library(rpart.plot)

# 2 (a) build the model using rpart, xval=10, minsplit=50
fit <- rpart(y ~ ., b.train, 
            method="class", control=rpart.control(xval=10, minsplit=50)) 

# 2 (b) plot the tree
rpart.plot(fit, type = 1, extra = 4, main="Classification Tree for Direct Marketing")  

# 2 (c) use b.test to get the confusion matrix 
# put the confusion matrix in variable cm
y.pred <- predict(fit, b.test, type="class")
y.actual <- b.test$y
cm <- table(y.pred, y.actual)
cm

# 2 (d) let us define the confusion matrix in our terms
tn = cm[1,1]
fn = cm[1,2]
fp = cm[2,1]
tp = cm[2,2]

# 2 (e) compute performance measures
# false positive rate (type I error)
fp / (fp + tn)

# false negative rate (type II error)
fn / (fn + tp)

# specificity (true negative rate)
tn / (tn +fp)

# sensitivity (true positive rate, recall)
tp / (tp + fn)

# accuracy
(tp +tn)/(tp + tn +fp +fn)
