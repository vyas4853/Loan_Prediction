library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(plyr)


setwd("G:\\R Language\\R project\\Loan prediction")
train_data<- read.csv("train_data.csv")
test_data<- read.csv("test_data.csv")
str(train_data)
View(train_data)

boxplot(train_data$ApplicantIncome)
quantile(train_data$ApplicantIncome, c(0.5, 0.9, 0.93, 0.95, 0.97, 0.98, 0.99))
train_data<- train_data[train_data$ApplicantIncome<9000, ]
boxplot(train_data$ApplicantIncome)

boxplot(train_data$CoapplicantIncome)
quantile(train_data$CoapplicantIncome, c(0.5, 0.9, 0.93, 0.95, 0.97, 0.98, 0.99))
train_data<- train_data[train_data$CoapplicantIncome<6000, ]
boxplot(train_data$CoapplicantIncome)

boxplot(train_data$LoanAmount)
quantile(train_data$LoanAmount, c(0.5, 0.9, 0.93, 0.95, 0.97, 0.98, 0.99), na.rm = TRUE)
train_data<- train_data[train_data$LoanAmount<220, ]
boxplot(train_data$LoanAmount)
train_data<- train_data[train_data$LoanAmount>20, ]

sapply(train_data, function(x) sum(is.na(x)))
train_data<- na.omit(train_data)
nrow(train_data)
View(train_data)
str(train_data)


table(train_data$Gender)
train_data$Gender<- as.character(train_data$Gender)
train_data$Gender<- ifelse(train_data$Gender=="", "Male", train_data$Gender)
train_data$Gender<- as.factor(train_data$Gender)


table(train_data$Married)
train_data$Married<- as.character(train_data$Married)
train_data$Married<- ifelse(train_data$Married=="", "Yes", train_data$Married)
train_data$Married<- as.factor(train_data$Married)

str(train_data$Dependents)
table(train_data$Dependents)
train_data$Dependents<- as.character(train_data$Dependents)
train_data$Dependents<- ifelse(train_data$Dependents=="", "0", train_data$Dependents)
train_data$Dependents<- ifelse(train_data$Dependents=="3+", "3", train_data$Dependents)
train_data$Dependents<- as.factor(train_data$Dependents)
table(train_data$Dependents)
train_data$ApplicantIncome<- as.numeric(train_data$ApplicantIncome)


table(train_data$Self_Employed)
train_data$Self_Employed<- as.character(train_data$Self_Employed)
train_data$Self_Employed<- ifelse(train_data$Self_Employed=="", "No", train_data$Self_Employed)
train_data$Self_Employed<- as.factor(train_data$Self_Employed)

train_data$Credit_History<- as.factor(train_data$Credit_History)

str(train_data)

set.seed(123)
x<- sample(1:nrow(train_data), nrow(train_data)/5)
training_data<- train_data[-x, ]
testing_data <- train_data[x, ]

training_data$CoapplicantIncome<- scale(training_data$CoapplicantIncome)
training_data$ApplicantIncome<- scale(training_data$ApplicantIncome)
training_data$LoanAmount<- scale(training_data$LoanAmount)
training_data$Loan_Amount_Term<- scale(training_data$Loan_Amount_Term)

testing_data$CoapplicantIncome<- scale(testing_data$CoapplicantIncome)
testing_data$ApplicantIncome<- scale(testing_data$ApplicantIncome)
testing_data$LoanAmount<- scale(testing_data$LoanAmount)
testing_data$Loan_Amount_Term<- scale(testing_data$Loan_Amount_Term)
#######################################cleaning Test_data############################
table(test_data$Gender)
test_data$Gender<- as.character(test_data$Gender)
test_data$Gender<- ifelse(test_data$Gender=="", "Male", test_data$Gender)
test_data$Gender<- as.factor(test_data$Gender)

table(test_data$Married)
test_data$Married<- as.character(test_data$Married)
test_data$Married<- ifelse(test_data$Married=="", "Yes", test_data$Married)
test_data$Married<- as.factor(test_data$Married)

str(test_data$Dependents)
table(test_data$Dependents)
test_data$Dependents<- as.character(test_data$Dependents)
test_data$Dependents<- ifelse(test_data$Dependents=="", "0", test_data$Dependents)
test_data$Dependents<- ifelse(test_data$Dependents=="3+", "3", test_data$Dependents)
test_data$Dependents<- as.factor(test_data$Dependents)
table(test_data$Dependents)

test_data$ApplicantIncome<- as.numeric(test_dataa$ApplicantIncome)


table(test_data$Self_Employed)
test_data$Self_Employed<- as.character(test_data$Self_Employed)
test_data$Self_Employed<- ifelse(test_data$Self_Employed=="", "No", test_data$Self_Employed)
test_data$Self_Employed<- as.factor(test_data$Self_Employed)

sapply(test_data, function(x) sum(is.na(x)))
table(test_data$LoanAmount)
test_data$LoanAmount<- ifelse(is.na(test_data$LoanAmount),ave(test_data$LoanAmount, FUN= function(x) mean(x, na.rm = TRUE)), test_data$LoanAmount)

test_data$Loan_Amount_Term<- ifelse(is.na(test_data$Loan_Amount_Term),ave(test_data$Loan_Amount_Term, FUN= function(x) mean(x, na.rm = TRUE)), test_data$Loan_Amount_Term)
test_data$Credit_History<- as.factor(test_data$Credit_History)
str(test_data)
nrow(test_data)

table(test_data$Credit_History)
test_data$Credit_History<- as.character(test_data$Credit_History)
test_data$Credit_History<- ifelse(is.na(test_data$Credit_History), "1", test_data$Credit_History)
test_data$Credit_History<- as.factor(test_data$Credit_History)

test_data$CoapplicantIncome<- scale(test_data$CoapplicantIncome)
test_data$ApplicantIncome<- scale(test_data$ApplicantIncome)
test_data$LoanAmount<- scale(test_data$LoanAmount)
test_data$Loan_Amount_Term<- scale(test_data$Loan_Amount_Term)
str(test_data)

test_data<- test_data[c(2:12)]
str(test_data)

#############################################################################
library(randomForest)
set.seed(123)
rf<- randomForest( x= training_data[c(7, 9, 11)], y=training_data$Loan_Status, ntree=300, mtry=1 ,importance = TRUE, proximity = TRUE)

pred2<- predict(rf, testing_data)


cm4<- table(testing_data$Loan_Status, pred2)

cm4 



tuneRF(training_data[c(7, 9, 11)], training_data[ ,13], stepFactor = 1, plot=TRUE, ntreeTry = 300, trace = TRUE, improve = 0.05)
#####################################################

test_data$Loan_Status<- predict(rf, test_data)
View(test_data)
write.csv(test_data,"G:\\R Language\\R project\\Loan prediction\\rfprediction.csv")