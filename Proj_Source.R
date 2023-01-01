#Install packages
install.packages('Amelia')

#load libraries
library(ggplot2)
library(Amelia)
library(tidyverse)
library(ltm)
library(caTools)
library(class)
#load data
data <- read.csv("iphone_purchase_records.csv")

# Summarize data
summary(data)


# Data Visualization

#plot for gender
plot1<- ggplot(data, aes(Gender)) + geom_bar(aes(fill = factor(Gender)))
plot1

#plot for age ranges
plot2<- ggplot(data, aes(Age)) + geom_histogram(fill ='lightblue', color='black', bins = 20, alpha=0.5)
plot2

#plot for salary ranges
plot3<- ggplot(data, aes(Salary)) + geom_histogram(fill ='lightgreen', color='black', bins = 20, alpha=0.5)
plot3

#Data Cleaning
any(is.na(data))
missmap(data, main="Imputation check", col=c("yellow", "black"), legend=FALSE)

#factoring the data
data$Gender[data$Gender == "Male"] <- 0
data$Gender[data$Gender == "Female"] <- 1
data$Gender
data$Gender<- as.numeric(as.character(data$Gender))
data$Gender

#Feature Selection
correlation1<- cor(data$Age,data$Purchase.Iphone)
correlation1

correlation2<- cor(data$Salary, data$Purchase.Iphone)
correlation2

correlation3 <- cor(data$Gender, data$Purchase.Iphone)
correlation3

#Split the data into train and test

set.seed(101)
split = sample.split(data, SplitRatio = 0.75)

final.train = subset(data, split == TRUE)
final.test = subset(data, split == FALSE)

#Model fitting -- LOGISTIC REGRESSION

log.model<- glm(formula = Purchase.Iphone ~ . , family= binomial(link = 'logit'), data = final.train)
summary(log.model)

#Test and evaluate

fitted.probabilities <- predict(log.model, newdata = final.test, type = 'response')
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

evaluation_tab<-table(final.test$Purchase.Iphone, fitted.probabilities > 0.5)
evaluation_tab


#Accuracy
TP<- evaluation_tab[2,2]; TP
TN<- evaluation_tab[1,1]; TN

FP<- evaluation_tab[1,2]; FP
FN<- evaluation_tab[2,1]; FN

accuracy1<- (TP+TN)/(TP+TN+FP+FN); accuracy1
precision1<- TP/(TP+FP); precision1
recall1<- TP/(TP+FN); recall1
F1.score1<- 2* (precision1*recall1)/(precision1+recall1); F1.score1
specificity1<- TN/(TN+FP); specificity1
error.rate1<- (FP+FN)/(TP+TN+FP+FN); error.rate1



#KNN
purchase <- data[,4]

standardized.data <- scale(data[,-4])

test.index <- 1:50
test.data <- standardized.data[test.index,]
test.purchase <- purchase[test.index]


train.data <- standardized.data[-test.index,]
train.purchase <- purchase[-test.index]

predicted.purchase1 <- knn(train.data,test.data,train.purchase,k=1)
head(predicted.purchase1)

#evaluate the model
mean(test.purchase != predicted.purchase1)

#choosing a different k value
predicted.purchase2 <- knn(train.data,test.data,train.purchase,k=2)
mean(test.purchase != predicted.purchase2)

predicted.purchase3 <- knn(train.data,test.data,train.purchase,k=3)
mean(test.purchase != predicted.purchase3)


predicted.purchase5 <- knn(train.data,test.data,train.purchase,k=5)
mean(test.purchase != predicted.purchase5)

predicted.purchase8 <- knn(train.data,test.data,train.purchase,k=8)
mean(test.purchase != predicted.purchase8)

predicted.purchase15 <- knn(train.data,test.data,train.purchase,k=15)
mean(test.purchase != predicted.purchase15)


predicted.purchase = NULL
error.rate = NULL

for(i in 1:20){
  set.seed(101)
  predicted.purchase = knn(train.data, test.data, train.purchase, k=i)
  error.rate[i] = mean(test.purchase != predicted.purchase)
}
print(error.rate)


# At k = 5
tab5<- table(predicted.purchase5,test.purchase)

accuracy<- function(x){
  sum(diag(x)/(sum(rowSums(x))))}

accuracy(tab5)

# At k = 8

tab8<- table(predicted.purchase8,test.purchase)

accuracy<- function(x){
  sum(diag(x)/(sum(rowSums(x))))}

accuracy(tab8)


# At k = 2

tab2<- table(predicted.purchase2,test.purchase)

accuracy<- function(x){
  sum(diag(x)/(sum(rowSums(x))))}

accuracy(tab2)


#k = 15

tab15<- table(predicted.purchase15,test.purchase)

accuracy<- function(x){
  sum(diag(x)/(sum(rowSums(x))))}

accuracy(tab15)

#Elbow method

k.values <- 1:20

error.df <- data.frame(error.rate,k.values)

error.df

ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()+ geom_line(lty="dotted",color='red')

# optimal K = 12

predicted.purchase12 <- knn(train.data,test.data,train.purchase,k=12)
mean(test.purchase != predicted.purchase12)

tab12<- table(predicted.purchase12,test.purchase)

accuracy<- function(x){
  sum(diag(x)/(sum(rowSums(x))))}

accuracy(tab12)


