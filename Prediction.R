setwd("C:/Users/USER/Desktop/R project/final")
#Question 1
#Load Libraries
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071) 
library(randomForest)
library(nnet)
#Create both training (80%) and testing (20%) sets by selecting samples randomly.
adult <- read.csv("adult.csv")
names(adult) <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status",
                  "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss", "hours-per-week","native-country", "income")
set.seed(1000)
spl <- sample.split(adult$income, SplitRatio = 0.8)
adultTrain <- subset(adult, spl == TRUE)
adultTest <- subset(adult, spl == FALSE)

#Apply the following methods for classification and calculate success of each method.

#Support Vector Machine
svmModel <- svm(formula = income ~ ., 
                data = adultTrain, 
                type = 'C-classification', 
                kernel = 'linear') 
predSVM <- predict(svmModel, newdata = adultTest)
svmMat <-confusionMatrix(data = predSVM, reference = adultTest$income, mode = "prec_recall")#Accuracy - 0.834

#Decision Tree
adultTree <- rpart(income ~.,data = adultTrain, method = "class")
prp(adultTree)
predTree <- predict(adultTree, newdata = adultTest, type = "class")
treeMat <-confusionMatrix(data = predTree, reference = adultTest$income, mode = "prec_recall")#Accuracy <- 0.8333845

#Random Forest
adultTrain2 <- adultTrain
adultTest2 <- adultTest
names(adultTrain2) <- make.names(names(adultTrain2))#Make names legal
names(adultTest2) <- make.names(names(adultTest2))#Make names legal

adultForest <- randomForest(income ~ .,data = adultTrain2)
predForest<- predict(adultForest, newdata = adultTest2)
forestMat <- confusionMatrix(data = predForest, reference = adultTest2$income, mode = "prec_recall")#Accuracy <- 0.8521

#Neural Networks
#ormalize Data
preproc <- preProcess(adultTrain2) #pre process data
adultNorm <- predict(preproc, adultTrain2)
preproc <- preProcess(adultTest2) #pre process data
adultNormT <- predict(preproc, adultTest2)

adultNet <- nnet(income ~., data = adultNorm, size=2)
predNet <- predict(adultNet, newdata = adultNormT, type="class")
netMat <- confusionMatrix(data = as.factor(predNet), reference = adultNormT$income, mode = "prec_recall")#Accuracy <- 0.8443
classification_methods <- c("Support Vector Machine","Decision Tree","Random Forest","Neural Networks")
Accuracy <- c(0.8469, 0.8474, 0.8649, 0.8438)
success_frame <- as.data.frame(cbind(classification_methods,Accuracy))
ggplot(success_frame,aes(x=classification_methods, y=Accuracy))+geom_bar(stat = "identity")

#---------------------------------------------------------------------------------------------
#Question 2
#load Libraries
library(ggplot2)
library(reshape2)
library(scales)
library(np)
#read data
married <- read.csv("married.csv")
marriedMelted <- melt(married, id.vars = "year", measure.vars = c("married","unmarried"))#melt data for easy plotting

#produce a plot with two panels, containing scattered plots to show the changes on married population and unmarried population.
ggplot(marriedMelted, aes(x= factor(year), y=value)) + geom_point() +  facet_grid(variable ~ .) + scale_y_continuous(labels = comma) + labs(x="year")

#predict number of married and unmarried population for years 2019, 2020 and 2025, using the regression techniques, both linear regression and kernel regression

#Test Years
year <- c(2019,2020,2025)

#Linear Regression Models
marriedModel <- lm(married ~year, data=married)
unmarriedModel <- lm(unmarried ~year, data=married)
testMarried <- as.data.frame(year)
#Predictions
testMarried$married <- predict(marriedModel, newdata = testMarried)
testMarried$unmarried <- predict(unmarriedModel, newdata = testMarried)
testMarried$model<- "linear"

#Kernel Regression Models
testMarried2 <- as.data.frame(year)
marriedModel2 <- npreg(married ~year, data=married,regtype="ll", bwmethod = "cv.aic")
unmarriedModel2 <- npreg(unmarried ~year, data=married,regtype="ll", bwmethod = "cv.aic")
#Predictions
testMarried2$married <- predict(marriedModel2, newdata = testMarried)
testMarried2$unmarried <- predict(unmarriedModel2, newdata = testMarried)
testMarried2$model<- "kernel"

#compare the results by plotting bar charts
testMarriedMelted <- rbind(testMarried, testMarried2)
testMarriedMelted <- melt(testMarriedMelted, id.vars = c("year","model"), measure.vars = c("married","unmarried"))#melt data for easy plotting
ggplot(testMarriedMelted, aes(x= factor(year), y=value)) + geom_bar(stat="identity") +  facet_grid(variable ~ model) + scale_y_continuous(labels = comma) + labs(x="year")+geom_text(aes(label=value))
