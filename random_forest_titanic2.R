titanic_rf <- read.csv("titanic.csv")
View(titanic_rf)
summary(titanic_rf)
# remove unneeded columns (cabin, ID, name, etc)
titanic_rf <- titanic_rf[,-c(1,4,9,11,13)]
View(titanic_rf)
titanic_rf$Survived <- as.factor(titanic_rf$Survived)
library(caTools)
titanic_rfsplit <- sample.split(titanic_rf$Survived, SplitRatio = .7)
titanic_rf_train <- subset(titanic_rf, titanic_rfsplit== TRUE)
titanic_rf_test <- subset(titanic_rf, titanic_rfsplit == FALSE)
## random forest model
titanic.rf.model <- randomForest(Survived ~., data= titanic_rf_train,
                                 importance= TRUE, 
                                 ntree= 200)
titanic.rf.model
## predictions
titanic.rf.pred <- predict(titanic.rf.model, newdata= titanic_rf_test,
                           type= "response")
table(titanic.rf.pred, titanic_rf_test$Survived)
## confusion matrix
(149 + 82)/ (149 + 21 + 16 + 82)
# 86 accuracy

importance(titanic.rf.model)
varImpPlot(titanic.rf.model)
## sex and Plcass most important; sex, fare, age decrease Gini
titanic.rf.predictions.prob <- predict(titanic.rf.model, titanic_rf_test,
                                       type="prob")
titanic.rf.predictions.prob


