## titanic Random forest with interpolated ages (with regression) and fare
titanic_clean <- read_csv("titanic_clean.csv")
View(titanic_clean)
summary(titanic_clean)
titanic_clean[is.na(titanic_clean$Embarked),]
titanic_clean[is.na(titanic_clean$Embarked)] <- "S"
titanic_RF_Clean <- titanic_clean[, -c(1,8,9,10)]
titanic_RF_Clean$Sex <- as.factor(titanic_RF_Clean$Sex)
titanic_RF_Clean$Age <- as.numeric(titanic_RF_Clean$Age)
titanic_RF_Clean$SibSp <-as.factor(titanic_RF_Clean$SibSp)
titanic_RF_Clean$Parch <-as.factor(titanic_RF_Clean$Parch)
titanic_RF_Clean$FamilyNo <- as.factor(titanic_RF_Clean$FamilyNo)
titanic_RF_Clean$Fare.PP <- as.numeric(titanic_RF_Clean$Fare.PP)
titanic_RF_Clean$Pclass <- as.factor(titanic_RF_Clean$Pclass)


View(titanic_RF_Clean)
str(titanic_RF_Clean)
titanic_RF_Clean$Survived <- as.factor(titanic_RF_Clean$Survived)
library(caTools)
RF_split <- sample.split(titanic_RF_Clean$Survived, SplitRatio = .70)
RF_train <-subset(titanic_RF_Clean, RF_split== TRUE)
RF_test <- subset(titanic_RF_Clean, RF_split == FALSE)
library(randomForest)

RF_Model2 <- randomForest(Survived ~., data= RF_train,
                          importance = TRUE,
                          ntree= 200)
RF_Model2
RF_Model2_Predict <- predict(RF_Model2, newdata= RF_test,
                             type= "response")
table(RF_Model2_Predict, RF_test$Survived)
(149 + 73)/ (149 + 73 + 30 + 16)
## 83 % accuracy
importance(RF_Model2)
varImpPlot(RF_Model2)
