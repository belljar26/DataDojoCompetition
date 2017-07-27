titanic_train <- read_csv("titanic_train.csv")
titanic_test <- read_csv("titanic_test.csv")
View(titanic_train)
# Make new "High Fare" column
titanic_train$HighFare <- ifelse(titanic_train$Fare > 75, "yes", "no")
titanic_test$HighFare <- ifelse(titanic_test$Fare > 75, "yes", "no")
titanic_train$Age[is.na(titanic_train$Age)] <- 32
titanic_test$Age[is.na(titanic_test$Age)] <- 32
titanic_train$Child <- ifelse(titanic_train$Age <14, "yes", "no")
titanic_test$Child <- ifelse(titanic_test$Age < 14, "yes", "no")
Mod1TitanicTrain <- glm(Survived ~ Child + HighFare + Sex,
                       data= titanic_train, family= "binomial" )
summary(Mod1TitanicTrain)
predstitanic <- ifelse(predict(Mod1TitanicTrain, 
                               newdata= titanic_test,
                               type= "response") > .6,
                       1,0)
predstitanic
em_submission <- data.frame(PassengerID = titanic_test$PassengerId,
                            Survived= predstitanic)
View(em_submission)
write.csv(em_submission, "emSubmission.csv",
          row.names = FALSE)


