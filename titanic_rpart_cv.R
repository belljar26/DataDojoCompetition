# Rpart with CV on titanic data set
library(caTools)

titanic.cv.split <- sample.split(titanic_rf$Survived, SplitRatio = .70)
titanic.cv.train <- subset(titanic_rf, titanic.cv.split == TRUE)
titanic.cv.test <- subset(titanic_rf, titanic.cv.split== FALSE)
titanic.cv <- rpart(Survived ~ ., data= titanic.cv.train, 
                    method= "class", minbucket= 25)
prp(titanic.cv)
titanic.cv.predict <- predict(titanic.cv, titanic.cv.test,
                              type= "class")
table(titanic.cv.predict, titanic.cv.test$Survived)
#accuracy 76%
(145 + 59)/ (145 + 59 + 44 + 20)
# Precision 87
145/(145 + 20)
#Recall/Sensitivity 77
145/(145 + 44)
# use cross validation to set the minbucket
numFolds <- trainControl(method= "cv", number=10)
CpGridTitanic <- expand.grid(.cp= seq(.01, .05, .01))
train(Survived ~., data= titanic.cv.train,
      method= "rpart", trControl= numFolds, tuneGrid= CpGridTitanic)
Titanic.Tree.Cv <- rpart(Survived ~ ., 
                         data= titanic.cv.train, method= "class",
                         cp= .03)
predict.titanic.tree.cv <- predict(Titanic.Tree.Cv, titanic.cv.test,
                                   type= "class")

table(predict.titanic.tree.cv, titanic.cv.test$Survived)

# accuracy with cv 76% -- no difference with CV
(143 + 60)/ (143 + 43 + 22 + 60)


                         