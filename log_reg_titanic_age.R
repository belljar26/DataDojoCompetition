# logistic regression with new variables
log_modelspl <- sample.split(titanic_clean$Survived, SplitRatio = .70)
log_model_train <- subset(titanic_clean, log_modelspl==TRUE)
log_model_test <- subset(titanic_clean, log_modelspl== FALSE)
Log_Model_Titanic  <- glm(Survived ~ Sex + Age + 
                            FamilyNo  +
                            Pclass,
                          data= log_model_train,
                          family = "binomial")
summary(Log_Model_Titanic)
Log_Model_Predict <- predict(Log_Model_Titanic, newdata=log_model_test,
                             type= "response")
table(log_model_test$Survived, Log_Model_Predict >.5)
(143 + 65)/ (143 + 22 + 38 + 65)
# 78 %

library(ROCR)
ROCRpred2 <- prediction(Log_Model_Predict, log_model_test$Survived)
as.numeric(performance(ROCRpred2, "auc")@ y.values)
# AUC = 86%