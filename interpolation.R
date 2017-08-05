# interpolation of Fare data using regression
View(new_titanic)
new_titanic <- read.csv("titanic_train.csv")
#this is just for practice, so can delete survived column
new_titanic[,"Survived"] <- NULL
test_titanic <- read.csv("test_titanic.csv")
new_titanic <-rbind(new_titanic, test_titanic)
new_titanic[is.na(new_titanic$Fare), "Fare"]
summary(new_titanic)
boxplot(new_titanic$Fare)
#filter out outliers
boxplot.stats(new_titanic$Fare)
upper.whisker <- boxplot.stats(new_titanic$Fare)$stats[5]
outlier.filter <- new_titanic$Fare < upper.whisker
no.out.titanic <-new_titanic[outlier.filter,]

Fare_int <- lm(Fare ~ Sex + Parch + SibSp + Age + Pclass + Embarked,
               data= no.out.titanic)
missing.fare <-new_titanic[is.na(new_titanic$Fare), 
            c("Pclass", "Sex", "Age", "Parch", "SibSp", "Embarked")]

pred.missing.fare <-predict(Fare_int, newdata= missing.fare)
new_titanic$Fare[new_titanic$Fare == NA]<- pred.missing.fare
new_titanic$Fare[is.na(new_titanic$Fare)] <- pred.missing.fare

View(new_titanic)
new_titanic[1044,]

# age --this would be more accurate after extracting the titles and using those as predictor variable
#but this is just for regression practice
new_titanic[is.na(new_titanic$Age), "Age"]
boxplot(new_titanic$Age)
boxplot.stats(new_titanic$Age)
age.u.whisker <- boxplot.stats(new_titanic$Age)$stats[5]
age.outlier.filter <- new_titanic$Age < age.u.whisker
remove.age.outlier <- new_titanic[age.outlier.filter,]
Age.int <- lm(Age~ Sex + Pclass + SibSp + Parch, 
              data= remove.age.outlier)
missing.age <-new_titanic[is.na(new_titanic$Age),
                          c("Sex", "Pclass", "SibSp", "Parch")]
pred.missing.age <- predict(Age.int, newdata= missing.age)
pred.missing.age
new_titanic$Age[is.na(new_titanic$Age)] <- pred.missing.age
