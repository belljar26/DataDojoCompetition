#extracting titles
library(regexr)
library(stringr)
titanic_train$title <- str_extract(titanic_train$Name, "[A-Z][a-z]{1,}\\.")
titanic_clean <- titanic_train[, -c(4,9,11)]
View(titanic_clean)
# Make family size category
titanic_clean$FamilyNo <- 1+ titanic_clean$SibSp + titanic_clean$Parch
# Interpolate ages with new title column
# filter out outliers
boxplot(titanic_clean$Age)
boxplot.stats(titanic_clean$Age)
age.u.whisker<- boxplot.stats(titanic_clean$Age)$stats[5]
age.outlier.filter<- titanic_clean$Age < age.u.whisker
remove.age.outlier <-titanic_clean[age.outlier.filter,]
Age.int.2 <- lm(Age ~ Pclass + Sex + title + Parch + SibSp,
                data= remove.age.outlier)
missing.age.2 <- titanic_clean[is.na(titanic_clean$Age),
                               c("Pclass", "Sex", "title", "Parch",
                                 "SibSp")]
pred.missing.age2 <-predict(Age.int.2, newdata = missing.age.2)
pred.missing.age2
titanic_clean$Age[is.na(titanic_clean$Age)] <- pred.missing.age2
View(titanic_clean)
# Create variable Fare per person
titanic_clean$Fare.PP <- titanic_clean$Fare/titanic_clean$FamilyNo
write_csv(titanic_clean, "titanic_clean.csv")
