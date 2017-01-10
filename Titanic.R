#Titanic - Pratice exercise from Kaggle


#Set the working directory
setwd("C:/Users/Sanjiv/R Practice")

#Read train and test .csv files
train <- read.csv("C:/Users/Sanjiv/R Practice/train.csv")
View(train)

test <- read.csv("C:/Users/Sanjiv/R Practice/test.csv")
View(test)

#str(train)
#str(test)

prop.table(table(train$Survived))

test$Survived <- rep(0, 418)

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

train$Child <- 0
train$Child[train$Age < 18] <- 1

train$Fare2 <- '30+'
train$Fare2[train$Fare >= 20 & train$Fare <30] <- '20-30'
train$Fare2[train$Fare >= 10 & train$Fare <20] <- '10-20'
train$Fare2[train$Fare <10] <- '<10'

#test$Survived <- 0
#test$survived[test$Fare]
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

fit <- rpart(Survived ~ Pclass + Age +Sex + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)

Prediction <- predict(fit, test, type="class")



submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdectree.csv", row.names = FALSE)




