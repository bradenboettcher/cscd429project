library(ggplot2)

data <- read.csv('train.csv', stringsAsFactors = F)
##test <- read.csv('test.csv', stringsAsFactors = F)

data$FamilySize <- data$SibSp + data$Parch + 1

data$Survived <- as.logical(data$Survived)
levels(data$Survived) <- c("Not Survived", "Survived")

#for(i in c(3, 5, 12)){
#  data[,i] <- as.factor(data[,i])
#}

ggplot(data, aes(x=Age, y=Pclass, color=Survived)) +
  geom_jitter(position = position_jitter(height = .1)) +
  scale_color_manual(values = c("red", "blue")) +
  facet_grid(Sex ~ .) +
  ggtitle("Age, Sex, and Class as Survival Factors") + ylab("Pclass")



cabins <- data$Cabin
n_occur <- data.frame(table(Var1=cabins))
n_occur <- subset(n_occur, nchar(as.character(Var1)) > 1)

sharedCabins <- n_occur$Var1[n_occur$Freq > 1]
data$FamilySizeAdj <- data$FamilySize
print(table(data$FamilySize))

sharedInd <- data$FamilySizeAdj == 1 & data$Cabin %in% sharedCabins
data$FamilySizeAdj[sharedInd] <- 2  ## if cabin has 2 ppl, then it's shared 
rowCount <- sum(sharedInd)
print(c("adjusted rows", rowCount))
print(table(data$FamilySizeAdj))


library(caret)
set.seed(820)
inTrainingSet <- createDataPartition(data$Survived, p = 0.5, list=FALSE)
train <- data[inTrainingSet,]
test <- data[-inTrainingSet,]

modelAccuracy <-function(test, rpred){
  result_1 <- test$Survived == rpred
  sum(result_1) / length(rpred)
}

checkAccuracy <- function(accuracy){
  if(accuracy > bestAccuracy)
  {
    bestAccuracy <- accuracy
    assign("bestaccuracy", accuracy, envir = .GlobalEnv)
    label <- 'better'
  } 
  else if (accuracy < bestAccuracy)
  {
    label <- 'worse'
  } 
  else 
  {
    label <- 'no change'
  }
  label
  
}

library(rpart)

#prediction including Age and Sex
fol <- formula(Survived ~ Age + Sex)
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelAccuracy(test, rpred)
bestAccuracy <- accuracy
print(c("acc1", accuracy))

#prediction with Age, Sex, Pclass
fol <- formula(Survived ~ Age + Sex + Pclass)
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelAccuracy(test, rpred)
accuracyLabel <- checkAccuracy(accuracy)
print(c("acc2", accuracy, accuracyLabel))

#Prediction with Age, Sex, Fare
fol <- formula(Survived ~ Age + Sex + Fare)
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelAccuracy(test, rpred)
accuracyLabel <- checkAccuracy(accuracy)
print(c("acc3", accuracy, accuracyLabel))

#prediction with Age, Sex, Pclass, Fare
fol <- formula(Survived ~ Age + Sex + Pclass + Fare)
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelAccuracy(test, rpred)
accuracyLabel <- checkAccuracy(accuracy)
print(c("acc4", accuracy, accuracyLabel))

#prediction with Sex, Pclass
fol <- formula(Survived ~ Sex + Age + FamilySize + Pclass)
rmodel <- rpart(fol, method="class", data=train)
rpred <- predict(rmodel, newdata=test, type="class")
accuracy <- modelAccuracy(test, rpred)
accuracyLabel <- checkAccuracy(accuracy)
print(c("acc5", accuracy, accuracyLabel))

print(rpred)

data$Title <- gsub('(.*, )|(\\..*)', '', data$Name)

table(data$Sex, data$Title)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

data$Title[data$Title == 'Mlle']        <- 'Miss' 
data$Title[data$Title == 'Ms']          <- 'Miss'
data$Title[data$Title == 'Mme']         <- 'Mrs' 
data$Title[data$Title %in% rare_title]  <- 'Rare Title'


library(party)
tree <- ctree(fol, data = train) 
