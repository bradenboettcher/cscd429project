train <- read.csv('train.csv', stringsAsFactors = F, header = T)
test <- read.csv('test.csv', stringsAsFactors = F, header = T)

test$PassengerId <- NULL
train$PassengerId <- NULL
test.survived <- data.frame(Survived = rep("NA", nrow(test)), test[,])

data.combined <- rbind(train, test.survived)

str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Sex <- as.factor(data.combined$Sex)

train$Survived <- as.factor(train$Survived)

train$Sex <- as.factor(train$Sex)



table(data.combined$Survived)

table(data.combined$Pclass)

library(ggplot2)

train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  stat_count(width = .5) + 
  xlab("Pclass") + 
  ylab("Total Count") +
  labs(fill = "Survived")

head(as.character(train$Name))

length(unique(as.character(data.combined$Name)))

dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

data.combined[which(data.combined$Name %in% dup.names),]



library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

extractTitle <- function(name){
  name <- as.character(name)
  
  if(length(grep("Miss.", name)) > 0){
    return ("Miss.")
  }
  if(length(grep("Master.", name)) > 0){
    return ("Master.")
  }
  if(length(grep("Mrs.", name)) > 0){
    return ("Mrs.")
  }
  if(length(grep("Mr.", name)) > 0){
    return ("Mr.")
  }
  else{
    return ("Other")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined))
{
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

data.combined$Title <- as.factor(titles)

titles <- NULL
for(i in 1:nrow(train))
{
  titles <- c(titles, extractTitle(train[i, "Name"]))
}

train$Title <- as.factor(titles)


ggplot(data.combined[1:891, ], aes(x = Title, fill = Survived)) + 
  stat_count(width = .5) + 
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") + 
  xlab("Title") + 
  ylab("Total Count") + 
  labs(fill = "Survived")


library(party)
fol <- formula(Survived ~ Pclass+ Age + Sex )
tree <- ctree(fol, data = train)
plot(tree)

fol <- formula(Survived ~ Sex + Title + Age)
tree <- ctree(fol, data = train)
plot(tree)

fol <- formula(Survived ~ Sex + Title)
tree <- ctree(fol, data = train)
plot(tree)

fol <- formula(Survived ~ Title)
tree <- ctree(fol, data = train)
plot(tree)
