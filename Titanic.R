
Train <- read.csv(file = "train_titanic.csv",header = TRUE,stringsAsFactors = FALSE,na.strings=c(""))
Test <- read.csv(file = "test_titanic.csv",header = TRUE,stringsAsFactors = FALSE,na.strings=c(""))
tail(Train)
tail(Test)
str(Train)

Test$Survived <- NA

Test$isTrain <- FALSE 
Train$isTrain <- TRUE

data <- rbind(Train,Test)
str(data)

#table(is.na(data))
table(data$Embarked)

#replaced with the mode
#data[(data$Embarked== ''),"Embarked"] = "S"

data[is.na(data$Embarked),"Embarked"] = "S"



table(is.na(data$Pclass)) #false
table(is.na(data$Name)) #false
table(is.na(data$Sex)) #false
table(is.na(data$SibSp)) #false
table(is.na(data$Parch)) #false
table(is.na(data$Ticket)) #false
table(is.na(data$Fare)) #TRUE
table(is.na(data$Cabin)) #False
table(is.na(data$Embarked)) #True


names(data)
#[1] "PassengerId" "Survived"    "Pclass"      "Name"        "Sex"         "Age"         "SibSp"      
#[8] "Parch"       "Ticket"      "Fare"        "Cabin"       "Embarked"    "isTrain"

#Fare
median(data$Fare,na.rm = TRUE)
(temp<-boxplot(data$Fare))
data[(is.na(data$Fare)),"Fare"]  = median(data$Fare,na.rm = TRUE) 

#age
#check how many are na values
table(is.na(data$Age))

#replace with median age 28

data[(is.na(data$Age)),"Age"] = median(data$Age,na.rm = TRUE)
(temp <- boxplot(data$Age))

#Categorical Casting
table(data$Pclass) #1(323),2(277),3(709)
#table(data$Name) #
table(data$Sex)#F(466),M(843)
table(data$SibSp) #  0(891)   1   2   3   4   5   8 
table(data$Parch) #0(1002)    1    2    3    4    5    6    9
#table(data$Ticket) #false
table(data$Fare) #TRUE
table(data$CabinNo) #False
table(data$Embarked) #C Q S

data$CabinNo <- (substr(data$Cabin,1,1))
data[is.na(data$CabinNo),"CabinNo"] <- "z"
data$CabinNo <- as.factor(data$CabinNo)
data$Pclass <- as.factor(data$Pclass)
data$Sex <- as.factor(data$Sex)
data$SibSp <- as.factor(data$SibSp)
data$Parch <- as.factor(data$Parch)
data$Embarked  <- as.factor(data$Embarked)

str(data)
ship.train  <- data[(data$isTrain ==TRUE),]
ship.test <- data[(data$isTrain ==FALSE),]

ship.train$Survived <-as.factor(ship.train$Survived)

names(ship.train)
col.indi <- paste(names(ship.train)[c(3,5,6,7,8,10,12,14)],collapse = ' + ')
col.dep <- "Survived ~ "
col.str <- paste(col.dep,col.indi)
col.formula <-as.formula(col.str)

#table(tempo)<-is.na((ship.train)[c(3,5,6,7,8,10,12,14)])


#install.packages("randomForest")
#library(randomForest)

ship.model <- randomForest(formula= col.formula,
                           data = ship.train, 
                           ntree = 500, mtry = 8,
                           nodesize = 0.001 * nrow(ship.train))

plot(ship.model)
importance(ship.model)
varImpPlot(ship.model)
summary(ship.model)
Survived <- predict(ship.model,newdata = ship.test)
predSurvived <- predict(ship.model,newdata = ship.train)
t<- table(predictions=predSurvived,actual=ship.train$Survived)
t
temp <- cbind(ship.train,predSurvived)
str(temp)
temp1<-temp[c(1,2,15)]
temp1$match <- (temp1$Survived==temp1$predSurvived)
table(temp1$match)  

PassengerId <- ship.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

#write.csv(output.df,file="kaggle_submission_F1.csv",row.names = FALSE)
#-----------E-N-D---------------#
