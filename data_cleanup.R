### Data Cleanup Script
###
### This script should read the train and test sets
## and produce a clean "train" and "test" datasets
## as well as labels and id variables.

## This includes feature engineering, inputing missing
## values and all the usual tricks
## You usually spend a lot of time on this script
## The data needs not to be scaled, we'll do that in
## the next script


# Load train and test datasets
train<-read.csv("train.csv",stringsAsFactors=FALSE)
test<-read.csv("test.csv",stringsAsFactors=FALSE)

# Add label to test set
test$Survived <- 0

# Combine train and test sets
combi <- rbind(train,test)

combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title <- factor(combi$Title)
combi$Sex<-factor(combi$Sex)
combi$Fare2 <- '30+'
combi$Fare2[combi$Fare < 30 & combi$Fare >= 20] <- '20-30'
combi$Fare2[combi$Fare < 20 & combi$Fare >= 10] <- '10-20'
combi$Fare2[combi$Fare < 10] <- '00-10'

combi$Fare2<-factor(combi$Fare2)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)


# Synthetize missing ages
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

# Cleanup missing embarking port
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Fix missing fare
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# Fix Maximum of 32 levels per factor
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

# Now the fun  begins
combi$CabinLetter<-substr(combi$Cabin,1,1)
combi$CabinLetter<-factor(combi$CabinLetter)

combi$CabinLetter<-as.character(combi$CabinLetter)
combi$CabinLetter[combi$CabinLetter==""]<-"U"
combi$CabinLetter<-factor(combi$CabinLetter)

# Create Cabin Number
combi$CabinNum<-gsub('[^0-9]',"",substr(combi$Cabin,1,4))
combi$CabinNum[combi$CabinNum==""]<-0

# Create Cabin Type
combi$Cabintype[(as.numeric(combi$CabinNum) %% 2) == 0] <- "even"
combi$Cabintype[(as.numeric(combi$CabinNum) %% 2) == 1] <- "odd"
combi$Cabintype[(as.numeric(combi$CabinNum) ==0) ] <- "unknown"
combi$Cabintype<-factor(combi$Cabintype)

# Add cabinnum2
combi$Cabinnum2<-as.numeric(combi$CabinNum) %/% 10
combi$Cabinnum2<-factor(combi$Cabinnum2)

# Fix Mrs Anderson
combi$SibSp[69]<-0
combi$Parch[69]<-0
combi$SibSp[1106]<-0
combi$Parch[1106]<-0

combi$HasFamily<-as.numeric(combi$FamilySize)>1
combi$HasFamily<-factor(combi$HasFamily)

combi$multicabin<-nchar(combi$Cabin)>3
combi$multicabin<-factor(combi$multicabin)
combi$hascharge<-grepl("\\(",combi$Name)
combi$hascharge<-factor(combi$hascharge)


rm(famIDs)

combi$Name<-NULL
combi$Ticket<-NULL
combi$Cabin<-NULL
combi$Surname<-NULL
combi$CabinNum<-NULL


combi$PassengerId<-NULL

combi$SibSp<-as.factor(combi$SibSp)
combi$Parch<-as.factor(combi$Parch)
combi$Pclass<-as.factor(combi$Pclass)

# Get Ids
id<-test$PassengerId

# Move label to first column
labels<-as.data.frame(train$Survived)
names(labels)<-"label"
combi$Survived<-NULL
#combi<-cbind(labels,combi)
#names(combi)[1]<-"label"
#combi$label<-as.factor(combi$label)

rm(Agefit)

# Create new prediction
train <- combi[1:891,]
test <- combi[892:1309,]
 
rm(combi)
str(train)
