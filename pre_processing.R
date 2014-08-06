### Data PreProcessing
###
### INPUT: clean and tidy train and test 
###
### OUTPUT: cleand, tidy, scaled and feature reduced train and test
###
### Note you have to do the PCA analysis manually

library(caret)

asNumeric <- function(x) as.numeric(as.character(as.numeric(x)))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))

train<-factorsNumeric(train)
test<-factorsNumeric(test)

objp <- preProcess(train,method=c("center","scale"))

train<-predict(objp,train)
test<-predict(objp,test)

rm(objp)
rm(asNumeric)
rm(factorsNumeric)

train<-cbind(labels,train)

# Split train in train and test sets
intTr<-createDataPartition(y=train$label,p=0.7,list=FALSE)
t1<-train[intTr,]
t2<-train[-intTr,]
rm(intTr)

# Create container datasets for all predictions
train_predictions<-as.data.frame(t2$label)
names(train_predictions)<-"label"
test_predictions<-as.data.frame(id)
names(test_predictions)<-"id"

max_acc<-0
best_method="none"
method="rf"
