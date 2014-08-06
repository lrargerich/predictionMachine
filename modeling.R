### Modeling tests
###
### INPUT train and test sets, id variable for test set
### train has labels in first column test not
### Output:
### Dataframe with all the predictions for test set in each column
### Dataframe with all the predictions for train set in each column


for (method in c("gbm","rf","svmRadial","C5.0","RRF")) {
    print (paste("Running:",method))
    # Fit training 
    fit<-train(as.factor(t1$label)~.,method=method,data=t1)
    # Save Model
    save(fit,file=paste("model_",method,".model",sep=""))
    pred<-predict(fit,t2)
    
    
    # Compute Accuracy
    acc<- sum((pred == t2$label))/length(t2$label)
    print (paste("Accuracy for ",method,":",acc))
    
    if (acc>max_acc) {
        max_acc <- acc
        best_method <- method
    }
    
    # Aggregate
    pred<-as.data.frame(pred)
    names(pred)<-method
    train_predictions<-cbind(train_predictions,pred)
    # Save partial result
    write.csv(as.data.frame(pred),paste("pred_",method,".csv",sep=""),row.names=FALSE)
    # Save Aggregate
    write.csv(train_predictions,"train_predictions.csv",row.names=FALSE)
    
    # NOW create prediction for test set
    result=predict(fit, test)
    submit <- data.frame(Id = id, Cover_Type = result)
    result<-as.data.frame(result)
    names(result)<-method
    test_predictions<-cbind(test_predictions,result)
    write.csv(submit, file = paste("submit_",method,".csv",sep=""), row.names = FALSE)
    write.csv(test_predictions,"test_predictions.csv",row.names=FALSE)
}

print(paste("Best method was:",best_method,"with accuracy:",max_acc))

fit<- train(as.factor(train_predictions$label)~.,method="gam",data=train_predictions)
pred<-predict(fit,t2)
acc<- sum((pred == t2$label))/length(t2$label)
print (paste("Accuracy for ensamble with gam:",acc))

fit<- train(as.factor(train_predictions$label)~.,method="rf",data=train_predictions)
pred<-predict(fit,t2)
acc<- sum((pred == t2$label))/length(t2$label)
print (paste("Accuracy for ensamble with rf:",acc))

fit<- train(as.factor(train_predictions$label)~.,method="LogitBoost",data=train_predictions)
pred<-predict(fit,t2)
acc<- sum((pred == t2$label))/length(t2$label)
print (paste("Accuracy for ensamble with LogitBoost:",acc))