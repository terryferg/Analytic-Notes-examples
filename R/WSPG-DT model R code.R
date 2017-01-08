#data <- Red.Decision.Tree...Associations
#data <- Red.Decision.Tree...Associations[Red.Decision.Tree...Associations$User==2,]
#data <- Red.Decision.Tree...Associations[Red.Decision.Tree...Associations$PricePoint==1,]
#data <- Red.Decision.Tree...Associations[Red.Decision.Tree...Associations$PricePoint==2,]
#data <- Red.Decision.Tree...Associations[Red.Decision.Tree...Associations$PricePoint==3,]
#data <- Red.Decision.Tree...Associations[Red.Decision.Tree...Associations$PricePoint==4,]
#data <- rbind(Red.Decision.Tree...Associations[Red.Decision.Tree...Associations$PricePoint==1,],Red.Decision.Tree...Associations[Red.Decision.Tree...Associations$PricePoint==2,])
#data <- data[data$PricePoint==3,]


library(randomForest)
library(rpart)

for(i in 2:28){
  data[,i] <- as.factor(as.character(data[,i]))
}



### Tree ############################

tree.fit1=rpart(Red2008Cluster ~.,data[,5:28],control=rpart.control(cp=0.00005),method="class")
printcp(tree.fit1)
tree.fit2<-prune(tree.fit1,cp=0.00005)
tree.pred <- predict(tree.fit2,data[,5:27],type="class")
table(tree.pred,data[,28])
1-sum(diag(table(tree.pred,data[,28])))/dim(data)[1]
plot(tree.fit2)
text(tree.fit2,use.n=FALSE,all=TRUE,cex=1,xpd=TRUE)
as.data.frame(tree.fit1$variable.importance)

### Random Forest ###################
rf.fit <- randomForest(Red2008Cluster ~.,data[,5:28],mtry=5,ntree=1000,importance=TRUE)
pred.rf <- predict(rf.fit,data[,5:28])
table(data[,28] == pred.rf)
importance(rf.fit)
varImpPlot(rf.fit)

