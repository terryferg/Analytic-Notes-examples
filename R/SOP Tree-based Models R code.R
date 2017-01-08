model.13= data3[,4787:4788] #Q4_29a Q4_29b
model.13=cbind(model.13,data3[,5194:5195]) # Q4_33_1 to Q4_33_2 
model.13=cbind(model.13,data3[,5196:5200]) # Q4_34_1 to Q4_34_5 
model.13=cbind(model.13,data3[,5202:5203]) # Q4_35 Q4_36
model.13=cbind(model.13,data3[,5205:5206]) # Q4_37a to Q4_37b 
model.13=cbind(model.13,data3[,5211:5219]) # Q4_42, Q4_43, Q4_44
model.13=cbind(model.13,data3[,5229:5235]) # Q4_47_1 to Q4_47_7
model.13=cbind(model.13,data3[,5238:5248]) # Q4_48_1 to Q4_48_11 
model.13=cbind(model.13,data3[,5250:5254]) # Q4_50_1 to Q4_50_5 
model.13=cbind(model.13,data3[6434])#TypicalPurchaseOcc

for(i in 10:11){
  model.13[,i] <- as.factor(as.character(model.13[,i]))
}
for(i in 15:22){
  model.13[,i] <- as.factor(as.character(model.13[,i]))
}
model.13[,46] <- as.factor(as.character(model.13[,46]))

#####Decision Tree

install.packages("tree")
library(tree)
fit1 <- tree(TypicalPurchaseOcc~.,data = model.13,control=tree.control(8714,mincut=10,minsize =30,mindev=0.005))
summary(fit1)
plot(fit1)
text(fit1,use.n=TRUE, all=TRUE, cex=.8,xpd=TRUE)
tree.pred <- predict(fit1,model.13,type="class")
table(tree.pred,model.13[,46])

library(rpart)
fit2=rpart(TypicalPurchaseOcc~.,model.13,control=rpart.control(cp=0.0005),method="class")
printcp(fit2)
tree1<-prune(fit2,cp=0.0005)

library(rpart)
fit2=rpart(TypicalPurchaseOcc~.,model.13,control=rpart.control(cp=0.0005),method="class")
printcp(fit2)
tree1<-prune(fit2,cp=0.0005)

tiff("model.13 tree plot.tif",unit="in",width=11,height=6,res=300)
plot(tree1)
text(tree1,use.n=TRUE,all=FALSE,cex=0.2,xpd=TRUE)
dev.off()

tree1.pred <- predict(tree1,model.13,type="class")
table(tree1.pred,model.13[,46])
1-sum(diag(table(tree1.pred,model.13[,46])))/dim(model.13)[1]
as.data.frame(fit2$variable.importance)

### Random Forest
library(randomForest)
rf.tree=randomForest(TypicalPurchaseOcc~.,data=model.13, mtry =7,ntree=500,importance=TRUE)
importance(rf.tree)
varImpPlot(rf.tree)
pred.rf = predict(rf.tree,model.13)
table(model6b$TypicalPurchaseOcc == pred.rf)
# Partial dependency plot
partialPlot(rf.tree,model.13,Q4_50_3,1,col=1,ylim=c(-12,5),main="Partial Dependence on Q4_50_3",xlab="Increase the number of trips I currently make to this store to purchase wine")
partialPlot(rf.tree,model.13,Q4_50_3,2,col=2,add=TRUE)
partialPlot(rf.tree,model.13,Q4_50_3,3,col=3,add=TRUE)
partialPlot(rf.tree,model.13,Q4_50_3,4,col=4,add=TRUE)
partialPlot(rf.tree,model.13,Q4_50_3,5,col=5,add=TRUE)
partialPlot(rf.tree,model.13,Q4_50_3,6,col=6,add=TRUE)
legend(8,-5,c("Class 1","Class 2","Class 3","Class 4","Class 5","Class 6"),lty=1,col=c(1,2,3,4,5,6))

#### Boosting
install.packages("gbm")
library(gbm)
gbm_tree <- gbm(PurchaseOccSeg_6Clus~.,data=model6b,train.fraction=1,interaction.depth=1,shrinkage=0.05,n.trees=1000,bag.fraction=0.5,cv.folds=10,verbose=T)

gbm_treePredict <- predict(gbm_tree,type="response",n.trees=500)
final <- gbm_treePredict[,,1]
prediction <- rep(0,8714)
for(k in 1:8714){
  prediction[k] <-  which.max(final[k,])
}
table(model.13$ TypicalPurchaseOcc == prediction)
summary(gbm_tree)

#### Bagging
bag.tree=randomForest(TypicalPurchaseOcc~.,data=model.13, mtry =45,ntree=500,importance=TRUE)


