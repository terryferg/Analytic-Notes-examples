### Function and Package Needed ###
install.packages("pls") # Onlyexecute for the first time
library(pls)
AddMainEffect<-function(x){
  result <- NULL
  for (i in 1:length(x)) {
    if (substr(x[i],nchar(x[i])-1,nchar(x[i])) == ".1"){
      result<- c(result,substr(x[i],1,nchar(x[i])-2))
    }
  }
  return(result)
}

DataCleaning<-function(x){
  temp<-x
  temp2<-NULL
  temp<-subset(temp,N_BREAK>50)
  temp<-temp[,1:ncol(x)-1]
  for (i in 4:ncol(x)) {
    if (max(x[,i])-min(x[,i]) <= 0.05){temp2<-c(temp2,i) }
  }
  temp<-temp[,-temp2]
  names(temp)[1]<-"Lag0"
  names(temp)[2]<-"Lag1"
  names(temp)[3]<-"Lag2"
  return(temp)
}
QuadCreation<-function(x){
  temp<-NULL
  temp <- scale(x[,4:ncol(x)],center=TRUE) 
  temp <- data.frame(temp) 
  temp <- temp^2
  temp <- cbind(x,temp) 
  temp <-data.frame(temp)
  return(temp)
}
AutoVarSelection<-function(mod,data,ncomp,cutoff){
  temp1<-data.frame(mod$coef[,,ncomp])
  temp1<-subset(temp1,abs(temp1)>cutoff)
  temp1<-rownames(temp1)
  Newnames<-AddMainEffect(temp1)
  Newnames<-c(temp1,Newnames)
  Newnames<-unique(Newnames)
  NewData<-data[,colnames(data)%in%Newnames]
  return (NewData)
}
GetcvResult<-function(mod,ncomp,std){
  X<-mod$x
  Y<-mod$y
  cvpred<-matrix(nrow=nrow(X),ncol=1)
  data<-data.frame(Y=Y, X=X)
  for (i in 1:nrow(X)){
    temp<-data[-i,]
    fit<-plsr(Y[-i]~X[-i,],ncomp=ncomp)
    cvpred[i]<-predict(fit,ncomp=ncomp,newdata=t(data.frame(X[i,])))
  }
  cvRMSE<-sqrt(sum((cvpred-Y)^2)/nrow(X))*std
  print("cvRMSE")
  return (cvRMSE) 
}

GetRMSE<-function(mod,ncomp,std){
  Y<-mod$y
  pred<-matrix(nrow=nrow(Y),ncol=1)
  pred<-drop(predict(mod,ncomp=ncomp))
  RMSE<-sqrt(sum((pred-Y)^2)/length(Y))*std
  R2<-(cor(pred,Y))^2
  print(c("RMSE","R Square"))
  return (c(RMSE,R2))
}  

GetLastYearResult<-function(X,NewNames,data){
  temp<-colnames(X)
  Newnames<-c(temp,NewNames)
  Newnames<-unique(Newnames)
  NewData<-data[,colnames(data) %in% Newnames]
  return(NewData)
}


#####################################################################################
### Getting Barefoot Data ###########################################################
#####################################################################################


Barefoot<-SPIDER[1:20,-c(1,2,3,7)]
Barefoot.main<-DataCleaning(Barefoot)
Barefoot.quad<-QuadCreation(Barefoot.main)



### Data Checking ####################
#dim(Barefoot.quad)
#summary(Barefoot.quad) 
#head(Barefoot.quad)
#tail(Barefoot.quad)
#colnames(Barefoot.quad)

### Rescaling the data ####
BarefootData<-scale(Barefoot.quad,center=TRUE)
BarefootData<-data.frame(BarefootData)

###########################################################################################
############################ Lag 0 ########################################################
###########################################################################################

### Build the full model #############
Lag0Data<-BarefootData[,-c(2,3)]
lag0std<-sd(Barefoot.main$Lag0)
mod.lag0 <- plsr(Lag0 ~., data=Lag0Data,ncomp=10,validation="LOO",x=TRUE,y=TRUE)

plot(RMSEP(mod.lag0),legendpos="topright")
NumComp<-3
plot(as.matrix(data.frame(mod.lag0$coef[,,NumComp])))
Cutoff<-0.07

GetcvResult(mod.lag0,NumComp,lag0std)
GetRMSE(mod.lag0,NumComp,lag0std)
mod.lag0$coef[,,NumComp][order(abs(mod.lag0$coef[,,NumComp]))]

### Automatically variable selection
NewX<-AutoVarSelection(mod.lag0,Lag0Data,NumComp,Cutoff)
NewData<-data.frame(NewX,Lag0=Lag0Data$Lag0)

### Adding Last Year Top Driver
LastYearNames<-c("BIR_LIFESTYLE.T3B","SUBST_pct","TRUST_A.brand.I.trust","DP_T3B","TALK_Something.to.talk.about")
NewX<-GetLastYearResult(NewX,LastYearNames,Lag0Data)
NewData<-data.frame(NewX,Lag0=Lag0Data$Lag0)

### Rerun model ######
mod.lag0.1 <- plsr(Lag0 ~ .,data=NewData ,ncomp=10,validation="LOO",x=TRUE,y=TRUE)
plot(RMSEP(mod.lag0.1),legendpos="topright")
NumComp=2
mod.lag0.1 <- update(mod.lag0.1,.~.,ncomp=NumComp) # Update model with component2 

summary(mod.lag0.1)
GetcvResult(mod.lag0.1,NumComp,lag0std)
GetRMSE(mod.lag0.1,NumComp,lag0std)
mod.lag0.1$coef[,,NumComp][order(abs(mod.lag0.1$coef[,,NumComp]))]

           
           # Droping one variable
           mod.lag0.2 <- update(mod.lag0.1,.~.-X75_Will_impress_guests-X75_Will_impress_guests.1,x=TRUE,y=TRUE )
           summary(mod.lag0.2)
           GetRMSE(mod.lag0.2,NumComp,lag0std)
           GetcvResult(mod.lag0.2,NumComp,lag0std)
           mod.lag0.2$coef[,,NumComp][order(abs(mod.lag0.2$coef[,,NumComp]))]

           # Adding Variable
           NewX<-data.frame(NewX,X60_Relaxing=Lag0Data$X60_Relaxing)
           NewData<-data.frame(NewX,Lag0=Lag0Data$Lag0)
           mod.lag0.3 <- plsr(Lag0 ~ .,data=NewData ,ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
           summary(mod.lag0.3)
           GetRMSE(mod.lag0.3,NumComp,lag0std)
           GetcvResult(mod.lag0.3,NumComp,lag0std)
           mod.lag0.3$coef[,,NumComp][order(abs(mod.lag0.3$coef[,,NumComp]))]

           # Automatically dropping variables #
           plot(as.matrix(data.frame(mod.lag0.1$coef[,,NumComp])))
           Cutoff<-0.1 # Choose new cutoff depends on the plot
           NewX<-AutoVarSelection(mod.lag0.1,NewData,NumComp,Cutoff)
           NewData<-data.frame(NewX,Lag0=Lag0Data$Lag0)
           mod.lag0.4 <- plsr(Lag0 ~ .,data=NewData ,ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
           GetRMSE(mod.lag0.4,NumComp,lag0std)
           GetcvResult(mod.lag0.4,NumComp,lag0std)
           mod.lag0.4$coef[,,NumComp][order(abs(mod.lag0.4$coef[,,NumComp]))]

           # Adding Interaction #
           mod.lag0.5 <- plsr(Lag0 ~ (.)^2,data=NewData, ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
           mod.lag0.5$coef[,,NumComp][order(abs(mod.lag0.5$coef[,,NumComp]))]
           # Or 
           mod.lag0.6 <- plsr(Lag0 ~ .+TRUST_A.brand.I.trust*X53_Adventurous,data=NewData, ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
           mod.lag0.6$coef[,,NumComp][order(abs(mod.lag0.6$coef[,,NumComp]))]
           summary(mod.lag0.6)


### Choosing the final model
mod.lag0.final<-mod.lag0.6
Coef<-data.frame(mod.lag0.final$coef[,,NumComp])
Coef

###########################################################################################
######################## Lag 1 ############################################################
###########################################################################################
### Build the full model ##################################################################
Lag1Data<-BarefootData[-20,-c(1,3)]
lag1std<-sd(Barefoot.main$Lag1)
mod.lag1 <- plsr(Lag1 ~., data=Lag1Data,ncomp=10,validation="LOO",x=TRUE,y=TRUE)
plot(RMSEP(mod.lag1),legendpos="topright")
NumComp<-3
plot(as.matrix(data.frame(mod.lag1$coef[,,NumComp])))
Cutoff<-0.07

GetcvResult(mod.lag1,NumComp,lag1std)
GetRMSE(mod.lag1,NumComp,lag1std)
mod.lag1$coef[,,NumComp][order(abs(mod.lag1$coef[,,NumComp]))]

### Automatically variable selection and add main effect back
NewX<-AutoVarSelection(mod.lag1,Lag1Data,NumComp,Cutoff)
NewData<-data.frame(NewX,Lag1=Lag1Data$Lag1)

### Adding Last Year Top Driver
LastYearNames<-c("BIR_LIFESTYLE.T3B","SUBST_pct","TRUST_A.brand.I.trust","DP_T3B","TALK_Something.to.talk.about")
NewX<-GetLastYearResult(NewX,LastYearNames,Lag1Data)
NewData<-data.frame(NewX,Lag1=Lag1Data$Lag1)

### Rerun model ######
mod.lag1.1 <- plsr(Lag1 ~ .,data=NewData ,ncomp=8,validation="LOO",x=TRUE,y=TRUE)# Number of componetns must be smaller than number of variables
plot(RMSEP(mod.lag1.1),legendpos="topright")
NumComp=2
mod.lag1.1 <- update(mod.lag1.1,.~.,ncomp=NumComp) # Update model with component2 

summary(mod.lag1.1)
GetcvResult(mod.lag1.1,NumComp,lag1std)
GetRMSE(mod.lag1.1,NumComp,lag1std)
mod.lag1.1$coef[,,NumComp][order(abs(mod.lag1.1$coef[,,NumComp]))]


    # Droping one variable
    mod.lag1.2 <- update(mod.lag1.1,.~.-TRUST_A.brand.I.trust,x=TRUE,y=TRUE )
    summary(mod.lag1.2)
    GetRMSE(mod.lag1.2,NumComp,lag1std)
    GetcvResult(mod.lag1.2,NumComp,lag1std)
    mod.lag1.2$coef[,,NumComp][order(abs(mod.lag1.2$coef[,,NumComp]))]

    # Adding Variable
    NewX<-data.frame(NewX,X60_Relaxing=Lag1Data$X60_Relaxing)
    NewData<-data.frame(NewX,Lag1=Lag1Data$Lag1)
    mod.lag1.3 <- plsr(Lag1 ~ .,data=NewData ,ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
    summary(mod.lag1.3)
    GetRMSE(mod.lag1.3,NumComp,lag1std)
    GetcvResult(mod.lag1.3,NumComp,lag1std)
    mod.lag1.3$coef[,,NumComp][order(abs(mod.lag1.3$coef[,,NumComp]))]

    # Automatically dropping variables #
    plot(as.matrix(data.frame(mod.lag1.1$coef[,,NumComp])))
    Cutoff<-0.1 # Choose new cutoff depends on the plot
    NewX<-AutoVarSelection(mod.lag1.1,NewData,NumComp,Cutoff)
    NewData<-data.frame(NewX,Lag1=Lag1Data$Lag1)
    mod.lag1.4 <- plsr(Lag1 ~ .,data=NewData ,ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
    GetRMSE(mod.lag1.4,NumComp,lag1std)
    GetcvResult(mod.lag1.4,NumComp,lag1std)
    summary(mod.lag1.4)
    mod.lag1.4$coef[,,NumComp][order(abs(mod.lag1.4$coef[,,NumComp]))]

    # Adding Interaction #
    mod.lag1.5 <- plsr(Lag1 ~ (.)^2,data=NewData, ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
    mod.lag1.5$coef[,,NumComp][order(abs(mod.lag1.5$coef[,,NumComp]))]
    # Or 
    mod.lag1.6 <- plsr(Lag1 ~ .+TRUST_A.brand.I.trust*X53_Adventurous,data=NewData, ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
    mod.lag1.6$coef[,,NumComp][order(abs(mod.lag1.6$coef[,,NumComp]))]
    summary(mod.lag1.6)

### Choosing the final model
mod.lag1.final<-mod.lag1.6
Coef<-data.frame(mod.lag1.final$coef[,,NumComp])
Coef

###########################################################################################
######################### Lag 2 ###########################################################
###########################################################################################

### Build the full model ######################################
Lag2Data<-BarefootData[-c(19,20),-c(1,2)]
lag2std<-sd(Barefoot.main$Lag2)
mod.lag2 <- plsr(Lag2 ~., data=Lag2Data,ncomp=10,validation="LOO",x=TRUE,y=TRUE)
plot(RMSEP(mod.lag2),legendpos="topright")
NumComp<-3
plot(as.matrix(data.frame(mod.lag2$coef[,,NumComp])))
Cutoff<-0.05
GetcvResult(mod.lag2,NumComp,lag2std)
GetRMSE(mod.lag2,NumComp,lag2std)
mod.lag2$coef[,,NumComp][order(abs(mod.lag2$coef[,,NumComp]))]

### Automatically variable selection and add main effect back
NewX<-AutoVarSelection(mod.lag2,Lag2Data,NumComp,Cutoff)
NewData<-data.frame(NewX,Lag2=Lag2Data$Lag2)

### Adding Last Year Top Driver
LastYearNames<-c("BIR_LIFESTYLE.T3B","SUBST_pct","TRUST_A.brand.I.trust","DP_T3B","TALK_Something.to.talk.about")
NewX<-GetLastYearResult(NewX,LastYearNames,Lag2Data)
NewData<-data.frame(NewX,Lag2=Lag2Data$Lag2)

### Rerun model ######
mod.lag2.1 <- plsr(Lag2 ~ .,data=NewData ,ncomp=8,validation="LOO",x=TRUE,y=TRUE)# Number of componetns must be smaller than number of variables
plot(RMSEP(mod.lag2.1),legendpos="topright")
NumComp=2
mod.lag2.1 <- update(mod.lag2.1,.~.,ncomp=NumComp) # Update model with component2 

summary(mod.lag2.1)
GetcvResult(mod.lag2.1,NumComp,lag2std)
GetRMSE(mod.lag2.1,NumComp,lag2std)
mod.lag2.1$coef[,,NumComp][order(abs(mod.lag2.1$coef[,,NumComp]))]


    # Droping one variable
    mod.lag2.2 <- update(mod.lag2.1,.~.-BIR_LIFESTYLE.T3Bt)
    summary(mod.lag2.2)
    GetRMSE(mod.lag2.2,NumComp,lag2std)
    GetcvResult(mod.lag2.2,NumComp,lag2std)
    mod.lag2.2$coef[,,NumComp][order(abs(mod.lag2.2$coef[,,NumComp]))]

    # Adding Variable
    NewX<-data.frame(NewX,X60_Relaxing=Lag1Data$X60_Relaxing)
    NewData<-data.frame(NewX,Lag2=Lag2Data$Lag2)
    mod.lag2.3 <- plsr(Lag2 ~ .,data=NewData ,ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
    summary(mod.lag2.3)
    GetRMSE(mod.lag2.3,NumComp,lag2std)
    GetcvResult(mod.lag2.3,NumComp,lag2std)
    mod.lag2.3$coef[,,NumComp][order(abs(mod.lag2.3$coef[,,NumComp]))]

    # Automatically dropping variables #
    plot(as.matrix(data.frame(mod.lag2.1$coef[,,NumComp])))
    Cutoff<-0.1 # Choose new cutoff depends on the plot
    NewX<-AutoVarSelection(mod.lag2.1,NewData,NumComp,Cutoff)
    NewData<-data.frame(NewX,Lag2=Lag2Data$Lag2)
    mod.lag2.4 <- plsr(Lag2 ~ .,data=NewData ,ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)

    GetRMSE(mod.lag2.4,NumComp,lag2std)
    GetcvResult(mod.lag2.4,NumComp,lag2std)
    summary(mod.lag2.4)
    mod.lag2.4$coef[,,NumComp][order(abs(mod.lag2.4$coef[,,NumComp]))]

    # Adding Interaction #
    mod.lag2.5 <- plsr(Lag2 ~ (.)^2,data=NewData, ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
    mod.lag2.5$coef[,,NumComp][order(abs(mod.lag2.5$coef[,,NumComp]))]
    # Or 
    mod.lag2.6 <- plsr(Lag2 ~ .+TRUST_A.brand.I.trust*X56_Sophisticated,data=NewData, ncomp=NumComp,validation="LOO",x=TRUE,y=TRUE)
    mod.lag2.6$coef[,,NumComp][order(abs(mod.lag2.6$coef[,,NumComp]))]
    summary(mod.lag2.6)


### Choosing the final model
mod.lag2.final<-mod.lag2.6
Coef<-data.frame(mod.lag2.final$coef[,,NumComp])
Coef
