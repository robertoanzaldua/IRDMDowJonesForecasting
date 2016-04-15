library(forecast)
library(randomForest)
library(RSNNS)
data=read.table(file.choose(), header=T, sep=",")
data$Date=as.Date(data$Date, "%Y-%m-%d")
dowdata<-data[order(data$Date),]

#---plot
dowdata$Adj.Close <- msts(dowdata$Adj.Close, seasonal.periods=c(7,365.25), ts.frequency=365.25, start=2009+299/365)
plot(dowdata$Adj.Close,type="l", main="IBM Stock Prices", ylab="Adj.Close Price", col="darkblue")

#---lag1
end=nrow(dowdata)
clase=ifelse(dowdata$Adj.Close[-1]-dowdata$Adj.Close[-end]>0,1,0)
dowdatavar=cbind(dowdata[-1,1],clase,dowdata[-end,c(7,2:6)])
colnames(dowdatavar)<-c('Date','Target','Closeadj_L1','Open_L1','High_L1','Low_L1','Close_L1','Vol_L1')

#---features

dowdatavar$Acc1[1]=1
dowdatavar$PerCloseAdj[1]=dowdatavar$PerOpen[1]=dowdatavar$PerHigh[1]=dowdatavar$PerLow[1]=dowdatavar$PerClose[1]=dowdatavar$PerVol[1]=0
dowdatavar$Maxcloseadj30[1]=dowdatavar$Mincloseadj30[1]=dowdatavar$Maxopen30[1]=dowdatavar$Minopen30[1]=1
dowdatavar$Maxhigh30[1]=dowdatavar$Minhigh30[1]=dowdatavar$Maxlow30[1]=dowdatavar$Minlow30[1]=1
dowdatavar$Maxclose30[1]=dowdatavar$Minclose30[1]=dowdatavar$Maxvol30[1]=dowdatavar$Minvol30[1]=1
dowdatavar$Maxcloseadj90[1]=dowdatavar$Mincloseadj90[1]=dowdatavar$Maxopen90[1]=dowdatavar$Minopen90[1]=1
dowdatavar$Maxhigh90[1]=dowdatavar$Minhigh90[1]=dowdatavar$Maxlow90[1]=dowdatavar$Minlow90[1]=1
dowdatavar$Maxclose90[1]=dowdatavar$Minclose90[1]=dowdatavar$Maxvol90[1]=dowdatavar$Minvol90[1]=1

for(i in 2:nrow(dowdatavar)){
  dowdatavar$Acc1[i]=ifelse(dowdatavar$Closeadj_L1[i]>dowdatavar$Closeadj_L1[i-1],dowdatavar$Acc1[i-1]+1,dowdatavar$Acc1[i-1]-1)
  dowdatavar$PerCloseAdj[i]=round(dowdatavar$Closeadj_L1[i]/dowdatavar$Closeadj_L1[i-1],4)-1
  dowdatavar$PerOpen[i]=round(dowdatavar$Open_L1[i]/dowdatavar$Open_L1[i-1],4)-1
  dowdatavar$PerHigh[i]=round(dowdatavar$High_L1[i]/dowdatavar$High_L1[i-1],4)-1
  dowdatavar$PerLow[i]=round(dowdatavar$Low_L1[i]/dowdatavar$Low_L1[i-1],4)-1
  dowdatavar$PerClose[i]=round(dowdatavar$Close_L1[i]/dowdatavar$Close_L1[i-1],4)-1
  dowdatavar$PerVol[i]=round(dowdatavar$Vol_L1[i]/dowdatavar$Vol_L1[i-1],4)-1
  
  if(i>30){
  ind=(i-1):(i-30)
  }else{  
  ind=(i-1):1}
  newmincloseadj=min(dowdatavar$Closeadj_L1[ind])
  newmaxcloseadj=max(dowdatavar$Closeadj_L1[ind])
  newminopen=min(dowdatavar$Open_L1[ind])
  newmaxopen=max(dowdatavar$Open_L1[ind])
  newminlow=min(dowdatavar$Low_L1[ind])
  newmaxlow=max(dowdatavar$Low_L1[ind])
  newminhigh=min(dowdatavar$High_L1[ind])
  newmaxhigh=max(dowdatavar$High_L1[ind])
  newminclose=min(dowdatavar$Close_L1[ind])
  newmaxclose=max(dowdatavar$Close_L1[ind])
  newminvol=min(dowdatavar$Vol_L1[ind])
  newmaxvol=max(dowdatavar$Vol_L1[ind])
  
  if(i>90){
    ind2=(i-1):(i-90)
  }else{  
    ind2=(i-1):1}
  newmincloseadj2=min(dowdatavar$Closeadj_L1[ind2])
  newmaxcloseadj2=max(dowdatavar$Closeadj_L1[ind2])
  newminopen2=min(dowdatavar$Open_L1[ind2])
  newmaxopen2=max(dowdatavar$Open_L1[ind2])
  newminlow2=min(dowdatavar$Low_L1[ind2])
  newmaxlow2=max(dowdatavar$Low_L1[ind2])
  newminhigh2=min(dowdatavar$High_L1[ind2])
  newmaxhigh2=max(dowdatavar$High_L1[ind2])
  newminclose2=min(dowdatavar$Close_L1[ind2])
  newmaxclose2=max(dowdatavar$Close_L1[ind2])
  newminvol2=min(dowdatavar$Vol_L1[ind2])
  newmaxvol2=max(dowdatavar$Vol_L1[ind2])
  
  dowdatavar$Maxcloseadj30[i]=ifelse(dowdatavar$Closeadj_L1[i]>newmaxcloseadj,1,0)
  dowdatavar$Mincloseadj30[i]=ifelse(dowdatavar$Closeadj_L1[i]<newmincloseadj,1,0)
  dowdatavar$Maxopen30[i]=ifelse(dowdatavar$Open_L1[i]>newmaxopen,1,0)
  dowdatavar$Minopen30[i]=ifelse(dowdatavar$Open_L1[i]<newminopen,1,0)
  dowdatavar$Maxhigh30[i]=ifelse(dowdatavar$High_L1[i]>newmaxhigh,1,0)
  dowdatavar$Minhigh30[i]=ifelse(dowdatavar$High_L1[i]<newminhigh,1,0)
  dowdatavar$Maxlow30[i]=ifelse(dowdatavar$Low_L1[i]>newmaxlow,1,0)
  dowdatavar$Minlow30[i]=ifelse(dowdatavar$Low_L1[i]<newminlow,1,0)
  dowdatavar$Maxclose30[i]=ifelse(dowdatavar$Close_L1[i]>newmaxclose,1,0)
  dowdatavar$Minclose30[i]=ifelse(dowdatavar$Close_L1[i]<newminclose,1,0)
  dowdatavar$Maxvol30[i]=ifelse(dowdatavar$Vol_L1[i]>newmaxvol,1,0)
  dowdatavar$Minvol30[i]=ifelse(dowdatavar$Vol_L1[i]<newminvol,1,0)
  
  dowdatavar$Maxcloseadj90[i]=ifelse(dowdatavar$Closeadj_L1[i]>newmaxcloseadj2,1,0)
  dowdatavar$Mincloseadj90[i]=ifelse(dowdatavar$Closeadj_L1[i]<newmincloseadj2,1,0)
  dowdatavar$Maxopen90[i]=ifelse(dowdatavar$Open_L1[i]>newmaxopen2,1,0)
  dowdatavar$Minopen90[i]=ifelse(dowdatavar$Open_L1[i]<newminopen2,1,0)
  dowdatavar$Maxhigh90[i]=ifelse(dowdatavar$High_L1[i]>newmaxhigh2,1,0)
  dowdatavar$Minhigh90[i]=ifelse(dowdatavar$High_L1[i]<newminhigh2,1,0)
  dowdatavar$Maxlow90[i]=ifelse(dowdatavar$Low_L1[i]>newmaxlow2,1,0)
  dowdatavar$Minlow90[i]=ifelse(dowdatavar$Low_L1[i]<newminlow2,1,0)
  dowdatavar$Maxclose90[i]=ifelse(dowdatavar$Close_L1[i]>newmaxclose2,1,0)
  dowdatavar$Minclose90[i]=ifelse(dowdatavar$Close_L1[i]<newminclose2,1,0)
  dowdatavar$Maxvol90[i]=ifelse(dowdatavar$Vol_L1[i]>newmaxvol2,1,0)
  dowdatavar$Minvol90[i]=ifelse(dowdatavar$Vol_L1[i]<newminvol2,1,0)
}

#--- model

cuts=c("01/01/2014","01/01/2015","01/07/2015","01/10/2015")
precision=recall=f1=acc=matrix(0,length(cuts),2)

for(k in 1:length(cuts)){
print(k)
dowdatasub=dowdatavar[which(dowdatavar$Date>=as.Date(cuts[k], "%d/%m/%Y")),]
#ts <- msts(data1$Closeadj_L1, seasonal.periods=c(7,365.25), ts.frequency=365.25, start=2009+299/365)
plot(dowdatasub$Closeadj_L1,type="l", main="IBM Stock Prices", ylab="Adj.Close Price", col="darkblue")

h=round(nrow(dowdatasub)*0.8)
end=nrow(dowdatasub)
l=1
Pred=c()
Pred2=c()
true=c()

for (j in h:(end-1) ){
  train=dowdatasub[1:h,-1]
  test=dowdatasub[(h+1),-c(1,2)]
  
  
  fit <- randomForest(as.factor(Target) ~ .,   data=train)
  Pred[l] <- as.numeric(predict(fit, test))-1
  //
  trainx <- as.matrix(train[,-1])
  trainy <- as.matrix(train[,1] * 1)
  testx <- as.matrix(test)


  per <- perceptron(trainx, trainy, maxit = 200, learn.rate = 1)
  Pred2[l]<- thresh.bin(testx %*% per$weights + per$bias)
  l=l+1
}

true=dowdatasub[((h+1):end),2]
tp=sum(ifelse(Pred==1 &true==1,1,0))
tn=sum(ifelse(Pred==0 &true==0,1,0))
fp=sum(ifelse(Pred==1 &true==0,1,0))
fn=sum(ifelse(Pred==0 &true==1,1,0))

precision[k,1]=round(tp/(tp+fp),4)
recall[k,1]=round(tp/(tp+fn),4)
f1[k,1]=round((2*tp)/(2*tp+fp+fn),4)
acc[k,1]=round((tp+tn)/(tp+tn+fp+fn),4)

tp=sum(ifelse(Pred2==1 &true==1,1,0))
tn=sum(ifelse(Pred2==0 &true==0,1,0))
fp=sum(ifelse(Pred2==1 &true==0,1,0))
fn=sum(ifelse(Pred2==0 &true==1,1,0))

precision[k,2]=round(tp/(tp+fp),4)
recall[k,2]=round(tp/(tp+fn),4)
f1[k,2]=round((2*tp)/(2*tp+fp+fn),4)
acc[k,2]=round((tp+tn)/(tp+tn+fp+fn),4)

}


n <- colnames(train)
f <- as.formula(paste("Target ~", paste(n[!n %in% "Target"], collapse = " + ")))

library(e1071)

nb <- naiveBayes(as.factor(Target) ~ .,   data=train)
pred2=predict(nb, test)
rsult2=ifelse(pred2==true,1,0)#---54.8
sum(rsult2)/length(rsult2)

library(neuralnet)

mydata=train
nam=colnames(mydata)
mydatasca=matrix(0,nrow(mydata),ncol(mydata))
colnames(mydatasca)<-colnames(mydata)
strain=train
stest=test
for (i in 2:ncol(mydata)){
  #mydata[,i]=na.spline(mydata[,i]) #impute
  maxs <- max(mydata[,i]) 
  mins <- min(mydata[,i])
  strain[,i] <- as.data.frame(scale(mydata[,i], center = mins, scale = maxs - mins))
  stest[,i]<-as.data.frame(scale(test[,i], center = mins, scale = maxs - mins))
}


n <- colnames(strain[,1:16])
f <- as.formula(paste("Target ~", paste(n[!n %in% "Target"], collapse = " + ")))
nn <- neuralnet(f,data=strain[,1:16],hidden=c(5,3),linear.output=T)
pr.nn <- compute(nn,stest[,1:16])
Pred2= pr.nn$net.result
rsult2=ifelse(Pred2==true,1,0)
sum(rsult2)/length(rsult2)


