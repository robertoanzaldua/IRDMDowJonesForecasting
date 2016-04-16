
mypath = "/Users/mariflorvega/Downloads/IRDM_DATA_STOCKS/STOCKS/"



#---------------------------------Calling libraries

library(forecast)
library(infotheo)
library(randomForest)
library(deepnet)

#--------------------------------- Accuracy Function

acc=function(Pred, true, type="acc"){
  tp=sum(ifelse(Pred==1 &true==1,1,0))
  tn=sum(ifelse(Pred==0 &true==0,1,0))
  fp=sum(ifelse(Pred==1 &true==0,1,0))
  fn=sum(ifelse(Pred==0 &true==1,1,0))
  
  precision=round(tp/(tp+fp),4)
  recall=round(tp/(tp+fn),4)
  f1=round((2*tp)/(2*tp+fp+fn),4)
  acc=round((tp+tn)/(tp+tn+fp+fn),4)
  
  if(type=="precision"){result<-precision}
  if(type=="recall"){result<-recall}
  if(type=="f1"){result<-f1}
  if(type=="acc"){result<-acc}
  return(result)
}


#---------------------------------Getting the Data
files=list.files(path = mypath)
stocksacc=matrix(0,0,6)
for (s in 1:length(files)){
file=paste(mypath,files[s], sep="")
stockname=substr(files[s],1,nchar(files[s])-11)

data=read.table(file, header=T, sep=",")
data$Date=as.Date(data$Date, "%Y-%m-%d")
dowdata<-data[order(data$Date),]


#---------------------------------Feature Creation 
#--Lag1
end=nrow(dowdata)
target=ifelse(dowdata$Adj.Close[-1]-dowdata$Adj.Close[-end]>0,1,0)
dowdatavar=cbind(dowdata[-1,1],dowdata$Adj.Close[-1],target,dowdata[-end,c(7,2:6)])
colnames(dowdatavar)[1:2]<-c('Date','Serie')

columnNames=cbind('Closeadj_L1','Open_L1','High_L1','Low_L1','Close_L1','Vol_L1')
nrlags = 5
for(j in 2:nrlags){
  last = end-j
  dowdatavar = cbind(dowdatavar[-1,], dowdata[1:last,c(7,2:6)])
  
  columnNames = cbind(columnNames,paste('Closeadj_L',j, sep=""),paste('Open_L',j, sep=""),paste('High_L',j,sep=""),paste('Low_L',j, sep=""),paste('Close_L',j,sep=""), paste('Vol_L',j,sep=""))
}

columnNames = cbind('Date','Serie','Target', columnNames)
colnames(dowdatavar)<-columnNames

#--Other Features
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
  dowdatavar$PerCloseAdj[i]= 1-round(dowdatavar$Closeadj_L1[i]/dowdatavar$Closeadj_L1[i-1],4)
  dowdatavar$PerOpen[i]= 1-round(dowdatavar$Open_L1[i]/dowdatavar$Open_L1[i-1],4)
  dowdatavar$PerHigh[i]= 1-round(dowdatavar$High_L1[i]/dowdatavar$High_L1[i-1],4)
  dowdatavar$PerLow[i]=1-round(dowdatavar$Low_L1[i]/dowdatavar$Low_L1[i-1],4)
  dowdatavar$PerClose[i]=1-round(dowdatavar$Close_L1[i]/dowdatavar$Close_L1[i-1],4)
  dowdatavar$PerVol[i]=1-round(dowdatavar$Vol_L1[i]/dowdatavar$Vol_L1[i-1],4)
  
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


#--------------------------------- Feature selection with mutual information

dowdatavarDisc = discretize(dowdatavar)
mutualIfo = c()
for (k in 4:ncol(dowdatavar)){
  mutualIfo=c(mutualIfo,mutinformation(dowdatavarDisc$Serie, dowdatavarDisc[,k], method="emp"))
}
columns = colnames(dowdatavar)
names(mutualIfo)<-colnames(dowdatavar)[4:ncol(dowdatavar)]
mibest=names(sort(mutualIfo,decreasing=T)[1:15])

dowdatavarbest_mi = dowdatavar[,c(1,2,3)]
for(i in 1:length(mibest)){
col=which(colnames(dowdatavar)==mibest[i])
  dowdatavarbest_mi = cbind(dowdatavarbest_mi, dowdatavar[,col])
}

namesCol = c('Date', 'Serie','Target',mibest)
colnames(dowdatavarbest_mi)<-namesCol


#--------------------------------- Feature selection with Random Forest

rfFeatures=c()
for(i in 1:5){
  fit <- randomForest(Serie ~ .,   data=dowdatavar[,-c(1,3)], importance=T, ntree=100)
  #fit <- randomForest(SalesVol ~.,   data=salesf, importance=T)
  ifit=importance(fit)
  vars=ifit[order(-ifit[,1]),][1:15,]
  rfFeatures<-c(rfFeatures,rownames(vars))
}

rfbest = names(sort(table(rfFeatures),decreasing=T)[1:15])

dowdatavarbest_rf = dowdatavar[,c(1,2,3)]
for(i in 1:length(rfbest)){
  col=which(colnames(dowdatavar)==rfbest[i])
  dowdatavarbest_rf = cbind(dowdatavarbest_rf, dowdatavar[,col])
}
namesCol = c('Date', 'Serie','Target',rfbest)
colnames(dowdatavarbest_rf)<-namesCol


#---------------------------------Rolling Cross-Validation

selectedvars=dowdatavarbest_rf
cuts=c("01/01/2014","01/01/2015","01/07/2015","01/10/2015")

meanacc=matrix(0,0,6)
for(k in 1:length(cuts)){
print(k)

dowdatasub=selectedvars[which(selectedvars$Date>=as.Date(cuts[k], "%d/%m/%Y")),]
dowdatasubo=dowdatavar[which(dowdatavar$Date>=as.Date(cuts[k], "%d/%m/%Y")),1:9]
stock <- msts(dowdatasubo$Serie, seasonal.periods=c(5), ts.frequency=5)
plot(stock, main=paste(stockname," Stock Prices"), ylab="Adj.Close Price", col="darkblue")

results=matrix(0,0,6)
for(j in 1:20){
edge=round(nrow(dowdatasub)*0.8)
end=nrow(dowdatasub)

PredAR=PredDM=PredLR=PredRF=PredPer=PredDBN=PredDBNtanh=PredDBNlin=PredNN=PredSAE=true=c()
accAR=accDM=accLR=accRF=accPer=accDBN=accDBNtanh=accDBNlin=accNN=accSAE=c()
colnames(results)<-c('accDM','accLR','accRF','accDBN','accDBNtanh','accNN')
h=1
for (i in edge:(end-1) ){

  train=dowdatasub[1:i,-1]
  test=dowdatasub[(i+1),-c(1)]
  
  
  dm=auto.arima(stock[1:i], xreg= dowdatasubo[1:i,-c(1,2,3)],seasonal=T, lambda = 0)
  PredDM=ifelse(forecast(dm,xreg= dowdatasubo[i+1,-c(1,2,3)], h=1)$mean[1]>train[nrow(train),1],1,0)
  
  lr <- lm(Serie ~ .-Target,   data=train)
  PredLR[h] <- ifelse(as.numeric(predict(lr, test))>train[nrow(train),1],1,0)
  
  rf <- randomForest(Serie ~ .-Target,   data=train)
  PredRF[h] <- ifelse(as.numeric(predict(rf, test))>train[nrow(train),1],1,0)  
  
  trainx <- as.matrix(train[,-c(1,2)])
  trainy <- as.matrix(train[,2] * 1)
  testx <- as.matrix(test[,-c(1,2)])
  testy<- as.matrix(test[,2] * 1)
  true=c(true,testy)
  
  dnn <- dbn.dnn.train(trainx, trainy, hidden = c(5,5), numepochs=5)
  PredDBN[h]<- ifelse(nn.predict(dnn, testx)>=0.5,1,0)
  
  dnn <- dbn.dnn.train(trainx, trainy, hidden = c(5,5), activationfun="tanh")
  PredDBNtanh[h]<- ifelse(nn.predict(dnn, testx)>=0.5,1,0)
  
  nn <- nn.train(trainx, trainy, hidden = c(5))
  PredNN[h]<- ifelse(nn.predict(nn, testx)>=0.5,1,0)
  
  h=h+1
}

accDM=acc(PredDM,true)
accLR=acc(PredLR,true)
accRF=acc(PredRF,true)
accDBN=acc(PredDBN,true)
accDBNtanh=acc(PredDBNtanh,true)
accNN=acc(PredNN,true)

results=rbind(results, c(accDM,accLR,accRF,accDBN,accDBNtanh,accNN))
}

meanacc<-rbind(meanacc,apply(results,2,mean))

}
rownames(meanacc)<- paste(rep(stockname,4), rep("k",4),seq(1:4))
stocksacc=rbind(stocksacc,meanacc)
print(stocksacc)
}

