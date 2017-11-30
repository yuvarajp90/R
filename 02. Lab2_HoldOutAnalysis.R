
#setwd("/Users/roshanaknateghi/Desktop/608_595DataAnalytics/595_Labs")


hurdata<-read.table('sabattelli_mann.txt',header=T)

attach(hurdata)

names(data)

summary(data)


RandomString = function(percent,length)
{
  y = c()
  for(i in 1:length)
  {
      if(runif(1,0,1)<=percent)
      {
          y[i] = 1
      }
      else
      {
          y[i] = 0
      }
  }
  y
}
data=hurdata
actual=TC
numHoldouts = 50
vecMSE = c()
vecMAE= c()

for(i in 1:numHoldouts)
{
y = RandomString(.10,nrow(data))
tmp_data = cbind(data,y)
tmp_actual = cbind(actual,y)
holdout=subset(tmp_data,y==1)
holdout_actual=subset(tmp_actual,y==1)
leftover=subset(tmp_data,y==0)

leftover<-data.frame(leftover)
holdout<-data.frame(holdout)

model<-glm(TC~SST+NAO+NINO,family=poisson,data=leftover)

vecMSE[i] = mean((holdout_actual[,1]-predict(model,holdout,type="response"))^2)
vecMAE[i] = (mean(abs(holdout_actual[,1]-predict(model,holdout,type="response"))))
}