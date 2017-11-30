#set the directory to the desired location
setwd("/Users/roshanaknateghi/Desktop/608_595DataAnalytics/595_SP16/595_Labs")

#Install and load the directory
install.packages("randomForest")
library("randomForest")

#load your data
hurdata<-read.table('sabattelli_mann.txt',header=T)

#fit a random forest model
rf<-randomForest(TC~SST+NINO+NAO,hurdata)
#shows the optimal #of trees: choose the minimum number of trees for lowest errors
plot(rf)
#fitting the model
rf<-randomForest(TC~SST+NINO+NAO,hurdata, ntree=60)
### how well is the model fit??

##Let's check the residuals:
qqnorm(rf$mse)
library(car)
qqplot(rf$mse)


#variable importance

varImpPlot(rf,sort=TRUE,scale=TRUE,cex=1.2,pch=16,main="Variable Importance")
importance(rf)
#check for variable influence
partialPlot(rf,x.var='NINO',pred.data=hurdata, ylab='TC')
partialPlot(rf,x.var='NAO',pred.data=hurdata, ylab='TC')
partialPlot(rf,x.var='SST',pred.data=hurdata, ylab='TC')
