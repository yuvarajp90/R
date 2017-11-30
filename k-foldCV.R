library(plyr)
library(randomForest)
library(ROSE)
k=5 #Folds
id <- sample(1:k,nrow(data),replace=TRUE)
list <- 1:k
prediction <- data.frame()
testsetCopy <- data.frame()
#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)
for (i in 1:k){
  trainingset <- subset(data, id %in% list[-i])
  # Performing upsampling of minorities using ROSE package
  trainingset <- ROSE(V1~., data=trainingset, seed=3, p=0.15, N=length(trainingset$V1))$data
  testset <- subset(data, id %in% c(i))
  mymodel <- randomForest(trainingset$V1~., data=trainingset, ntree=100)
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  progress.bar$step()
}
result <- cbind(adapred, testsetCopy)
names(result) <- c("Predicted", "Actual")
table(result)