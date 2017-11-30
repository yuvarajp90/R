setwd("/Users/roshanaknateghi/Desktop/608_595DataAnalytics/595_SP16/595_Labs")

getwd()

input<-read.table(file='sabattelli_mann.txt', header=T)
#Holdout Sampling

#Helper function for holdout sampling
#Define this function in the environment before running the holdout sample
#Just highlight and run script to define in environment

GenString = function(num, length)
{
	y = c()
	for(i in 1:length)
	{
		if(num==i)
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



#define variables for the holdout script

#Data to predict from
data = input
#Data to predict
actual = input$TC

#number of holdout samples to run
numHoldouts = nrow(data)

#initialize vector of mean squared errors
vecOutSampMSE = c()
vecOutSampSSE = c()
vecOutSampSST = c()
vecOutSampRSq = c()
vecInSampRSq = c()
vecHoldout = c()
vecPredicted = c()

#for loop of holdouts
for (i in 1:numHoldouts)
{
	#generate random string
	y = GenString(i, nrow(data))

	#create holdout sample for this iteration
	tmp_data = cbind(data,y)
	tmp_actual = cbind(actual,y)
	holdout=subset(tmp_data,y==1)[,1:length(data)]
	holdout_actual=subset(tmp_actual,y==1)[,1]
	leftover=subset(tmp_data,y==0)[,1:length(data)]
	leftover_actual=subset(tmp_actual,y==0)[,1]

	#Fit Model to leftover data
	reg<-glm(TC~SST+NAO+NINO,family=poisson, data=leftover)
	
	
	model <-step(reg, direction = "both", k = 3.84, data = leftover)

	vecPredicted[i] = predict(model,holdout,type="response")
	vecHoldout[i] = holdout_actual

	#calculate means squared error for this holdout sample
	vecOutSampMSE[i] = mean((holdout_actual-predict(model,holdout))^2)

	#Calculate the sum of squared error
	vecOutSampSSE[i] = sum((holdout_actual-predict(model,holdout))^2)

	#Calculate the total sum of squares
	vecOutSampSST[i] = sum((holdout_actual-mean(actual))^2)

	#Calculate R squared
	vecOutSampRSq[i] = 1-(vecOutSampSSE[i]/vecOutSampSST[i])

	#Calculate R squared for insample
	vecInSampRSq[i] = 1 - (sum((leftover_actual-predict(model,leftover))^2)/sum((leftover_actual-mean(leftover_actual))^2))
}

	AverageSSE = mean(vecOutSampSSE)
	AverageSST = mean(vecOutSampSST)
	AverageRSq = AverageSSE/AverageSST

