#optional change of prompt appearance
# i.e. instead of ">" appearing in the console, a text of your choosing will
options(prompt="what do you want  ")
options(prompt = "> ")

# get the current working directory
getwd()

# Change your working directory
setwd("E:/01. M.S. - IE (OR)/02. Spring 2016/01. IE 595 - Advanced data analytics/02. R Lab sessions")

###Let's first do some exploratory data analysis

# Imports the data, sep="\t" means tab delimited
data<-read.table("sabattelli_mann.txt",sep="\t",header=TRUE)

# attaches the dataset. so that it doesnt have to called 
# everytime to reference columns
attach(data)

# Allows editing the data
edit(data)

# returns the summary of the dataset - min/mean/median/mode/max
summary(data)

# names of data variables in the dataset
names(data)

# creates a scatterplot matrix of all the variables in the data
pairs(data)
# scatterplot matrix for only specific variables
pairs(~SST+NAO)

ls()
objects()

# prints the column "TC" from the "data" dataset
data$TC

# returns the mean and std-dev of the "TC" variables
mean(data$TC)
sd(data$TC)

# plots a scatterplot with proper cosmetic additions
plot(Year,TC, xlab="Year",ylab="TC", main="TC Plot", xlim=c(1870,2005),ylim=c(0,25),col="blue")

identify(Year,TC, labels=row.names(data))
##Now click on the points of interest on the plot

plot(Year,TC, xlab="Year",ylab="TC", main="TC Plot", xlim=c(1870,2005),ylim=c(0,25),col="blue",pch=19)

plot(Year,TC, xlab="Year",ylab="TC", main="TC Plot", xlim=c(1870,2005),ylim=c(0,25),col="blue",pch=19,cex=1)

plot(Year,TC, xlab="Year",ylab="TC", main="TC Plot", xlim=c(1870,2005),ylim=c(0,25),col="blue",pch=19,cex.main=2)

plot(Year,TC, xlab="Year",ylab="TC", main="TC Plot", xlim=c(1870,2005),ylim=c(0,25),col="blue",pch="R",cex.main=2)

plot(Year,TC, xlab="Year",ylab="TC", main="TC Plot", xlim=c(1870,2005),ylim=c(0,25),col="blue",pch=22,cex.main=2)

# pch is used to define the shape of the data points
# cex adjusts the size of the shape of the data points
?pch

hist(TC,xlim=c(0,25))
hist(TC)

plot(TC)

# lines has to be used in the conjunction with plot
# the fn joins the points of only the current plot
# lty=connected/dotted line
# lwd=width of the line
lines(TC,lty=2,lwd=2)

### after this i dont understand ###
text(64,21,"anomaly")
legend("bottomleft", cex=1.25, legend = ("TC"))

plot(TC)
text(locator(1),"anomaly")

par(c("col","lty"))
par(oldpar) ## restores the original setting


################################################################################################################################################################################
#################Excercise in plotting
 You can show mathematical symbols in texts using the function expression(). Please use help(plotmath) to learn details.
 
x<-seq(0,2*pi,by=0.01); y<-sin(x^2)
xtext<-expression(theta)
ytext<-expression(hat(mu))
mtext<-expression(paste(hat(mu),"=sin(",theta^2,")"))
plot(x,y,type="l",xlab=xtext,ylab=ytext)
text(0.5,0,mtext)

#######Axis() can be used to set the axis line and tick marks:
x<-seq(0,2*pi,by=0.01); y<-sin(x)
plot(x,y,type="l")
plot(x,y,type="l",xaxt="n",yaxt="n")
u<-2*pi*(0:2)/2
axis(1,at=u,labels=c("0",expression(pi),expression(paste("2", pi))))
axis(2,at=c(-1,0,1),las=1)

#mfcol and mfrow allow you to create multiple figures on a single page. mfcol fills the subplots by column, and mfrow fills by row.
x<-rnorm(100); y<-x+rnorm(100,sd=0.5)
oldpar<-par(mfrow=c(2,3),oma=c(0,1,1,0),mar=c(4,4,3,2))
hist(x); hist(y); boxplot(x,y)
plot(x,y); qqplot(x,y); qqnorm(x)

#layout() is another way to organize multiple figures:
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
oldpar<-par(oma=c(0,1,1,0),mar=c(4,4,3,2))
plot(x,y)
hist(x); hist(y)

#save files
save(list=ls(all=TRUE), file="595LabSession3.RData")
savehistory(file="595LabSession3.txt")




################################################################################################################################################################################
################################Plotting exercise
#############Plotting CO2 data
?CO2

layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
oldpar<-par(oma=c(0,1,1,0),mar=c(4,4,3,2))
par(oldpar)
xtext<-expression(paste("ambient"," ", CO[2]," ","concentration (mL/L)"))
ytext<-expression(paste(CO[2]," ","uptake rates"," ", "(",mu, "mol/" , m^2, "sec",")",))

plot(jitter(uptake)~jitter(conc),data=CO2[64:84,],xlab=xtext, ylab=ytext,type="o",col="blue",ylim=c(0,50),pch=c("1","2","3"))
mtext(expression(paste(CO[2]," ","uptake curves for individual plants")),3)

title(main="Exploratory plots for Echinochloa crus-galli data",cex.main=1.2)


points(jitter(uptake)~jitter(conc),data=CO2[43:63,],col="brown",type="o", pch=22)
points(jitter(uptake)~jitter(conc),data=CO2[22:41,],col="navy",type="o", pch=20)
points(jitter(uptake)~jitter(conc),data=CO2[1:21,],col="red",type="o", pch=24)


legend("bottomright", legend=c("Mississipi chilled", "Mississipi nonchilled","Quebec chilled", "Quebec nonchilled" ), col=c("blue","brown","navy","red"),lty=1,bg="gray",cex=0.4)
boxplot(CO2[,5]~CO2[,2],col=c("yellow","orange"),main=expression(paste("Boxplot for"," ",CO[2],"uptake in terms of plant origin")), ylab=ytext)

hist(CO2[,5],freq=FALSE,xlab="",main=expression(paste("Histogram of"," ",CO[2]," ","uptake rates")))

d<-density(CO2[,5])
lines(d,col="orange",lwd="3")

mtext("Just proving that we can write things here too", 4, col="purple",adj=0)

######################################################################################################################################################################################################simple linear model#################################
<-rnorm(16,mean=3,sd=2)
y<-0.2+0.1*x+rnorm(16,mean=0,sd=0.3)
z<-lm(y~x)
summary(z)

