
# installs the package "HSAUR2"
install.packages('HSAUR2')

# loads an installed package
library('HSAUR2')

# loads the specified dataset
data("Forbes2000", package="HSAUR2")

ls()

print(Forbes2000)

# displays the structure of the dataset
str(Forbes2000)

# returns the documentation on the topic
help("Forbes2000")

# returns the data type  
class(Forbes2000)

# reuturns the dimensions i.e. number of rows and columns
dim(Forbes2000)

# returns the number of rows
nrow(Forbes2000)

# returns the number of columns
ncol(Forbes2000)

# returns the names of the data variables in the dataset
names(Forbes2000)

# returns the row and column names
dimnames(Forbes2000)

# returns the data type of the specified data variable in the mentioned dataset
class(Forbes2000[,'rank'])

# returns the number of elements in specific data column in the dataset
length(Forbes2000[,'rank'])

class(Forbes2000[,'category'])

# returns the number of levels
nlevels(Forbes2000[,'category'])

# prints the distinct levels, levels option also does the sames
unique(Forbes2000[,'category'])

# returns the frequency count of the values in the specified column
table(Forbes2000[,'category'])

# storing the dataset in a new dataset
f<-Forbes2000

# prints the names of the companies and their sales whose assets > 1000
f[f$assets>1000, c('name','sales')]

# returns the number of companies satisfying the asset condition
table(Forbes2000$assets>1000)

# complete cases checks if there are missing values
# returns the count of number of rows with missing values
table(complete.cases(Forbes2000))

# creates a dataset which satisfied the condition
UK<-subset(Forbes2000,country=="United Kingdom")

# returns the summary of the specified data column or all the columns in
# the dataset depending on the user input
lapply(f, summary)

# returns the profit @ a category level
mprofits<-tapply(f$profits, f$category, median, na.rm=T)

iqr<-function(x){
	q<-quantile(x,prob=c(0.25,0.75),names=F)
	return(diff(q))
}

?IQR


iqr<-function(x,...){
	q<-quantile(x,prob=c(0.25,0.75),names=F,...)
	return(diff(q))
}


iqr_profits<-tapply(f$profits,f$category, iqr,na.rm=T)

# returns the unique levels of a data variable
levels(f$category)

# returns those levels which satisfy the criterion
levels(f$category)[which.max(iqr_profits)]

###graphics
par(mfrow=c(2,1))
hist(f$marketvalue)
hist(log(f$marketvalue))

plot(log(marketvalue)~log(sales),data=f,col=rgb(1,0,0,0.1), pch=16)


tmp<-subset(f, country %in% c("United Kingdon", "Germany", "India", "Turkey"))

#what happens if you don't include drop?
tmp$country<-tmp$country[,drop=T]
plot(log(marketvalue)~country,data=tmp,varwidth=T)