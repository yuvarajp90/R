##LAB 7: Classification And Regression Trees
##predicting body fat and galucoma diagnosis

install.packages("partykit")

#rpart is implementation of Beirman et al (1984)
library("rpart")
library("sandwich")
library("party") 
library("lattice") 
library("partykit") 
library("stabs")
library("parallel")
library("mboost")

############################
#Predicting body fat content
############################

install.packages('TH.data')
data("bodyfat", package = "TH.data")

## Dual Energy X-Ray Absorptiometry (DXA) measurement of fat
# rpart.control(minsplit) tells the model that a minimum number of observation
# should be there in each node before splitting it further
bodyfat_rpart <- rpart(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth, data = bodyfat, control = rpart.control(minsplit = 10))

plot(as.party(bodyfat_rpart), tp_args = list(id = FALSE))

print(bodyfat_rpart$cptable)

# which.min finds the location of the minimum xerror
opt <- which.min(bodyfat_rpart$cptable[,"xerror"])

cp <- bodyfat_rpart$cptable[opt, "CP"]

bodyfat_rpart$cptable[opt]

bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
plot(as.party(bodyfat_prune), tp_args = list(id = FALSE))


DEXfat_pred <- predict(bodyfat_prune, newdata = bodyfat) 

xlim <- range(bodyfat$DEXfat) 
plot(DEXfat_pred ~ DEXfat, data = bodyfat, xlab = "Observed", ylab = "Predicted", ylim = xlim, xlim = xlim) 
abline(a = 0, b = 1)

bodyfat_ctree <- ctree(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth, data = bodyfat)
plot(bodyfat_ctree)

####################
#Galucoma diagnosis
####################
library("TH.data")
data("GlaucomaM", package = "TH.data") 
nrow(GlaucomaM)
head(GlaucomaM)

glaucoma_rpart <- rpart(Class ~ ., data = GlaucomaM, control = rpart.control(xval = 100)) 
plot(as.party(glaucoma_rpart), tp_args = list(id = FALSE))

glaucoma_rpart$cptable

opt <- which.min(glaucoma_rpart$cptable[,"xerror"]) 

cp <- glaucoma_rpart$cptable[opt, "CP"] 

glaucoma_prune <- prune(glaucoma_rpart, cp = cp)
plot(as.party(glaucoma_prune), tp_args = list(id = FALSE))

## what is the optimal tree size?

nsplitopt <- vector(mode = "integer", length = 25) 

for (i in 1:length(nsplitopt)) { 
	cp <- rpart(Class ~ ., data = GlaucomaM)$cptable 
	nsplitopt[i] <- cp[which.min(cp[,"xerror"]), "nsplit"] } 

table(nsplitopt)

## bagging

trees <- vector(mode = "list", length = 25)
n <- nrow(GlaucomaM)

#Creates the index for the samples
bootsamples <- rmultinom(length(trees), n, rep(1, n)/n) 

mod <- rpart(Class ~ ., data = GlaucomaM, control = rpart.control(xval = 0)) 

for (i in 1:length(trees)) 
trees[[i]] <- update(mod, weights = bootsamples[,i])

#table(sapply(trees, function(x) as.character(x$frame$var[1])))

classprob <- matrix(0, nrow = n, ncol = length(trees)) 

for (i in 1:length(trees)) { 
	classprob[,i] <- predict(trees[[i]], 
	newdata = GlaucomaM)[,1] 
	 classprob[bootsamples[,i] > 0,i] <- NA  }

avg <- rowMeans(classprob, na.rm = TRUE)

predictions <- factor(ifelse(avg > 0.5, "glaucoma",  "normal")) 

predtab <- table(predictions, GlaucomaM$Class) 
predtab

round(predtab[1,1] / colSums(predtab)[1] * 100)

round(predtab[2,2] / colSums(predtab)[2] * 100)



gdata <- data.frame(avg = rep(avg, 2), class = rep(as.numeric(GlaucomaM$Class), 2), obs = c(GlaucomaM[["varg"]], GlaucomaM[["vari"]]), var = factor(c(rep("varg", nrow(GlaucomaM)),rep("vari", nrow(GlaucomaM))))) 

panelf <- function(x, y) { 
	panel.xyplot(x, y, pch = gdata$class)
	panel.abline(h = 0.5, lty = 2)  
	} 

print(xyplot(avg ~ obs | var, data = gdata, panel = panelf, scales = "free", xlab = "", ylab = "Estimated Class Probability Glaucoma")) 

glaucoma_ctree <- ctree(Class ~ ., data = GlaucomaM)

plot(glaucoma_ctree)
