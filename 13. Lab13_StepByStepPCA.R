###### Step by Step Calculation of PCA

mycor <- cor(USArrests) # calculate a correlation matrix

myEig <- eigen(mycor) # find the eigenvalues and eigenvectors of correlation matrix 

# eigenvalues stored in myEig$values 
# eigenvectors (loadings) stored in myEig$vectors

mysd <- sqrt(myEig$values) # calculating singular values from eigenvalues

myloadings <- myEig$vectors

rownames(myloadings) <- colnames(USArrests) # saving as loadings, and setting rownames

# transforming data to zero mean and unit variance
standardize <- function(x)
{(x - mean(x))/sd(x)} 
X <- apply(USArrests, MARGIN=2, FUN=standardize) 

#an easier way to standardize the data is to use the scale() command in R

myscores <- X %*% myloadings # calculating scores from eigenanalysis results

#Compare results from the two analyses to demonstrate equivalency. 


###########How about better graphics?
install.packages('devtools')
library(devtools)
install_github('vqv/ggbiplot',force=T)
library(ggbiplot)
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = wine.class, ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

