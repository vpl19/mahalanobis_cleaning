library(MASS)  #  for simulating the data

# Example on simulated data

N <- 100000 # Number of random samples
set.seed(123)
# Target parameters for univariate normal distributions
rho <- 0.8
mu1 <- 1; s1 <- 1
mu2 <- 2; s2 <- 1.5

# Parameters for bivariate normal distribution
mu <- c(mu1,mu2) # Mean 
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance
bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma ) # from MASS package
#plot(bvn1[,1],bvn1[,2])

# adding some outliers
rho <- -0.8
mu1 <- 3; s1 <- 2
mu2 <- 5; s2 <- 3
mu <- c(mu1,mu2)
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)

bvn1<-rbind(bvn1,mvrnorm(100, mu = mu, Sigma = sigma ))
#plot(bvn1[,1],bvn1[,2])

var1<-bvn1[,1]
var2<-bvn1[,2]

res<-mahalanobis_plot(var1,var2) # from Mahalanonis_detection.R
res[[1]]
length(res[[2]])


