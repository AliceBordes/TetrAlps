#### PhD Tetralpes project ####

# Alice Bordes #

# September 2024 #

# Description:

# How to calculate the home range size ? Here a mathematical explanation of what is the HR
# Explanation use of mutivariate (bivariate) gaussian (normal) distribution 
# to compute confidence ellipse of the smallest circle guaranteeing a given probability

# Video support : 
# https://www.youtube.com/watch?v=OY5AmSP5BzE


#### 1_Loading objects ----

### Loading libraries ---- 
#********************************************************************
library(matlib)
#******************************************************************** 


#********************************************************************
CovMatrix <- diag(2) #standard normal distribution ; diag(n) returns an n-by-n identity matrix with ones on the main diagonal and zeros elsewhere.
eigen(CovMatrix) #valeurs propres of the identity matrix

# bivariate probability density function for standard normal = circles of equal probability density

# goal of the video : confidence region 

# to get the probability of 1 rectangle --> 1 need to integrate the probability density function in that rectangle.
# (if I integrate everywhere it gives 1)

# how computing confidence interval/confidence regions? 
# goal : find the "smallest" area guaranteeing a given probability (confidence, often 95%) of a sample landing at it 
# the confidence region for a given confidence (ex: 95%) with the smallest area = a circular shape

                # x <-  seq(-4, 4, length.out = 100)
                # contour(exp(-(1*x*inv(CovMatrix)*x))/sqrt(2*pi*det(CovMatrix)))

CovMatrix <- diag(1,2) #diagonal covariance matrix, such that the sd of X1 = 1, sd of X2 = 2... bc the variance is square of sd (in square meters unit)
# if this matrix is not diagonal, it means my variables are correlated (not statistically independents)
# so we have 2 independents normal variables stacked together to form a 2D normal variable

# sd = sqrt(of the corresponding element of the diag matrix)
sigma1 = sqrt(CovMatrix[1,1])
sigma2 = sqrt(CovMatrix[2,2])
#confidence region = 95%
errbnd <- 0.05 # equivalent 1/20 values falls outside of my predictions
confidence <- 1-errbnd


#### Marginal analysis = if i study each of the variable individually/independently without thinking of the relationship with the other one
cfsingle = qnorm(1-errbnd/2) 
# qnorm() to find the quantile for the normal distribution = calculates the inverse of the cumulative distribution function for a normal distribution
# 1.96 times the sd = the confidence interval for a single normal variable
# cfsingle depict the horizontal and vertical bands line around the circle (square rectangle)

# Joint analysis : in 2D
# chi-squared (chi2) distribution to compute(calculate) the confidence circle = minimal area
dims = 2 # x,y
chich = qchisq(confidence, dims) # chi2 bound, quantile function and random generation for the chi-squared
# chich is the probability that x1^2 + x2^2 beeing standard normal < 5.991465 : P(x1^2 + x2^2 =< 5.991465) = confidence value at 95%
# chich = squared radius of the circle
cfellipse = sqrt(chich) # radius of the circle of the minimum area


area = pi*sqrt(det(CovMatrix))*chich # pi*(diag(2)*variance)*r^2
# area of an ellipse = product of semiaxis times pi. The semiaxis = sqrt(covariance eigen values) = sqrt(det(covMatrix)) (eigen values = valeurs propres). chich = zoom factor at each axis (cfellipse^2) 
# this circle (area) is our 95% confident region with the smallest area 
#********************************************************************

# Computation of Abel's HR area
# for the formula BetaTR(x,y) = x*betax + y*betay - betarr*((x^2 + y^2)/2)
# In a GLM with family = poisson(), the estimated coefficients are interpretable on a log scale.
# Beta.rr (in log scale) = 1/var(x) = 1/var(y), so ~ var(x) = var(y) = exp(1/1.725) 
CovMatrix = diag(1/exp(1.725),2) #CovMatrix = contains the variance of the 2 variables (x,y) in a diag matrix 
Abel_area = pi*sqrt(det(CovMatrix))*chich  # sqrt(det(CovMatrix)) = contains 1/sd(x) and 1/sd(y)




#ellipsoid non corrélée = éléments diagonaux = 0


















dims = 2 # x,y
errbnd <- 0.05 
confidence <- 1-errbnd
chich = qchisq(confidence, dims)
CovMatrix = diag(180941.4,2) #CovMatrix = contains the variance of the 2 variables (x,y) in a diag matrix 
Abel_area = pi*sqrt(det(CovMatrix))*chich
Abel_area/1000000




