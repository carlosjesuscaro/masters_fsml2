# Title     : Class 1 and 2
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-11-04

# Class 1
####################
A = runif(1000)
H = hist(A, breaks = c(0,0.2,0.4,0.6,0.8,1), freq = FALSE)
H$counts
max(A)
min(A)
# The area in the density is always equal to 1
# The density histogram allows us to identify the family distribution

# Creating a dataset with normal distribution
B = rnorm(1000)
hist(B, freq = FALSE)

# Creating a dataset with exponential distribution
C = rexp(1000)
hist(C, freq = FALSE)

# In terminal, we can check and set the working directory with getwd() and setwd()

# Class 2
####################
A = read.table('data1.txt')
table(A)

# From class problem
x = seq(-1,5, 0.01)
fx = 0*(x<0)+x^2/8*(x>=0)*(x<2)+(-x^2/8+x-1)*(x>=2)*(x<4)+1*(x>=4)
plot(x,fx,type='l',col='red')

u = runif(500000)
Y = (8*u)^0.5*(u<=0.5)+(4-(8-8*u)^0.5)*(u>1/2)
min(Y)
max(Y)
hist(Y, freq = FALSE, breaks = 50)

qt(0.9,12)
qt(0.995, 12)
qt(0.505, 12)

# Class 4
####################
mu = 2
sig2 = 1.7
n = 500
alpha = 0.05
# Creating the 500 observations with the parameters
A = rnorm(n, mu, sqrt(sig2))

# Computing the confidence interval without assuming that we know mu or sigma2
lowerbound = mean(A) - var(A)/sqrt(n) * qt(1-alpha/2,n-1)
upperbound = mean(A) + var(A)/sqrt(n) * qt(1-alpha/2,n-1)
c(lowerbound, upperbound)
# This is applying the formula from page 30 (Class 4 - Whiteboard PDF)

visual <- function(k, n, mu, sig2)
{
  M = matrix(data = 0, ncol = 2, nrow = k)
  for (i in 1:k)
  {
    A = rnorm(n, mu, sqrt(sig2))
    lowerbound = mean(A) - var(A)/sqrt(n) * qt(1-alpha/2,n-1)
    upperbound = mean(A) + var(A)/sqrt(n) * qt(1-alpha/2,n-1)
    M[i,1] = lowerbound
    M[i,2] = upperbound
  }
  N = sum((M[,2]<2) + (M[,1]>2))
  visual = list(interval = M, count = N)
}

Z = visual(500000, n, mu, sig2)
Z$count

# Exercise from class 2 PDF (but done in class 4)
# Matrix with 200 columns and 1,000 rows with data from a Poisson
# distribution with lambda 5
Matrix = matrix(data=rpois(200*1000,5),ncol=200)

# Calculating the empirical mean row by row. This means that N is composed of
# 1,000 observations of lambdahat_(n,2)
N = apply(Matrix, 1, mean)
# Margin = 1 indicates that the function 'mean' must be applied to the rows
# (2 would indicate to apply the formula to the columns)

# Histogram of N
H = hist(N, freq = FALSE)
# The distrigution looks like a Gaussian. This is expected thanks to the
# Central Limit Theorem
# What are the values of the Gaussian distribution:
H1 = hist(N, plot = FALSE)

# The idea is to determine the Gaissian parameters and then we are going to
# plot the histogram from the sample data versus the Gaussian distribution

# Values that determine the classes
limits = H$breaks
# Smallest limit of classes
xmin = min(limits)
# Biggest limit of classes
xmax = max(limits)
x = seq(xmin, xmax, 0.01)
# The Variance of X_bar is lambda / n (n is the number of Poisson RV used to
# calculate the empirical mean oe the number of observations used to calculate
# the empirical mean)
y = dnorm(x, 5, sqrt(5/200))
ymax = max(y, H$density)
# Plotting the sample data histogram with a super position of the Gaussian
# distribution with the mu and sigma parameters
hist(N, freq = FALSE, xlim = c(xmin, xmax), ylim = c(0, ymax))
par(new = TRUE)
plot(x,y,type='l',col='red',xlim = c(xmin, xmax), ylim = c(0, ymax))

########################################################################
# Calculating confidence intervals for 3 data sets
# Dataset 1: simu1.txt

# Loading the data
D1 = read.table('simu1.txt')
# D1 is a vector with 10,000 values

# Distribution (visual inspection)
DH1 = hist(D1$V1, freq = FALSE) #Histogram
# Based on a first visual inspection, it could be a Poisson, Exponential,
# Chi squared or Gamma distribution. However, it is closer to an Exponential
# distribution where the key parameter is lambda

# Estimating lambda
# By definition, the expectation of the mean is 1/lambda. So, lambda_hat will
# be 1/mean(sample or D1)
lambda_hat = 1/mean(D1$V1)
ks.test(D1$V1, 'pexp', lambda_hat)
# the p-value is 0.5288 which is bigger than alpha (0.05). So we keep H0 ->
# meaning that the sample data comes from the reference distribution

# Building the confidence interval (based on the formula from class4.PDF page 65):
exp_lowerbound = 1/mean(D1$V1)-1/(sqrt(length(D1$V1))*mean(D1$V1))*qnorm(0.975)
exp_upperbound = 1/mean(D1$V1)+1/(sqrt(length(D1$V1))*mean(D1$V1))*qnorm(0.975)
c(exp_lowerbound, exp_upperbound)
# Results: lowerbound = 3.953059 and upperbpund = 4.111114
# lambda_hat is 4.032087 which is inside the confidence interval

# Box Muller technique to create artifically a Gaussian distribution

# Dataset 2: simu2.txt
D2 = read.table('simu2.txt')
# D2 is a vector with 1,000 values

# Distribution (visual inspection)
DH2 = hist(D2$V1, freq = FALSE) #Histogram
# It looks like a Gaussian distribution

# Confidence intervals
# Expectation
D2_mu_lowerbound = mean(D2$V1) - var(D2$V1)*qt(0.975, length(D2$V1)-1)/sqrt(length(D2$V1))
D2_mu_upperbound = mean(D2$V1) + var(D2$V1)*qt(0.975, length(D2$V1)-1)/sqrt(length(D2$V1))
c(D2_mu_lowerbound, D2_mu_upperbound)

# Variance
D2_var_lowerbound = (length(D2$V1) - 1)*var(D2$V1)/qchisq(0.975, length(D2$V1)-1)
D2_var_upperbound = (length(D2$V1) - 1)*var(D2$V1)/qchisq(0.025, length(D2$V1)-1)
c(D2_var_lowerbound, D2_var_upperbound)

# Dataset 3: simu3.txt
D3 = read.table('simu3.txt')
# D3 is a vector with 1,000 values

# Distribution (visual inspection)
DH3 = hist(D3$V1, freq = FALSE) #Histogram
# It looks like a Gaussian distribution

# Confidence intervals
# Expectation
D3_lowerbound = mean(D3$V1) - var(D3$V1)*qt(0.975, length(D3$V1)-1)/sqrt(length(D3$V1))
D3_upperbound = mean(D3$V1) + var(D3$V1)*qt(0.975, length(D3$V1)-1)/sqrt(length(D3$V1))
c(D3_mu_lowerbound, D3_mu_upperbound)

# Variance
D3_var_lowerbound = (length(D3$V1) - 1)*var(D3$V1)/qchisq(0.975, length(D2$V1)-1)
D3_var_upperbound = (length(D3$V1) - 1)*var(D3$V1)/qchisq(0.025, length(D2$V1)-1)
c(D3_var_lowerbound, D3_var_upperbound)

# Indepence test between D2 and D2 (simu2 and simu3)
# Chi square of independence:
# H0 -> the RVs are independent
# H1 -> the RVs are dependent
chisq <- chisq.test(D2$V1, D3$V1)
# p value = 0.2397
# sim2 and sim3 are NOT independent