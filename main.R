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

# Exercise from class 2
# Matrix with 200 columns and 1,000 rows with data from a Poisson
# distribution with lambda 5
Matrix = matrix(data=rpois(200*1000,5),ncol=200)

# Calculating the empirical mean row by row. This means that N is composed of
# 1,000 observations of lambdahat_(n,2)
N = apply(Matrix, 1, mean)

# Histogram of N
H = hist(N, freq = FALSE)
# The distrigution looks like a Gaussian. This is expected thanks to the
# Central Limit Theorem

# Values that determine the classes
limits = H$breaks
# Smallest limit of classes
xmin = min(limits)
# Biggest limit of classes
xmax = max(limits)
x = seq(xmin, xmax, 0.01)
y = dnorm(x, 5, sqrt(5))
ymax = max(y, H$density)
hist(N, freq = FALSE, xlim = c(xmin, xmax), ylim = c())
