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
x = seq(-1.5, 0.01)
fx = 0*(x<0)+x^2/8*(x>=0)*(x<2)+(-x^3/8+x-1)*(x>=2)*(x<4)+1*(x>=4)

