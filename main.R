# Title     : Class 1
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-11-04

A = runif(1000)
H = hist(A, breaks = c(0,0.2,0.4,0.6,0.8,1), freq = FALSE)
H$counts
max(A)
min(A)
# The area in the density is always equal to 1
# The density histogram allows us to identify the family distribution

# Loading datasets
H1 <- hist(data1.txt, freq = FALSE)
