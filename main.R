# Title     : Class 1
# Objective : TODO
# Created by: carloscaro
# Created on: 2020-11-04

A = runif(1000)
H = hist(A, breaks = c(0,0.2,0.4,0.6,0.8,1), freq = FALSE)
H$counts
max(A)
min(A)

