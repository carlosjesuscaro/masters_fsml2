# Title     : FSML II - Final exam
# Objective : Code for question 4
# Created by: carloscaro
# Created on: 2020-12-15

#Exercise 4
a = 5
b = 20
x = seq(0,100, 0.01)
# Density function
fabx = a*b^a/x^(a+1)*1*(x >= b)
hist(fabx, freq = FALSE, main = 'Density function fab')
# Distribution function
Fx = b^a/x^a*(x>=b)
plot(x,1-Fx,type='l',col = 'red', main = 'Distribution function Fab')

u = runif(100)
Y = b/(u)^(1/a)
hist(Y, type = 'l', col = 'red', main = 'Density function fx(u)')


