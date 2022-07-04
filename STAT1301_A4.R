# Q1 -----
B0 = -1.5
B1 = 0.3
var = 0.25
x = seq(0,1,0.01)
error = rnorm(101,  0, sd=sqrt(var))
sd(error)
y = B0+B1*x+error
model = lm(formula = y~x)
summary(model)
library(lattice)
xyplot(y~x)
plot(x,y)

# Q2 -----
d=c(68,137,315,405,700)
v=c(2.4,4.7,12.0,14.4,26.0)
model2 = lm(formula = v~d)
summary(model2)
m=mean(d)
n=mean(v)
sum((d-m)*(v-n))/sum((d-m)^2)
sum((d-m)^2)
# Q3 -----
babies = read.csv("Anova Data.csv")
names(babies)
summary(aov(ï..Baby~Group, data = babies))
babiesRefined = subset(babies, Group == 'A' | Group == 'B')
summary(aov(ï..Baby~Group, data = babiesRefined))
a = c(9,9.5,9.75, 10, 13, 9.5)
b = c(11, 10, 10, 11.75, 10.5, 15)
t.test(a, b, alternative = "two.sided", var.equal = TRUE)


