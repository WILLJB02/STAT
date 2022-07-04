# Q1 -----
data = c(8,6,6,4,5,6,11,8,9)
mean(data)
sd(data)
median(data)
IQR(data)
# Q4 ----
1-pbinom(499,size=1500,prob=0.3)
binom.test(500, 1500, 0.3, alternative = "greater" )


# Q5 ----
XABar = 13.3
nA = 25
SA = 1.4
XBBar = 12.4
nB = 21
SB = 2.0

SP = sqrt(((nA-1)*SA^2+(nB-1)*SB^2)/(nA+nB-2))

t = (XABar -XBBar)/(SP*sqrt(1/nA+1/nB))
dfvalue = nA+nB-2

2*(1-pt(t, df = dfvalue))

# Q6 ----
XABar = 4.6
XBBar = 4.2
nA = 16
nB = 26
SA = 1.1
SB = 0.9
a = SA^2/nA
b = SB^2/nB
t = (XABar -XBBar)/(sqrt(a+b))
dfvalue = (a + b)^2/(a^2/(nA-1) + b^2/(nB-1))
1-pt(t, df = dfvalue)

