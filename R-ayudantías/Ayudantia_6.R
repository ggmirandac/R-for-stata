# Pregunta 1

zeta1=log(3050/2270)/(qnorm(0.75)-qnorm(0.25))
lamba1=log(2270)-zeta1*qnorm(0.25)

# a
prob1a=plnorm(1520, meanlog = lamba1 , sdlog = zeta1) - plnorm(1300, meanlog = lamba1 , sdlog = zeta1)

#
distan1b = qlnorm(0.1, meanlog = lamba1, sdlog=zeta1)-1520

# Pregunta 2

n=4
p=1/3

# b
dbinom(3,size=4,prob=1/3)+dbinom(4,size=4,prob=1/3)

# c

# Pregunta 3

ENS <- rio::import("R-ayudantías/ENS.xlsx")
coles<-ENS$HDL

#### variables

# a
cov=sd(coles)/mean(coles)

zeta3=sqrt(log(1+cov^2))
lambda3=log(mean(coles))-1/2 * zeta3^2

# b 

hist(coles, freq = F, 50)
curve(dlnorm(x,meanlog=lambda3,sdlog = zeta3),add=T,lwd=3)

# c 

p <- plnorm(40,meanlog=lambda3,sdlog = zeta3)

# d

1 - pbinom(5,6,1-p)

1 - pbinom(5,10,1-p)

1 - pbinom(5,11,1-p)
