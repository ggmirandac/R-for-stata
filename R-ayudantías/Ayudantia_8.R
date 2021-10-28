# Pregunta 1

# a)

ans1a <- 1 - pweibull(5,scale=2, shape = 1)

# b)

ans1b <- ppois(3, lambda=2)

# c)

ans1c <- dnbinom(4-2,size = 2, prob = 0.082)

# d)

mux= 80

varx=40**2

dsp=40

v=(mux-dsp)/varx

k=(mux-dsp)*v

ans1d=1-pgamma(20,shape=k, rate=v)

# Pregunta 2

n = 25
p = 0.15
N = 100
m = 15

# a
 
ans2a= 1- pbinom(0,size=25,prob=0.15)

# b

ans2b = 1 - phyper(q=0,m=m,n=N-m,k=n)

# Pregunta 3 

# b

ans3b=1-pgamma(60, shape=3, rate=1/15)

# c

p8 <- 1-pgamma(8, shape=4,rate=1)

ans3c <- dgeom(4,prob=p8)


# Pregunta 4

lambda4=log(2)

zeta4=sqrt(log(1+0.6**2))

p<-1-plnorm(3,meanlog=lambda4,sdlog=zeta4)
q<-1-plnorm(4,meanlog=lambda4,sdlog=zeta4)
v4=17.2
