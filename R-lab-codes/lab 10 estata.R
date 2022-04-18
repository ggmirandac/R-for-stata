
# Laboratorio 10 ----------------------------------------------------------



# Teorema del Límite Central ----------------------------------------------

#Ejemplo 1: x1...xn~Exponencial (nu)

set.seed(1113)

rep = 1000

nu = 5
y_10=c()
y_50=c()
y_100=c()
y_500=c()
for(i in 1:rep){
  y_10[i]<-sum(rexp(n=10,rate=nu))
  y_50[i]<-sum(rexp(n=50,rate=nu))
  y_100[i]<-sum(rexp(n=100,rate=nu))
  y_500[i]<-sum(rexp(n=500,rate=nu))

}

mu <- 1/nu
sd <- sqrt(1/nu^2)

par(mfrow=c(2,2),cex=0.7)
hist(y_10,freq=F,main=expression(n==10),col="darkblue",border="white")

curve(dgamma(x,shape=10,rate=nu),lwd=3,col="orange",add=TRUE,lty=2)
curve(dnorm(x,mean=mu*10,sqrt(10)*sd),col="purple",add=T,lty=2,lwd=3)

hist(y_50,freq=F,main=expression(n==50),col="darkblue",border="white")

curve(dgamma(x,shape=50,rate=nu),lwd=3,col="orange",add=TRUE,lty=2)
curve(dnorm(x,mean=mu*50,sqrt(50)*sd),col="purple",add=T,lty=2,lwd=3)

hist(y_100,freq=F,main=expression(n==100),col="darkblue",border="white")

curve(dgamma(x,shape=100,rate=nu),lwd=3,col="orange",add=TRUE,lty=2)
curve(dnorm(x,mean=mu*100,sqrt(100)*sd),col="purple",add=T,lty=2,lwd=3)

hist(y_500,freq=F,main=expression(n==500),col="darkblue",border="white")

curve(dgamma(x,shape=500,rate=nu),lwd=3,col="orange",add=TRUE,lty=2)
curve(dnorm(x,mean=mu*500,sqrt(500)*sd),col="purple",add=T,lty=2,lwd=3)

#Ejemplo 1: x1...xn~poiss (nu)


set.seed(1113)

rep = 1000

lambda = 10

y_10=c()
y_50=c()
y_100=c()
y_500=c()

for(i in 1:rep){
  y_10[i]<-sum(rpois(n=10,lambda=lambda))
  y_50[i]<-sum(rpois(n=50,lambda=lambda))
  y_100[i]<-sum(rpois(n=100,lambda=lambda))
  y_500[i]<-sum(rpois(n=500,lambda=lambda))
  
} 

mu <- lambda
sd <- sqrt(lambda)

par(mfrow=c(2,2),cex=0.7)

hist(y_10,freq=F,main=expression(n==10),col="darkblue",border="white")

x<-seq(from=min(y_10),to=max(y_10))
points(x,dpois(x,lambda = 10*lambda),lwd=10,pch=16,col="orange")
curve(dnorm(x,mean=mu*10,sqrt(10)*sd),col="purple",add=T,lty=2,lwd=3)

hist(y_50,freq=F,main=expression(n==50),col="darkblue",border="white")

x<-seq(from=min(y_50),to=max(y_50))
points(x,dpois(x,lambda = 50*lambda),lwd=10,pch=16,col="orange")
curve(dnorm(x,mean=mu*50,sqrt(50)*sd),col="purple",add=T,lty=2,lwd=3)

hist(y_100,freq=F,main=expression(n==100),col="darkblue",border="white")

x<-seq(from=min(y_100),to=max(y_100))
points(x,dpois(x,lambda = 100*lambda),lwd=10,pch=16,col="orange")
curve(dnorm(x,mean=mu*100,sqrt(100)*sd),col="purple",add=T,lty=2,lwd=3)

hist(y_500,freq=F,main=expression(n==500),col="darkblue",border="white")

x<-seq(from=min(y_500),to=max(y_500))
points(x,dpois(x,lambda = 500*lambda),lwd=10,pch=16,col="orange")
curve(dnorm(x,mean=mu*500,sqrt(500)*sd),col="purple",add=T,lty=2,lwd=3)




# Estadísticos de orden ---------------------------------------------------


#Ejemplo: X_1,...X_n,~Exponencial (nu)
# Y_1=X(1)=min(X_1,...,X_n)~Exponencial(n*nu)
# El minimo de una n distribuciones~exp(nu) distribuye exp(n*nu), donde n son los n minimos
set.seed(1113)
n<-100
nu<-3
y<-vector("numeric")

rep <- 10000
for (i in 1:rep){
  x<-rexp(n=n,rate=nu)
  y[i]=min(x)
}

par(mfrow=c(1,1))
hist(y,freq=FALSE,col="darkblue",border="white")
curve(dexp(x,rate=nu*n),col="orange",lty=2,lwd=3,add=T)









