##### Laboratorio 6 ##### ------------------------------------------------------

### función lm() ###-----------------------------------------------------------

# La función lm() es una función que sirve para crear modelos de regresión lineal
# para este laboratorio se utilizará para estimar el intercepto y la pendiente
# de una recta que prediga y con respecto a x. Es decir: y=b0+b1x. 
# Esta función se usa como lm(y~x)

# Por lo general los modelos de distribución de una variable aleatoria son desconocidos.
# con excepción de ciertos casos, por ejemplo:
# Cumple vs No Cumple -> Bernoulli
# Número de eventos en un periodo -> Poisson
# Tiempo de duración o espera -> Exponencial
# Suma de eventos individuales -> Normal
# Condiciones extremas -> Valor extremo

### Construcción de gráficos de probabilidad ### -------------------------------

# Normal ------------------------------------------------------------------------

n<-300
mu<- 10
sigma<-2

set.seed(1113)

y<-rnorm(n,mean=mu,sd=sigma)


y<- sort(y) # ordenarlo de menor a mayor

N<-length(y) # cantidad de datos
m<-1:N # secuencia de numeros de 1 hasta N
x<-m/(N+1) # probabilidades empíricas a cada punto

p <- seq(from=0,to=1,by=0.05) # percentiles

plot(qnorm(x),y,xaxt="n",ylab="valores de Y", xlab="probabilidad acumulada",
     bty="n",lwd=2)  # Simulamos una normal
axis(1,at=qnorm(p),label=p,las=1,lwd=0.5)

modelo<-lm(y~qnorm(x))
valores<-lm(y~qnorm(x))$coefficients
hat.mu<-valores[1]
hat.sigma<-valores[2]

abline(a=hat.mu,b=hat.sigma,lwd=2,col="blue") #a=intercepto y b=pendiente

# ajuste de distribución de probabilidad
hist(y,freq=FALSE,col="darkred",border="white",las=1,main="Histograma de ajuste normal")
curve(dnorm(x,mean=hat.mu,sd=hat.sigma),add=T,lwd=2,lty=2,col="blue")

# Log-Normal -------------------------------------------------------------------
set.seed(1113)
n<-300
lambda<- 2
zeta<-0.4

y<-rlnorm(n,meanlog=lambda,sdlog=zeta)


y<- sort(y) # ordenarlo de menor a mayor

N<-length(y) # cantidad de datos
m<-1:N # secuencia de numeros de 1 hasta N
x<-m/(N+1) # probabilidades empíricas a cada punto

p <- seq(from=0,to=1,by=0.05) # percentiles

plot(qnorm(x),log(y),xaxt="n",ylab="valores de Y", xlab="probabilidad acumulada",
     bty="n",lwd=2)  # Simulamos una normal
axis(1,at=qnorm(p),label=p,las=1,lwd=0.5)

modelo<-lm(log(y)~qnorm(x))
valores<-lm(log(y)~qnorm(x))$coefficients
hat.lambda<-valores[1]
hat.zeta<-valores[2]

abline(a=hat.lambda,b=hat.zeta,lwd=2,col="blue") #a=intercepto y b=pendiente

# ajuste de distribución de probabilidad
hist(y,freq=FALSE,col="darkred",border="white",las=1,main="Histograma de ajuste normal")
curve(dlnorm(x,meanlog=hat.lambda,sdlog=hat.zeta),add=T,lwd=2,lty=2,col="blue")

# Gamma con ajuste Normal y Log-Normal------------------------------------------
k <-10
nu <- 0.1
n <-300
y <- rgamma(n, rate = nu, shape=k)

hist(y, freq = F, col="darkred",border="white",main="distribución gamma", las=1)

## qqplot Normal

par(mfrow=c(1,2),cex=0.8)

y<- sort(y) # ordenarlo de menor a mayor

N<-length(y) # cantidad de datos
m<-1:N # secuencia de numeros de 1 hasta N
x<-m/(N+1) # probabilidades empíricas a cada punto

p <- seq(from=0,to=1,by=0.05) # percentiles

plot(qnorm(x),y,xaxt="n",ylab="valores de Y", xlab="probabilidad acumulada",
     bty="n",lwd=2,main="Distribución Normal")  # Simulamos una normal
axis(1,at=qnorm(p),label=p,las=1,lwd=0.5)

modelo<-lm(y~qnorm(x))
valores<-lm(y~qnorm(x))$coefficients
hat.mu<-valores[1]
hat.sigma<-valores[2]

abline(a=hat.mu,b=hat.sigma,lwd=2,col="blue") #a=intercepto y b=pendiente


## qqplot log-normal

plot(qnorm(x),log(y),xaxt="n",ylab="valores de Y", xlab="probabilidad acumulada",
     bty="n",lwd=2)  # Simulamos una normal
axis(1,at=qnorm(p),label=p,las=1,lwd=0.5)

modelo<-lm(log(y)~qnorm(x))
valores<-lm(log(y)~qnorm(x))$coefficients
hat.lambda<-valores[1]
hat.zeta<-valores[2]

abline(a=hat.lambda,b=hat.zeta,lwd=2,col="blue") #a=intercepto y b=pendiente

# Histograma -------------------------------------------------------------------
par(mfrow=c(1,1),cex=0.8)

hist(y,freq=FALSE,col="darkred",border="white",las=1,main="Histograma de ajuste normal")
curve(dnorm(x,mean=hat.mu,sd=hat.sigma),add=T,lwd=2,lty=2,col="green")
curve(dlnorm(x,meanlog=hat.lambda,sdlog=hat.zeta),add=T,lwd=2,lty=2,col="blue")
legend("topright",col=c("blue","green"),lwd=c(2,2),c("Normal","Log-Normal"),cex=1,
       bty="n",lty=c(2,2))

### Weibull --------------------------------------------------------------------
set.seed(1113)
n <- 300
beta <- 0.8
eta <- 0.2 
X <- rweibull(n, shape=beta, scale=eta)

QQ.Weibull = function(y){
  x=sort(y)
  N=length(y)
  p=(1:N)/(N+1)
  plot(log(x)~log(-log(1-p)),
       pch = 20, col = "darkblue", bty = "n", las = 1,
       main = expression("QQ-Weibull"),
       ylab = expression(log(x[p])),
       xlab = expression(log(-log(1-p))))
  abline(lm(log(x) ~ log(-log(1-p))), lwd = 3, col = "darkorange")
  aux = lm(log(x) ~ log(-log(1-p)))
  aux
}


eta = as.numeric(exp(QQ.Weibull(X)$coef[1]))
beta = as.numeric(1/QQ.Weibull(X)$coef[2])

hist(X,freq = F,col="darkred",border="white",main="Histograma de Weibull",las=1)
curve(dweibull(x,shape=beta,scale=eta),lwd=3,col="darkblue",add=TRUE)
