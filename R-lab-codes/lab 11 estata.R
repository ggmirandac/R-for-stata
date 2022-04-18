
# Laboratorio 10 ----------------------------------------------------------

# Se puede utilizar la función optimize para optimizar funciones
# ejemplo 1

f <- function(x,a){
  (x-a)**2
}
xmin<-optimize(f,c(0,1),tol=0.00001,a=1/3)
xmax<-optimize(f,c(0,1),tol=0.00001,a=1/3,maximum = T)


# Compando optim minimizar funciones multivariadas
# ojo que maximizar h es lo mismo que minimizar -h
# ejemplo

fr <- function(x){
  x1 <- x[1]; x2 <- x[2]
  100 * (x2 - x1 * x1)**2 + (1 - x1)**2
}
optim(c(-1.2,1), fr) # Se optimiza fr a partir de los valores iniciales entregados


# fitdistr() command -------------------------------------------------------

# sirve para encontrar el estimador de momentos y el máximo verosimil de una función de una
# distribución unvariada, entre otros.

# fitdist(data (los datos a entregar),distr="" (la distribución de estos datos),method=""(el método 
# que puede ser por estimador de momentos mme y maximo verosimil mle),... )

#ejemplo
set.seed(1113)
x2 <- rnorm(n=250,mean=10,sd=4)
EM <- fitdist(data=x2, distr = "norm",method = "mme")
EM$estimate
EMV <- fitdist(data=x2, distr = "norm", method = "mle")
EMV$estimate



# Ejercicio ---------------------------------------------------------------

lambda <- 1.2
zeta <- 0.3
n <- 500
set.seed(1113)
Y <- rlnorm(n=n,meanlog=lambda,sdlog=zeta)
hist(Y, freq=F,border="white",col="darkblue",main="log-normal",ylab="densidad",xlab="muesta")
## estimación qqplot

x<-sort(Y)
N<-length(Y)
p <- 1:N/(N+1)
fit<- lm(log(x)~qnorm(p))
fit
lambda.qq <- fit$coefficients[1]
zeta.qq <- fit$coefficients[2]

# EM #

zeta.em <- sqrt(log(mean(Y^2))-2*log(mean(Y)));zeta.em
lambda.em <- log(mean(Y))-0.5*zeta.em^2;lambda.em

EM <- fitdist(data=Y,distr="lnorm",method="mme")
# maximizar log(prod(dlnorm()))

fit <- fitdist(data=Y, distr="lnorm", method = "mle")
fit$estimate

lambda.emv <- fit$estimate[1]
zeta.emv <- fit$estimate[2]

curve(dlnorm(x, meanlog=lambda, sdlog = zeta), col="orange",
      lty=1, add=TRUE, lwd=3)
curve(dlnorm(x, meanlog=lambda.qq, sdlog = zeta.qq), col="darkblue",
      lty=2, add=TRUE, lwd=3)
curve(dlnorm(x, meanlog=lambda.em, sdlog = zeta.em), col="darkred",
      lty=3, add=TRUE, lwd=3)
curve(dlnorm(x, meanlog=lambda.emv, sdlog = zeta.emv), col="darkgreen",
      lty=4, add=TRUE, lwd=3)
