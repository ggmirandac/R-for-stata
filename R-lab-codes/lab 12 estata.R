
# Laboratorio 12 ----------------------------------------------------------


n <- 30
N = 2000
b= 5
a<- 0
muestras <- matrix(runif(n*N, min = a, max = b),ncol=N, nrow =n)

estadisticoas <- apply(muestras, 2, max)

hist(estadisticoas,freq=F, main="Distribuci?n muestral del m?ximo \nde variables aleatorias Uniformes")
curve(n*x^(n-1)/(b^n),add=T)


# p de bernoulli

n <- 50
N <- 7000
p <- 0.3

muestras <- matrix(rbinom(n*N,size=1,prob = p),ncol=N,nrow=n)
estadisticos <- apply(muestras, 2 , mean)

hist(estadisticos, freq=F, main="distribución muestral de la meda de variables aleatorias Bernoulli")

curve(dnorm(x, mean=p, sd=sqrt(p*(1-p)/n)),add=T)


# lambda poisson

n<- 70
N<- 1000
lambda <- 15

muestras <- matrix(rpois(n*n, lambda = lambda),ncol=N,nrow = n)
estadis <- apply(muestras, 2, mean)

hist(estadis, freq=F, main="distribución muestral de la meda de variables aleatorias poisson")

curve(dnorm(x, mean=lambda, sd=sqrt(lambda/n)),add=T)



# Pruebas de Hipótesis ----------------------------------------------------

library(TeachingDemos)
set.seed(1113)
# test para la media con desviación estandar conocida

N <- 1000
mu<-55
sigma <- 11
X <- rnorm(n=N, mean = mu, sd=sigma)
alpha <- 0.05

summary(X)

mu0 <- 60

# H_0; mu=mu0 vs H_a: mu!=mu0

z.test(X, mu=mu0, sd=sigma, alternative = "two.sided", conf.level = 1-alpha)


# Como p-value < alpha, rechazo Ho, así la media es distinta a 60.

# H_0: mu <= mu0 vs H_a: mu>mu0

z.test(X, mu= mu0, sd=sigma, alternative = "greater", conf.level = 1-alpha)

# Como p-value > alpha, se tiene que no existe evidencia suficiente para rechazar H_0
# Concluyo que mi media es menor a 60

z.test(X, mu= mu0, sd=sigma, alternative = "less", conf.level = 1-alpha)

# Test para mu con sigma desconocido ###########3

alpha <- 0.05

summary(X)

mu0 <- 60

# H_0; mu=mu0 vs H_a: mu!=mu0

t.test(X, mu=mu0,  alternative = "two.sided", conf.level = 1-alpha)


# Como p-value < alpha, rechazo Ho, así la media es distinta a 60.

# H_0: mu <= mu0 vs H_a: mu>mu0

t.test(X, mu= mu0,alternative = "greater", conf.level = 1-alpha)

# Como p-value > alpha, se tiene que no existe evidencia suficiente para rechazar H_0
# Concluyo que mi media es menor a 60

t.test(X, mu= mu0, alternative = "less", conf.level = 1-alpha)



# Test para sigma con mu desconocido #######

sigma0=12
sd(X)
sigma.test(X, sigma=sigma0,  alternative = "two.sided", conf.level = 1-alpha)


# Como p-value < alpha, rechazo Ho, así la media es distinta a 60.

# H_0: mu <= mu0 vs H_a: mu>mu0

sigma.test(X, sigma= sigma0,alternative = "greater", conf.level = 1-alpha)

# Como p-value > alpha, se tiene que no existe evidencia suficiente para rechazar H_0
# Concluyo que mi media es menor a 12

sigma.test(X, sigma= sigma0, alternative = "less", conf.level = 1-alpha)

### En estos casos los test,  me confirman o no el valor de H0 a partir del contrario




# Test para la proporción -------------------------------------------------



#Aquí se hacen comparaciones con prop.test


set.seed(1113)
N <- 500
p <- 0.4
X <- rbinom(n=1, size=N, prob=p)
X
X/N

p0 <- 0.3

# H_0: p=p0 vs H_a: p!=p0
prop.test(x=X, n=N, p=p0, alternative = "two.sided",
          conf.level = 1-alpha, correct=FALSE)
# Como p-value > alpha no existe evidencia suficiente para rechazar H_0
# Concluyo que la proporci?n es igual a 0.3

# H_0: p<=p0 vs H_a: p>p0
prop.test(x=X, n=N, p=p0, alternative = "greater",
          conf.level = 1-alpha, correct=FALSE)
# Como p-value > alpha, no existe evidencia suficiente para rechazar H_0
# Concluyo que la proporci?n es menor a 0.3

# H_0: p>=p0 vs H_a: p<p0
prop.test(x=X, n=N, p=p0, alternative = "less",
          conf.level = 1-alpha, correct=FALSE)
# Como p-value > alpha, no existe evidencia suficiente para rechazar H_0
# Concluyo que la proporci?n es mayor a 0.3



# Bondad de Ajuste --------------------------------------------------------



