###### i1-2021-00 --------------------------------------------------------------

# Pregunta 10

ENS<-rio::import("R-studying/Estudio i2/ENS.xlsx")

enspas<- ENS$PAS

x50 <- median(enspas)
x80 <- quantile(enspas,prob=0.8)
beta=5.6
eta=132.3

pweibull(150,shape=beta,scale=eta)

# Pregunta 11

glubasal <- ENS$GLUBASAL

media <- mean(glubasal)

x90 <- quantile(glubasal, prob=0.9)

sigma <- (x90-media)/(log(0.9/(1-0.9)))

1-plogis(110,location=media, scale=sigma)

# Pregunta 12

X = ENS$PESO
x50 = quantile(X, 0.50)
x75 = quantile(X, 0.75)
a = log(2)/x50
b = (log(exp(x75*a)-1)/log(0.75/0.25))^(-1)
x = 80
Fx = (exp(a*x)-1)^b/(1+(exp(a*x)-1)^b)
1-Fx

###### i1-2021-01 --------------------------------------------------------------

# Pregunta 9

zeta <- sqrt(log(1+0.25**2))

lambda <- log(150)-qnorm(0.15)*zeta

(1-plnorm(250,meanlog=lambda,sdlog=zeta)+plnorm(130,meanlog=lambda,sdlog=zeta))**4

# Pregunta 10

demanda_gas<-rio::import("R-studying/Estudio i2/Demanda_GAS.xlsx")

# 10.1

x_10.1<-dplyr::filter(demanda_gas,ESTACION=="spring" & (SEMANA_MES==4 | SEMANA_MES==5))$MIN

sobre14=0
for(i in 1:length(x_10.1)){
  if (x_10.1[i]>14){
    sobre14=sobre14+1
  }
}
a_10.1 <- sobre14/length(x_10.1)

x50_10.1 <- median(x_10.1)

x80_10.1 <- quantile(x_10.1,prob=0.80)

lambda_10.1 <- log(x50_10.1)

zeta_10.1 <- (log(x80_10.1)-lambda_10.1)/qnorm(0.8)

plnorm(14,meanlog=lambda_10.1, sdlog=zeta_10.1,lower.tail = FALSE)

sd_10.1 <- sd(x_10.1)
mean_10.1 <- mean(x_10.1)

nu_10.1 <- mean_10.1/(sd_10.1**2)
kappa_10.1 <- mean_10.1*nu_10.1

pgamma(14, shape= kappa_10.1, rate = nu_10.1,lower.tail = FALSE)

# 10.2

x_10.2<-dplyr::filter(demanda_gas,ESTACION=="summer" & (SEMANA_MES>=4))$DEMANDA

prob_empirica<-length(x_10.2[x_10.2>15000])/length(x_10.2)

cov_10.2<-sd(x_10.2)/mean(x_10.2)

zeta_10.2<-sqrt(log(1+cov_10.2**2))
lambda_10.2<-log(mean(x_10.2))-(zeta_10.2**2)/2

plnorm(15000,meanlog=lambda_10.2,sdlog=zeta_10.2,lower.tail = FALSE)


mu_10.2<-median(x_10.2)
sigma_10.2<-(quantile(x_10.2,prob=0.8)-mu_10.2)/qnorm(0.8)
pnorm(15000,mean=mu_10.2,sd=sigma_10.2,lower.tail = FALSE)

# 11.1

x_11.1 <- c(tapply(demanda_gas$PRECIO, INDEX = list(demanda_gas$AÑO, demanda_gas$MES), mean))

IQR(x_11.1, na.rm = TRUE)
median(x_11.1)
sd(x_11.1)/mean(x_11.1)
modeest::mlv(x_11.1, na.rm=TRUE)
quantile(x_11.1,prob=0.65,na.rm=TRUE)

# 11.2

x_11.2 <- c(tapply(demanda_gas$DEMANDA, INDEX = list(demanda_gas$AÑO, demanda_gas$MES),mean))
IQR(x_11.2, na.rm = TRUE)
mean(x_11.2, na.rm=TRUE)
sd(x_11.2)/mean(x_11.2)
modeest::mlv(x_11.2, na.rm=TRUE)
quantile(x_11.2,prob=0.65,na.rm=TRUE)


###### i1-2020-02--------------------------------------------------------------


# Pregunta 7

mu_7=60/(1+qnorm(2/3)*1/3)
sigma_7=mu_7/3
qnorm(1/4, mean=mu_7,sd=sigma_7,lower.tail = FALSE)

qnorm(1/6, mean=mu_7,sd=sigma_7,lower.tail = FALSE)

qnorm(1/5, mean=mu_7,sd=sigma_7,lower.tail = TRUE)

qnorm(1/3, mean=mu_7,sd=sigma_7,lower.tail = TRUE)

# Pregunta 8

zeta_8=sqrt(log(1+(1/3)**2))
lambda_8<-log(60)-qnorm(2/3)*zeta_8

qlnorm(1/5,meanlog=lambda_8,sdlog=zeta_8,lower.tail = FALSE)

media_8 <- 45
sd_8 <- 15

zeta_8.2 <- sqrt(log(1+(sd_8/media_8)**2))
lambda_8.2 <- log(media_8)-(zeta_8.2**2)/2

qlnorm(1/3,meanlog = lambda_8.2, sdlog = zeta_8.2)

# Pregunta 9

datai1 <- rio:: import("R-studying/Estudio i2/DataI1.xlsx")

### parte 1
sepmai<-dplyr::filter(datai1, Mes==9 & Comuna=="Maipú")$Energia

lambda_9 <- log(2)/median(sepmai)

qexp(0.45,rate=lambda_9)/quantile(sepmai,prob=0.45)

### parte 2

consuflori <- dplyr::filter(datai1, Comuna=="La Florida" & Mes==5)$Energia

lambda_9.2<-1/mean(consuflori)

qexp(0.6,rate=lambda_9.2)/quantile(consuflori,prob=0.6)

## de aqui en adelante todo es lo mismo

# Pregunta 10

consumo_2019<- datai1$Energia

x25_10 <- quantile(consumo_2019, prob=0.25)

x50_10 <- median(consumo_2019)

lambda_10 <- log(x50_10)

zeta_10 <- (log(x25_10)-lambda_10)/qnorm(0.25)

qlnorm(0.75,meanlog=lambda_10,sdlog=zeta_10)-qlnorm(0.25,meanlog=lambda_10,sdlog=zeta_10)

###### i1-2020-01--------------------------------------------------------------

# Pregunta 11

4-qnorm(1-84.13/100)*0.2


# i1-2018-01 --------------------------------------------------------------

# Pregunta 1

# a

mayor5 <- pweibull(5,scale=2, shape = 1, lower.tail = FALSE)

# b

nu <- 6/24
t <- 8
ppois(3,lambda = nu*t)

# c 

dnbinom(4-2,size=2,prob=mayor5)

# d

1-pgamma(20,shape=1,rate=1/40)

# Pregunta 3

mu_in <- 720
sigma_in <- 240

mu_deu <- 504
sigma_deu <- 160

x20_ing <- qnorm(0.20,mean=mu_in,sd=sigma_in)

p20_deu <- pnorm(x20_ing, mean=mu_deu,sd=sigma_deu, lower.tail = FALSE)

c<- ((mu_deu/sigma_deu)+(mu_in/sigma_in))/(1/sigma_in + 1/sigma_deu)

zeta_deu <- sqrt(log(1+(sigma_deu/mu_deu)**2))
lambda_deu <- log(mu_deu)-0.5*(zeta_deu**2)

zeta_in <- sqrt(log(1+(sigma_in/mu_in)**2))
lambda_in <- log(mu_in)-0.5*(zeta_in**2)

c2 <- ((lambda_deu/zeta_deu)+(lambda_in/zeta_in))/(1/zeta_in + 1/zeta_deu)
exp(c2)


# i2-2021-01 --------------------------------------------------------------

# Pregunta 1

nu_1 <- 4.7974

beta_1<- 10.603

p_s5 <- 1 - pweibull(5,shape=beta_1,scale=nu_1)

p_a <- 1-pbinom(2,size=10, prob=p_s5)

# Pregunta 3

m <- 15

q <- 5/100

1-pbinom(1,size=m-1,prob=q)

# Pregunta 5

eta_t_5<-10

1-ppois(9,lambda=eta_t_5)

# Pregunta 6

kappa_6 <- (2/0.9)**2
nu_6 <- (14/kappa_6)**(-1)

1-pgamma(20,shape=kappa_6,rate=nu_6)

# Pregunta 7

mu =  0.52
delta = 0.96

sd_7 <- mu*delta

sigma <- (sd_7*sqrt(3))/pi

1-plogis(1,location=mu, scale=sigma)

# Pregunta 8.1

ens_muestra <- rio::import("R-studying/Estudio i2/ENS_Muestra.xlsx")

datos <- dplyr::filter(ens_muestra, SEXO=="Femenino")[c("COLESTEROL","IMC")]

#a)
mean(datos$COLESTEROL>180 & 20<datos$IMC & datos$IMC<30)

#b)

sigma_8=cov(datos) # matriz de covarianza
mu_8 = apply(datos, 2,mean) # media de variables

mvtnorm:: pmvnorm(lower=c(180,20),upper=c(Inf,30),mean=mu_8,sigma=sigma_8)

# Pregunta 8.2

datos_2 <- dplyr::filter(ens_muestra, SEXO=="Masculino")[c("COLESTEROL","IMC")]
mean(datos_2$COLESTEROL>170 & 25<datos_2$IMC & datos_2$IMC<30)

sigma_8.2 <- cov(datos_2)
mu_8.2 <- apply(datos_2,2,mean)
mvtnorm::pmvnorm(lower = c(170,25),upper = c(Inf,30),mean=mu_8.2,sigma=sigma_8.2)


# i2-2021-00 --------------------------------------------------------------

# Pregunta 1

x75 <- 650

mu <- x75 / ( 1 + qnorm(0.75)*0.25)
sigma <- 0.25 * mu

1-pnorm(700,mean=mu,sd=sigma)

 # Pregunta 2

pbinom(12,size=35,prob=0.45)

# Pregunta 3

N=1000
m=80
n=20
# Sin reemplazo
1 - phyper(2, m=m,n=N-m, k=n)
# Con reemplazo
1 - pbinom(2,size=20,prob=0.08)

# Pregunta 4

1-ppois(9,lambda = ((1/0.12)**2)*0.08  )


# Pregunta 5

mu_T <- 4
sigma_T <- 2

zeta_T <- sqrt(log(1+(sigma_T/mu_T)**2))
lambda_T <- log(mu_T)-0.5*zeta_T**2

p_7 <- 1-plnorm(7,meanlog = lambda_T, sdlog = zeta_T)

n=10

pbinom(2, size = 10, prob = p_7)

# Pregunta 10

data_base <- rio::import("R-studying/Estudio i2/IngresoPlanta.xlsx")


grasa <- data_base$GRASA

mean_10 <- mean(grasa)

delta <- sd(grasa)/mean_10

k<-1/delta**2
nu <- k/mean_10
iqr_teo=qgamma(0.75, shape=k, rate=nu) -  qgamma(0.25, shape=k, rate=nu)

(iqr_teo / IQR(grasa))-1

# Pregunta 11

hg <- data_base$HG

x20 <- quantile(hg,prob=0.2)
x80 <- quantile(hg, prob=0.8)

sigma <- (x80-x20)/(qnorm(0.8)-qnorm(0.2))

mu <- x20-sigma * qnorm(0.2)

1-pnorm(3,mean=mu,sd=sigma)

# Pregunta 12

peso <- data_base$PESO

cov_peso_hg <- cov(cbind(hg,peso)) # para que cov me de la matriz debe ser una tabla de dos columnas

mvtnorm::pmvnorm(lower = c(3.0,4.5), upper = c(Inf,Inf), mean=c(mean(hg),mean(peso)),sigma=cov_peso_hg )



# i2-2020-02 --------------------------------------------------------------

# Pregunta 1

x=50
r=6
p=0.07
pnbinom(x-r,size=r,prob = p)

# Pregunta 2

N=14500000
m=N*0.75
n=350

1 - phyper(n*0.8,m=m,n=N-m,k=n)

# Pregunta 3

eta <- 23.8791
beta <- 1.43138

1-pweibull(45,shape=beta,scale=eta)

mu <- 2.85189
sigma <- 0.5
1-plogis(log(45), location=mu,scale=sigma)

# Pregunta 4

nu=5
t=2
1-ppois(12,lambda=nu*t)

# Pregunta 6

library(readxl)
Pokedex <- read_excel("R-studying/Estudio i2/Pokedex.xlsx")

xp <- sort(Pokedex$velocidad)
N <- length(xp)
p <- 1:N/(N+1)

ajuste_weibull <- lm(log(xp)~log(-log(1-p)))
ajuste_logistica <- lm(xp~qlogis(p))
ajuste_normal <- lm(xp ~ qnorm(p))

summary(ajuste_weibull)$adj.r.square
summary(ajuste_logistica)$adj.r.square
summary(ajuste_normal)$adj.r.square
 
# el mejor es weibull

intercepto <- ajuste_weibull$coefficients[1]
pendiente <- ajuste_weibull$coefficients[2]
eta<- exp(intercepto)
beta <- 1/pendiente
iqr <- qweibull(0.75, shape=beta, scale= eta)-qweibull(0.25, shape=beta, scale= eta)

# Con HP

hp <- sort(Pokedex$HP)

N_hp <- length(hp)

p <- 1:N_hp / (N_hp+1)

ajus_weibull <- lm(log(hp) ~ log(-log(1-p)))
ajus_logistica <- lm(hp ~ qlogis(p))
ajus_loglogistica <- lm(log(hp) ~ qlogis(p))

summary(ajus_logistica)$adj.r.square                   
summary(ajus_loglogistica)$adj.r.square                   
summary(ajus_weibull)$adj.r.square                   

# la mejor es logistica

1-plogis(100, location = ajus_logistica$coefficients[1], scale = ajus_logistica$coefficients[2])

# Con defensa

defensa <- sort(Pokedex $defensa)

N_defensa <- length(defensa)

p_defensa <- 1:N_defensa / (N_defensa + 1)

aj_weibull <- lm(log(defensa) ~ log(-log(1-p_defensa)))
aj_loglogistica <- lm(log(defensa) ~ qlogis(p_defensa))
aj_lognormal <- lm(log(defensa)~qnorm(p_defensa))

summary(aj_weibull)$adj.r.square
summary(aj_loglogistica)$adj.r.square
summary(aj_lognormal)$adj.r.square

# la mejor es loglogistica

plogis(log(75), location = aj_loglogistica$coefficients[1], scale = aj_loglogistica$coefficients[2])

# i2-2020-01 --------------------------------------------------------------

# Pregunta 1

x30 = 6
mu = 10

sigma <- (x30-mu)/qnorm(0.3)

1-pnorm(24,mean=mu,sd=sigma)

# Pregunta 2

x50 <- 5
delta <- 0.33

lambda <- log(x50)
zeta <- sqrt(log(1+delta**2))

plnorm(8, meanlog=lambda, sdlog=zeta)

# Pregunta 5

n <- 10

p <- 0.09*(1-0.12)+(1-0.11)*0.12

1-pbinom(2,size=10,prob=p)

# Pregunta 6

N = 44
m = 4
n = 6-1

1-phyper(0,m=m, n=N-m,k=n)

# Pregunta 7

a <- 6 
b <- 3
p <- 0.121

pgeom(a-b-1,prob=p)

# Pregunta 8

lambda <- 43
nu <- 43/24

1- ppois(3,lambda=nu)

# Pregunta 10

muy=3.9
mux=3.9
sigmay<-0.6
sigmax<-0.6
rho<-0.4

mu_x_y <- muy + rho * (sigmay/sigmax)*(3.6-mux)
sigma_x_y <- sigmay * sqrt(1-rho**2)

1-pnorm(4,mean = mu_x_y,sd=sigma_x_y)

# Pregunta ultima

k=13
l=21

sqrt((l+k+1)/(l*k))



# Ensayo 1-i2 -------------------------------------------------------------

#Pregunta 1

eta <- 4.7974
beta <- 10.603

p <- 1-pweibull(5, shape=beta, scale=eta)

q <- 1-pbinom(2, size = 10,prob=p)
print(q)

# Pregunta 2

1/0.15

# Pregunta 3

n=15

p=0.05

1-pbinom(1, size=n-1,prob=p)

# Pregunta 5

1-ppois(9,lambda=(1/7)*60)

# Pregunta 6

k=(2/0.9)**2
nu <- k/14
1-pgamma(20,shape=k,rate=nu)

# Pregunta 7

mu = 0.52
delta <- 0.96
sigma_x <- mu*delta

Sigma <- (sigma_x*sqrt(3))/pi

1-plogis(1,location=mu,scale=Sigma)



# i2-2019-02 --------------------------------------------------------------

x50 <- 2
delta <- 0.6

lambda <- log (x50)
zeta <- sqrt(log(1+delta**2))

p_3 <- 1-plnorm(3,meanlog=lambda,sdlog=zeta)
p_4 <- 1-plnorm(4,meanlog=lambda,sdlog=zeta)

nu = 4/p_3

lambda_2 <- nu*p_4

2/lambda_2


# Ensayo 6 ----------------------------------------------------------------

# Pregunta 1
k=6
p=0.07
x=50

pnbinom(x-k,size=k,prob=p)

# Pregunta 2

N <- 14500000
n <- 350
m <- N*0.75

1-phyper(n*0.8, m=m, n=N-m, k=n)

# Pregunta 3

mu_3 <- 2.85189
sigma_3 <- 0.5

1-plogis(log(45),location = mu_3, scale = sigma_3)

# Pregunta 4

nu <- 5
t <- 2
k <- 2

1- ppois(12, lambda = 5*2)


# i2-2019-00 --------------------------------------------------------------

# Pregunta 1b

N=5000
n=50
m=1250
dhyper(50*0.24,m=m,n=N-m,k=n)

choose(1250,12)*choose(3750,38)/choose(5000,50)

# i2-2018-00 --------------------------------------------------------------

# Pregunta 1

mu_x <- 1.5
delta_x <- 0.2

zeta_x <- sqrt(log(1+delta_x**2))
lambda_x <- log(mu_x)-0.5*zeta_x**2

p_menos1 <- plnorm(1, meanlog = lambda_x, sdlog = zeta_x)

dgeom(3,prob = p_menos1)

p <- 0.5

dbinom(5,size=5,prob=p)

3/0.4


# i1-2017-00 --------------------------------------------------------------

# Pregunta 1

plnorm(2800-700,meanlog = 7.42742, sdlog = 0.148065)

qlnorm( 0.75,meanlog = 7.42742, sdlog = 0.148065)+700

# Pregunta 3

alpha <- 2.95308
nu <- 0.338629

p_s10 <- 1-pexp(10-alpha, rate=nu)

1-pgeom(10-1,prob=p_s10)


# Ejercicios Clase 2/11 ---------------------------------------------------


mx <- 3.9
my <- 3.9
sx <- 0.7
sy <- 0.7
rho <- 0.4

# P(Y>4| X=3.8)
my_x <- my + rho * (sx/sy)*(3.8-mx)
sy_x <- sy*sqrt(1-rho**2)

1-pnorm(4,mean = my_x, sd = sy_x)

# P(3.8 < X < 4.5 , 4 < Y < 5)

m_vector <- c(mx,my)
cov_matrix <- matrix(c(sx**2,rho*sx*sy,rho*sx*sy,sy**2),2,2)
mvtnorm:: pmvnorm(lower = c(3.8,4),upper = c(4.5,5),mean = m_vector, sigma = cov_matrix)


