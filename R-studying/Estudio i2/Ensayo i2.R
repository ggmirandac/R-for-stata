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
