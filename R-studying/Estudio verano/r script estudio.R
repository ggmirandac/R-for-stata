
# Ayudantia 5 -------------------------------------------------------------
# problema 1

media <- 30
sigma <- 0.4 * 30

pnorm(40, mean = media, sd = sigma, lower.tail = FALSE)
qnorm(0.4,mean=media, sd = sigma)
pnorm(36, mean = media,sd = sigma)-pnorm(24, mean= media, sd = sigma)
qnorm(2/3)

# problema 3 

mu = 60/(1+ qnorm(2/3)*1/3)

sd = 1/3 * mu

qnorm(3/4, mean = mu , sd = sd)


# Ayudantia 6 -------------------------------------------------------------

# Problema 1

zeta <- log(3050/2270)/(qnorm(0.75)-qnorm(0.25))

lambda <- log(2270)-zeta*qnorm(0.25)

plnorm(1520, meanlog= lambda, sdlog=zeta)-plnorm(1300, meanlog= lambda, sdlog=zeta)

qlnorm(0.1,meanlog=lambda, sdlog=zeta)-1520

# Problema 2

1-pbinom(2,size=4,prob=1/3)

# Problema 3

library(readxl)
ENS <- read_excel("R-studying/Estudio verano/ENS.xlsx")

colesterol <- ENS$HDL

lamdba<-log(median(colesterol))  
cov <- sd(colesterol)/mean(colesterol)
zeta <- sqrt(log(1+cov^2))

hist(colesterol, freq = F, 50)

curve(dlnorm(x,meanlog=lambda, sdlog=zeta),add=T, lwd=3)


plnorm(40, meanlog=lambda, sdlog= zeta)

p=1-plnorm(40, meanlog=lambda, sdlog= zeta)

1-pbinom(5,size=6,p)
1-pbinom(5,size=11,p)


# Ayudantia 7 -------------------------------------------------------------

# Problema 2

v <- (1.7*10^6)/(3*24*60*60)

9/v

zeta <- sqrt(log(1+0.35^2))
lambda <- log(233/1.7)-0.5*zeta^2

p <- 1-plnorm(250, meanlog = lambda, sdlog = zeta)

ppois(2,lambda = v*p*10)


# Ayudantia 8 -------------------------------------------------------------

# Problema 1

p <- 1- pweibull(5, scale = 2, shape = 1)

1- pgamma(8, shape = 4, rate = 6/24)

dnbinom(x= 4-2, size= 2, prob= p)

1-pgamma(60-40,shape=1,rate=1/40)

# Problema 2

1- pbinom(0, size = 25, prob= 0.15)
1- phyper(0, m=15,n=100-15,k=25)

# Problema 3

1- pgamma(60, shape = 3, rate = 1/15)

p<- 1-pgamma(8,shape=4,rate=1)

1-pgeom(3-1,prob=p)
# Problema 4
zeta <- sqrt(log(1+0.6^2))

1-plnorm(4,meanlog = log(2), sdlog = zeta)



# Ayudantia 9 -------------------------------------------------------------

# Problema 4

#a)

Ens <- rio::import("R-studying/Estudio verano/ENS_Muestra.xlsx")
colesterol <- dplyr::filter(Ens, SEXO=="Femenino")$COLESTEROL
imc <- dplyr::filter(Ens, SEXO=="Femenino")$IMC


mean(colesterol > 180 & imc<30 & imc>20)

#b)

sigma <- cov(cbind(colesterol,imc))
mu <- c(mean(colesterol),mean(imc))

p <- mvtnorm::pmvnorm(lower = c(180,20),upper = c(Inf,30),mean=mu, sigma=sigma)


# Ayudantia 11 ------------------------------------------------------------

1-ppois(1000,lambda = 1000)

muz<- 50-45
sdz<- sqrt(10**2 + 15**2)

pnorm(0,mean=muz,sd=sdz)

pnorm(60,mean = 50, sd=10)*pnorm(60, mean = 45, sd = 15)

muw <- 50+45+80
sdw <- sqrt(10**2 + 15**2 + 25**2)

pnorm(200, mean = muw, sd= sdw)



# Ayudantia 12 ------------------------------------------------------------

mu_x <- 5000
cov <- 0.3
sigma_x <- cov*mu_x

1-pnorm(4950, mean =  mu_x, sd = sigma_x/sqrt(1345))

pnorm(7000000, mean = mu_x*1345, sd= sigma_x*sqrt(1345))-pnorm(6900000, mean = mu_x*1345, sd= sigma_x*sqrt(1345))


# Ayudantía 13 ------------------------------------------------------------

((qnorm(1-0.05/2)*1800)/600)**2


# Ayudantia 14 ------------------------------------------------------------

# Problema 4

z0 <- (30/120-0.3)/sqrt((0.3*(1-0.3))/(120))

p.value<-pnorm(z0)

# Problema 5

pnorm(130, mean = 60*2, sd = sqrt(60)*1)

# Problema 7

abalon<-rio::import("R-studying/Estudio verano/Abalon.xlsx")

fit1 <- lm(abalon$pesocu~abalon$largo,data = abalon)
fit2 <-lm(abalon$pesocu~abalon$diametro,data = abalon)
fit3 <- lm(abalon$pesocu~abalon$alto,data = abalon)

summary(fit1)$r.square # el mejor estimador
summary(fit2)$r.square
summary(fit3)$r.square

summary(fit1)$coef[2]*5


fit4<- lm(abalon$pesocu~abalon$largo+abalon$alto,data=abalon)
fit5 <- lm(abalon$pesocu~abalon$largo+abalon$alto+abalon$diametro,data=abalon)

anova(fit4,fit5)$F


# Examen-2021-1 -----------------------------------------------------------

banda_ancha<-rio::import("R-studying/Estudio verano/BandaAncha.xlsx")

M<-dplyr::filter(banda_ancha, Operador=="M")$Velocidad

log_nor<-fitdistrplus::fitdist(M, method=("mme"),distr = "lnorm")$estimate

ks.test(M,"plnorm", meanlog=log_nor[1],sdlog=log_nor[2])$p.value

gamma<-fitdistrplus::fitdist(M, method=("mme"),distr = "gamma")$estimate

ks.test(M,"pgamma", shape=gamma[1],rate=gamma[2])$p.value

# se usa log-normal, mayor valor-p

# 9

W <- dplyr::filter(banda_ancha, Operador=="W")$Velocidad

weibull <- fitdistrplus::fitdist(W, distr = "weibull", method="mle")
params_weibull <- weibull$estimate

x_quantile<-quantile(W, probs = seq(0,1,0.2))
x_quantile[1]<-0
x_quantile[6]<-Inf
x_quantile

p<-diff(pweibull(x_quantile,shape = params_weibull[1],scale = params_weibull[2])) # las probabilidades de cada elemento

O<- hist(W, plot = F, breaks = x_quantile,right = T)$count # lo observado

x2<-chisq.test(x=O, p=p)$statistic # p se entrega con las probabilidades de cada intervalo de la distribución a la que se quiere acoplar

1 - pchisq(x2, df= 5-2-1)

# 10

means <- aggregate(Velocidad ~ Ciudad, data = banda_ancha, FUN = mean)[,2]

xp <- sort(means)

N <- length(means)

p<- 1:N/(N+1)

par<-lm(log(xp)~qnorm(p))$coef

# 11

#m H0: mu = 10, Ha: mu>10

mu0<- 10

# gamma 4, v

nu0 <- 4/mu0

sigma <- sqrt(4/nu0^2) 

TeachingDemos::z.test(x=means, mu=mu0, stdev = sigma, alternative = "greater")$p.value

# 12


# Examen 2020-2 -----------------------------------------------------------

# 1
((qnorm(0.95)*sqrt(0.15*0.85))/0.04)**2

# 2

pokedex <- rio::import("R-studying/Estudio verano/Pokedex.xlsx")

Hp_no_legen<- dplyr::filter(pokedex, legendario==FALSE)$HP



# i3-2021-1 ---------------------------------------------------------------
library(readxl)
ens_muestra<- read_excel("R-studying/Estudio verano/ENS_Muestra.xlsx")
ens_muestra_masc<- dplyr::filter(ens_muestra, SEXO=="Masculino")

cole<- ens_muestra_masc$COLESTEROL
hdl <- ens_muestra_masc$HDL

z<-4*hdl-cole
pnorm(0,mean = mean(z),sd = sd(z))

# 8

prototipo <- read_excel("R-studying/Estudio verano/Prototipo.xlsx")

new_prot<- dplyr::filter(prototipo, PROTOTIPO=="nuevo")$RENDIMIENTO
  
sign<-0.1

mu0<-23

t.test(x=new_prot, mu=mu0, alternative = "greater")$p.value

#9

stand_prot<- dplyr::filter(prototipo, PROTOTIPO=="estandar")$RENDIMIENTO

sigma0<-0.57

TeachingDemos::sigma.test(x=stand_prot, sigma = sigma0, alternative = "less")$p.value

#10

mean(new_prot>23.5)

norm<-fitdistrplus::fitdist(new_prot, distr = "norm", method = "mme")$estimate
lnorm<- fitdistrplus::fitdist(new_prot, distr = "lnorm", method = "mme")$estimate
gamma<-fitdistrplus::fitdist(new_prot, distr = "gamma", method = "mme")$estimate

1-pnorm(23.5, mean = norm[1],sd = norm[2])
1-plnorm(23.5, meanlog=lnorm[1],sdlog = lnorm[2])
1-pgamma(23.5, shape = gamma[1], rate = gamma[2])

# 11
k=2177
mu0=21.4
n=length(stand_prot)

z0<-(mean(stand_prot)-mu0)/(mu0/sqrt(n*k))

n=124
x=50
p0=1/3

prop.test(x=50, n=124, p = 1/3, alternative="greater", correct=F)$p.value


# i3-2021-0 ---------------------------------------------------------------

set.seed(1113)
prob<-c()
y=c()
for(j in 1:1000){
for(i in 1:1000){
  n=30
  vector<- rbinom(n, size= 1 , prob = 1/2)
  vector[vector==0]<- -7
  vector[vector==1]<- 10
  utilidad<-sum(vector)
  y[i]<-utilidad
  
}

prob[j]<-mean(y>50)
}
mean(prob)

sismos<-readxl::read_excel("R-studying/Estudio verano/Sismos2021.xlsx")
sismos.t<- readxl::read_excel("R-studying/Estudio verano/Sismos2021.xlsx")$Tiempo

exp<-fitdistrplus::fitdist(sismos.t, distr = "exp", method = "mle")$estimate
gamma<-fitdistrplus::fitdist(sismos.t, distr = "gamma", method = "mle")$estimate
weibull<-fitdistrplus::fitdist(sismos.t, distr = "weibull", method = "mle")$estimate

1-pexp(20, rate = exp[1])
1-pgamma(20, shape= gamma[1],rate=gamma[2])
1-pweibull(20, shape=weibull[1],scale=weibull[2])

mean(sismos.t>20)
# mejor ajuste gamma

sismos.m<-sismos$Magnitud
alpha<-min(sismos.m)
exp.m<- fitdistrplus::fitdist(sismos.m-alpha, distr = "exp", method="mme")$estimate
gamma.m<- fitdistrplus::fitdist(sismos.m-alpha, distr="gamma", method="mme")$estimate

pexp(4-alpha, rate=exp.m[1])
pgamma(4-alpha, shape=gamma.m[1],rate=gamma.m[2])

mean(sismos.m<4)

# mejor gamma


# i3-2020-2 ---------------------------------------------------------------

Digimon <- readxl::read_excel("R-studying/Estudio verano/Digimon.xlsx")

virus_mega_int<-dplyr::filter(Digimon, Tipo=="Virus" & Etapa=="Mega")$Inteligencia

t.test(x=virus_mega_int, mu=160, alternative = "two.sided")$p.value

velocidad_fuego<-dplyr::filter(Digimon, Atributo=="Fuego")$Velocidad

TeachingDemos::sigma.test(x=velocidad_fuego, sigma=sqrt(400), alternative="greater")$p.value

at_def_cham<- dplyr::filter(Digimon, Etapa=="Champion")$Ataque + dplyr::filter(Digimon, Etapa=="Champion")$Defensa

t.test(x=at_def_cham, mu=220, alternative="less")$p.value

hp<-Digimon$HP
log<-fitdistrplus::fitdist(hp, distr="lnorm", method="mle")$estimate
gamma<-fitdist(hp, distr = "gamma", method = "mme")$estimate

1-plnorm(1500, meanlog = log[1], sdlog = log[2])
1-pgamma(1500, shape= gamma[1], rate=gamma[2])
mean(hp>1500)

#9

xm<-30/120

p0<- 0.3
n=120
z0<-(xm-p0)/sqrt((p0*(1-p0))/(n))

pnorm(z0)

#10

z0<-(17-20)/((17)/sqrt(100))
pnorm(z0)

TeachingDemos::z.test(x=17, mu=20 , sd=17,n=100 ,alternative = "less")$p.value


# i3-2019-01 --------------------------------------------------------------

prop.test(x=72, n=124, p=2/3, alternative = "less", correct=F)$p.value
 
(16-20)/(12/sqrt(28))

pt((16-20)/(12/sqrt(28)),df=27)

sigma0<-27*(12**2)/(10**2)

1-pchisq(38.88, df=27)
pnorm(-1.463)


# Examen 2019 01 ----------------------------------------------------------

qf(0.95,df1=12, df2=20)

pt(-1.347, df=17.2175)
pnorm(-0.425)

plnorm(0.01271958, meanlog=4.372228, sdlog=0.12435090)

chisq.test(x=c(8, 40, 98, 107, 47),p=c(0.01271958, 0.1471416, 0.3715424, 0.3161463,0.1524503),rescale.p = T)

O<-c(8,40,98,107,47)
freq<-c(60, 70, 80, 90, 100)
shape1=10.19801
scale1=83.77327
p60<-pweibull(60,shape=shape1, scale=scale1)
p70<-pweibull(70,shape=shape1, scale=scale1)-pweibull(60,shape=shape1, scale=scale1)
p80<-pweibull(80,shape=shape1, scale=scale1)-pweibull(70,shape=shape1, scale=scale1)
p90<-pweibull(90,shape=shape1, scale=scale1)-pweibull(80,shape=shape1, scale=scale1)
p100<-1-(p90+p80+p70+p60)
p90
1-(p90+p80+p70+p60)
p1=c(p60, p70, p80, p90, p100)
p1
sum(p1)
chisq.test(x=O, p=p1, rescale.p = T)

pchisq(5.7232, df=2,lower.tail = F)


# Examen 2019 00 ----------------------------------------------------------

p1=pnorm(400, mean=500, sd=110)
p2=pnorm(500, mean=500, sd=110)-p1
p3=pnorm(600, mean=500, sd=110)-pnorm(500, mean=500, sd=110)
p4=1-p3-p2-p1
pp=c(p1,p2,p3,p4)
pp

o=c(12,18,22,8)
chisq.test(x=o, p=pp, rescale.p = T)$p.value

1-pf(9.058, 1,22)
1-pt(3.01,22)

modelo1<-rnorm(1:100, mean=1, sd=1)
hist(modelo1)
modelo2<-rlnorm(1:100, meanlog=1,sdlog=1)
hist(modelo2)

modelosss<-lm(formula= modelo1~modelo2)
summary(modelosss)
plot(modelo1~modelo2)
abline(modelo1, col="red", lwd=2)

# Examen 2020 02 ----------------------------------------------------------

(qnorm(0.95)*sqrt(0.15*0.85)/0.04)**2

marketing<- readxl::read_excel("R-studying/Estudio verano/Marketing(1).xlsx")

menor_16<-dplyr::filter(marketing, Ventas<16)$Periódico
mayor_16<-dplyr::filter(marketing, Ventas>=16)$Periódico

var.test(x=menor_16, y=mayor_16, alternative = "two.sided")$p.value

#asumimos que es valido xd

t.test(x=menor_16, y=mayor_16, alternative="less", var.equal = T)$p.value

#4

periodico<-marketing$Periódico

gamma1<-fitdistrplus::fitdist(periodico, distr="gamma", method="mme")$estimate
gamma
ks.test(x=periodico, "pgamma",shape=gamma1[1], rate=gamma1[2])

#5
library(readxl,dplyr,TeachingDemos )
library(fitdistrplus)
abalon <- read_excel("R-studying/Estudio verano/Abalon.xlsx")

puerto_anillos<- dplyr::filter(abalon, centro=="Puerto Montt")$anillos
o<- c(sum(puerto_anillos<9), sum(puerto_anillos<=12)-sum(puerto_anillos<9),sum(puerto_anillos>12))
k=length(o)
n=sum(o)

p<-diff(ppois(c(-Inf, 8, 12, Inf),lambda=mean(puerto_anillos)))
d<-chisq.test(x=o, p=p)$statistic

valor.p<-1-pchisq(d, df=k-1-1)
coquimbo_anillos<-dplyr::filter(abalon, centro=="Coquimbo")$anillos

o<-c(sum(coquimbo_anillos<9),sum(coquimbo_anillos<=14)-sum(coquimbo_anillos<9),sum(coquimbo_anillos>14))
k<-length(o)
n<-sum(o)
p<-diff(ppois(c(-Inf,8, 14, Inf), lambda=mean(coquimbo_anillos)))
d2<- chisq.test(x=o,p=p)$statistic
p.value2<-1-pchisq(d2, df=k-1-1)

chiloe_anillos<-dplyr::filter(abalon, centro=="Chiloé")$anillos
o<- c(sum(chiloe_anillos<8),sum(chiloe_anillos<=12)-sum(chiloe_anillos<8), sum(chiloe_anillos>12))
n<-sum(o)
k<-length(o)
p<-diff(ppois(c(-Inf, 7, 12, Inf), lambda=mean(chiloe_anillos)))
d<-chisq.test(x=o, p=p)$statistic
p.value<-1-pchisq(d, df=k-1-1)

#6

fit1<-summary(lm(pesocu~largo, data=abalon))$r.squared
fit2<-summary(lm(pesocu~diametro,data=abalon))$r.squared
fit3<-summary(lm(pesoco~alto, data=abalon))$r.squared

#7

fit4<-lm(pesocu~largo+alto,data=abalon)
fit5<-lm(pesocu~largo+alto+diametro, data=abalon)
anova(fit4,fit5)

vari<-c()
for(i in 1:10000){
varx<-sd(rlnorm(1000, meanlog=1,sdlog=1/2))**2
vary<-sd(rlnorm(1000, meanlog=2, sdlog=1))**2

vari[i]=varx-vary
}
mean(vari)

1-pgamma(1,9,15)



# Examen 2022 00 ----------------------------------------------------------

p<-1-pweibull(3, shape=2, scale=5)

dbinom(x=4, size=4, prob = p)
rho<-0.85
mux<-720
sdx<-50
muy<-680
sdy<-80
cov<-rbind(c(sdx^2,rho*sdx*sdy),c(rho*sdx*sdy,sdy^2))
mvtnorm::pmvnorm(lower=c(750,750),upper=c(Inf, Inf), mean=c(720, 680), sigma=cov)
x<-750
muz<-muy+(x-mux)*(sdy*rho)/sdx

sdz<-sdy*sqrt(1-rho**2)

pnorm(750, mean=muz,sd=sdz, lower.tail = F)

library(readxl)
sinca <- read_excel("R-studying/Estudio verano/SINCA2022.xlsx")

co_nov<-dplyr::filter(sinca, MONTH==11)$CO
fit4<-fitdistrplus::fitdist(co_nov, distr = "weibull",method="mle" )$estimate
fit4
ks.test(x=co_nov, "pweibull", shape=fit4[1],scale=fit4[2])  

o3_jun<-dplyr::filter(sinca, MONTH==6)$O3
o3_jul<-dplyr::filter(sinca, MONTH==7)$O3

var.test(o3_jun,o3_jul)$p.value

t.test(o3_jun, o3_jul, alternative="two.sided")$p.value

#10

tmin_en<-dplyr::filter(sinca, MONTH==1)$T.MIN

n1<-length(tmin_en)
x1<-length(subset(tmin_en, tmin_en<14))
p0<-1/3

prop.test(x=x1,n=n1,p=1/3, alternative="less", correct=F)$p.value

#11

pm_feb<-dplyr::filter(sinca, MONTH==2)$PM2.5

fit4<-fitdistrplus::fitdist(pm_feb, distr="lnorm", method="mme")$estimate
fit4

o<-c(39, 45, 35, 15, 15)
p<-diff(plnorm(c(0,15,20,25,30,Inf),meanlog = fit4[1],sdlog=fit4[2]))
k<-length(o)
d<-chisq.test(x=o, p=p)$statistic
p.value<-1-pchisq(d, df=k-1-2)

#12

modelo1<-summary(lm(O3~T.MIN,data=sinca))$r.squared
modelo2<-summary(lm(O3~T.MEAN,data=sinca))$r.squared
modelo3<-summary(lm(O3~T.MAX,data=sinca))$r.squared

r2<-c(modelo1, modelo2, modelo3)

##mejor modelo el 2

fit<-lm(O3~T.MEAN,data=sinca)$coef[2]



# Examen 2018 01 ----------------------------------------------------------

o<-c(16,412, 465, 94, 13)
p<-diff(plnorm(c(1,2,3,4,5,Inf),meanlog=1.147816, sdlog=0.1999574))

d1<-chisq.test(x=o, p=p)$statistic
1-pchisq(d1, df=5-1-2)
eta<-exp(1.235563)
beta<-1/0.1527360
p2<-diff(pweibull(c(1,2,3,4,Inf),shape=beta, scale=eta))
d2<-chisq.test(x=c(16,412,465,107),p=p2,rescale.p = T)$statistic

1-pchisq(d2, df=5-1-2)


# 2021-01 i3 --------------------------------------------------------------

library(readxl)
ENS_Muestra <- read_excel("R-studying/Estudio verano/ENS_Muestra.xlsx")

coles<-ENS_Muestra$COLESTEROL
hdl<-ENS_Muestra$HDL

mu0<-mean(hdl)*4-mean(coles)

sigma.2<-16*var(coles)+var(hdl)-2*4*cov(coles, hdl)

pnorm(0, mean=mu0, sd=sqrt(sigma.2))

Prototipo <- read_excel("R-studying/Estudio verano/Prototipo.xlsx")

mu0=23
rendi_new=dplyr::filter(Prototipo,PROTOTIPO=="nuevo")$RENDIMIENTO
t.test(x=rendi_new, mu=23, alternative="greater")$p.value 

simga0=0.57

stand<-dplyr::filter(Prototipo, PROTOTIPO=="estandar")$RENDIMIENTO

TeachingDemos::sigma.test(stand, sigma=simga0, alternative="less")$p.value

mean(rendi_new>23.5)

fit1<-fitdistrplus::fitdist(data=rendi_new, method="mme", distr="norm")$estimate
fit2<-fitdistrplus::fitdist(data=rendi_new, method="mme", distr="lnorm")$estimate
fit3<-fitdistrplus::fitdist(data=rendi_new, method="mme", distr="gamma")$estimate
1-pnorm(23.5, mean=fit1[1], sd=fit1[2])
1-plnorm(23.5, meanlog=fit2[1], sdlog=fit2[2])
1-pgamma(23.5, shape=fit3[1],rate=fit3[2])

#12

p0=1/3

prop.test(x=50, n=124, p=p0, alternative = "greater",correct=F)$p.value


# 2022 00 examen ----------------------------------------------------------
sdx<-50
sdy<-80
muxy=c(720, 680)
cov<-c(c(sdx**2,0.85*sdx*sdy),c(0.85*sdx*sdy),c(sdy**2))
cov<-matrix(cov, nrow=2, ncol=2)

mvtnorm::pmvnorm(lower=c(750,750),upper=c(Inf, Inf), mean=muxy, sigma=cov)

sinca<- read_excel("R-studying/Estudio verano/SINCA2022.xlsx")
feb_pm<-dplyr::filter(sinca, MONTH==2)$PM2.5
fit1<-fitdistrplus::fitdist(feb_pm, distr="lnorm", method="mme")$estimate
o<-c(39, 45, 35, 15, 15)
p<-diff(plnorm(c(0,15,20,25,30, Inf), meanlog=fit1[1], sdlog=fit1[2]))
d<-chisq.test(x=o, p=p)$statistic
1-pchisq(d, df= 5-1-2)




# Examen 2019 00 ----------------------------------------------------------

o<-c(12,18,22,8)
p<-diff(pnorm(c(-Inf,400, 500, 600, Inf),mean=500, sd=110))

d<-chisq.test(x=o, p=p)$statistic
1-pchisq(d, df=4-1-2)

