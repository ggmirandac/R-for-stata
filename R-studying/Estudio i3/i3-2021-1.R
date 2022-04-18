ens_muestra<- read_excel("R-studying/Estudio i3/ENS_Muestra.xlsx")

colesterol <- dplyr::filter(ens_muestra, SEXO=="Masculino")$COLESTEROL
hdl <- dplyr::filter(ens_muestra, SEXO=="Masculino")$HDL


# Busco P(Colesterol-4HDL>0)==P(4*HDL-Colesterol<0)

mu=4*mean(hdl)-mean(colesterol)
var=16*var(hdl)-2*4*cov(colesterol,hdl)+var(colesterol)
sigma=sqrt(var)

pnorm(0,mean=mu,sd=sigma)



# 8-2021-01 ---------------------------------------------------------------

library(readxl)
Prototipo <- read_excel("R-studying/Estudio i3/Prototipo.xlsx")

#8.1

mo<-23

#Ha m0>23

# alpha <- 0.01

rendi<-dplyr::filter(Prototipo, PROTOTIPO=="nuevo")$RENDIMIENTO

t.test(x=rendi, mu=mo,alternative="greater")$p.value

mean(rendi)


# 9-2021-01 ---------------------------------------------------------------

sigma0<-0.57
estan<-dplyr::filter(Prototipo, PROTOTIPO=="estandar")$RENDIMIENTO

TeachingDemos::sigma.test(x=estan, sigma=sigma0,altertative="less")$p.value


# 10-2021-01 --------------------------------------------------------------

mean(rendi>23.5)

#Para normal

nor<-fitdistrplus::fitdist(data=rendi, distr = "norm",method = "mme")
1-pnorm(23.5,mean=nor$estimate[1],sd=nor$estimate[2])
lnor<-fitdistrplus::fitdist(data=rendi, distr = "lnorm",method = "mme")
1-plnorm(23.5,meanlog = lnor$estimate[1],sdlog=lnor$estimate[2])
gamma<-fitdistrplus::fitdist(data=rendi, distr = "gamma",method = "mme")
1-pgamma(23.5,shape = gamma$estimate[1],rate=gamma$estimate[2])


# 11-2021-01 --------------------------------------------------------------


estan

k=2177
n=length(estan)

mu0=21.4

Z0=(mean(estan)-mu0)/sqrt(mu0^2/(n*k));Z0

#12

n=124

x=50

p0=1/3

prop.test(x=x,n=n,p=p0,alternative="greater",correct = F)$p.value



# 2021-00 -----------------------------------------------------------------
#simulaciones
y=c()
for (i in 1:100000){
  n=30
  x<-rbinom(30,size=1,prob = 1/2)
  suma=0
  x[x==0] <- -7
  x[x==1] <- 10
  y[i]<-sum(x)
}

mean(y>50)


# 9

library(readxl)
Sismos2021 <- read_excel("R-studying/Estudio i3/Sismos2021.xlsx")

tiem <- Sismos2021$Tiempo

exp <- fitdistrplus::fitdist(data=tiem, distr = "exp", method = "mle")$estimate
1-pexp(20,rate=exp)

gam<- fitdistrplus::fitdist(data=tiem, distr = "gamma", method = "mle")$estimate
1-pgamma(20,shape=gam[1],rate=gam[2])

wei <- fitdistrplus::fitdist(data=tiem, distr = "weibull", method = "mle")$estimate
1-pweibull(20,shape=wei[1],scale=wei[2])

mean(tiem>20)

# 10

magni <- Sismos2021$Magnitud
desp <- min(magni)

exp2 <- fitdistrplus::fitdist(data=magni - desp, distr = "exp", method="mme")$estimate
pexp(4-desp, rate=exp2[1])

gam2 <- fitdistrplus::fitdist(data=magni - desp, distr = "gamma", method="mme")$estimate
pgamma(4-desp, shape = gam2[1],rate=gam2[2])

mean(magni<=4)


# 2020-02 -----------------------------------------------------------------

library(readxl)
Digimon <- read_excel("R-studying/Estudio i3/Digimon.xlsx")

virus_mega <- dplyr::filter(Digimon, Etapa=="Mega" & Tipo=="Virus")
inteli_virus_mega <- virus_mega$Inteligencia

t.test(x=inteli_virus_mega, mu=160, alternative="two.sided")

fuego <- dplyr::filter(Digimon, Atributo=="Fuego")$Velocidad

TeachingDemos::sigma.test(x=fuego, sigma=sqrt(400),alternative="greater")$p.value

at_def<- dplyr::filter(Digimon, Etapa=="Champion")[c("Ataque","Defensa")]
at_def_sum<- apply(at_def,1,sum)

t.test(x=at_def_sum,mu=220,alternative = "less")$p.value

# 4

#Ajustes

hp <- Digimon$HP

lnor<-fitdistrplus::fitdist(data=hp,distr = "lnorm",metho="mle")$estimate
1-plnorm(1500,meanlog=lnor[1],sdlog=lnor[2])

gmm<-fitdistrplus::fitdist(data=hp,distr = "gamma",metho="mme")$estimate
1-pgamma(1500,shape = gmm[1],rate=gmm[2])

mean(hp>1500)

#5 

1-pexp(1,rate=1)


# 9

prop.test(x=30,n=120,p=0.3,alternative = "less")$p.value

# 10

pnorm(-1.5)

# 11

n=60
mu=2
sd=1
pnorm(130,mean=mu*60,sd=sd*sqrt(60))

# 12

1-pgamma(100,shape=50,rate=1/2)


# i3-2019-01 --------------------------------------------------------------


# 1

prop.test(x=72,n=124,p=2/3,alternative = "less")$p.value

alpha <- 5/100

prop.test(x=72,n=124,p=2/3,alternative = "less")$p.value > alpha

#p.value< alpha es valido que menos de 2/3 acerto

TeachingDemos::z.test(x = 16, mu = 20, stdev = 12, n = 28, alternative = "less")$p.value

# delta=10, delta >10

n=28
s=12
c=((n-1)*s^2)/(10^2)

1-pchisq(q=c,df=27)




#### 3

(4.611766-6)/(6/sqrt(40))
pnorm(-1.463327)


# i3-2019-00 --------------------------------------------------------------

# 1a

prop.test(x=30,n=78,p=0.3,alternative="greater")$p.value

T0=(5.8-5)/(1.8/sqrt(12))
1-pt(T0,df=12-1)



# Ensayo 7 ----------------------------------------------------------------

1-pnorm(sqrt(3*40)/(4*pi))

# 6

muestra<-rio::import("R-studying/Estudio i3/ENS_Muestra.xlsx")

cole <- muestra$COLESTEROL
hdl <- muestra$HDL

new_mean=4*mean(hdl)-mean(cole)
new_sd=sqrt(16*var(hdl)+var(cole)-2*4*cov(cole,hdl))

pnorm(0,mean=new_mean,sd=new_sd)

Z=4*hdl-cole
pnorm(0,mean=mean(Z),sd=sd(Z))

# 7 

pchisq(0.8*0.8*25,df=25)

#8 

proto <- rio::import("R-studying/Estudio i3/Prototipo.xlsx")
new_proto <- dplyr::filter(proto, PROTOTIPO=="nuevo")
t.test(x=new_proto$RENDIMIENTO, mu=23, alternative="greater")$p.value

#9 
library(TeachingDemos)
new_estan <- dplyr::filter(proto, PROTOTIPO=="estandar")
sigma.test(x=new_estan$RENDIMIENTO,sigma=0.57,alternative="less")$p.value

mean(new_proto$RENDIMIENTO>23.5)

nor<- fitdistrplus::fitdist(new_proto$RENDIMIENTO,distr = "norm",method="mme")$estimate
1-pnorm(23.5,mean=nor[1],sd=nor[2])

lnor<- fitdistrplus::fitdist(new_proto$RENDIMIENTO,distr = "lnorm",method="mme")$estimate
1-plnorm(23.5,meanlog=lnor[1],sdlog=lnor[2])

gamr<- fitdistrplus::fitdist(new_proto$RENDIMIENTO,distr = "gamma",method="mme")$estimate

1-pgamma(23.5,shape=gamr[1],rate=gamr[2])

# 11

mu=mean(new_estan$RENDIMIENTO)
mu0=21.4
n=length(new_estan$RENDIMIENTO)
k=2177

Z0= (mu-mu0)/sqrt(mu0^2/(k*n));Z0
              
                  
# 12
prop.test(x=50,n=124,p=1/3,alternative = "greater",correct=F)$p.value
