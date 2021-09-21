notA=choose(150,0)*choose(447-150,10)+choose(150,1)*choose(447-150,9)+choose(150,2)*choose(447-150,8)+choose(150,3)*choose(447-150,7)
S=choose(447,10)
1-notA/S
getwd()
demanda_gas=import(paste0(getwd(),"/R-studying/Ensayos/Ensayo 1/Demanda_GAS.xlsx"))

gas2015<-subset(demanda_gas,AÑO==2015)

gas2015_1<-subset(gas2015,MES==1)


promedios<-numeric()
for (year in 2015:2020){
  gas_year<-subset(demanda_gas,AÑO==year)
  if (year != 2020){
    for (mes in 1:12){
      gas_year_mes<-subset(gas_year,MES==mes)
      promedios<-append(promedios, mean(gas_year_mes$PRECIO))
    }  
  }
  else{
    for(mes in 1:7){
      gas_year_mes<-subset(gas_year,MES=mes)
      promedios<-append(promedios,mean(gas_year_mes$PRECIO))
    }
  }
}

print(promedios)

summary(promedios)

c.o.v=sd(promedios)/mean(promedios)
print(c.o.v)

mfv(promedios)

moda <- promedios[which.max(promedios)]
print(promedios)

quantile(promedios,.65)
#################### Corrección #########################

Demanda_GAS=rio::import("C:/Users/Gabri/Desktop/Universicosas/R-for-stata/R-studying/Estudio i1/Ensayo 1/Demanda_GAS.xlsx")
base=Demanda_GAS
head(base)

## Aplico la función mean a la columna base$PRECIO filtrada por los indices base$MES y base$AÑo
X=c(tapply(X=base$PRECIO,INDEX=list(base$MES,base$AÑO),mean)) ## Index son los valores que se usan para filtrar
print(X)
Tabla<-round(rbind(IQR(X,na.rm=T),median(X,na.rm=T),sd(X,na.rm=T)/mean(X,na.rm=T),mfv(X,na.rm=T),quantile(X,prob=0.65,na.rm=T)),2)
rownames(Tabla)<-c("IQR","Mediana","c.o.v.","moda","percentil 65")
colnames(Tabla)<-"Precio"
print(Tabla)



















############## REDOING ######################3


x<-c(tapply(Demanda_GAS$PRECIO,INDEX = list(Demanda_GAS$MES,Demanda_GAS$AÑO),mean))

### tapply lo que hace es a los datos X los filtra por INDEX segun su label, es decir
### Si tienen esos mismos indices se toman con como un conjunto de datos, y se les aplica la funcion FUN

IQRx<- IQR(x,na.rm=T)
medianax<- median(x,na.rm=T)
cov<-sd(x,na.rm=T)/mean(x,na.rm=T)
modax<-modeest::mfv(x,na.rm=T)
percentil65<-quantile(x,na.rm=T,prob=0.65)

###############################################################
#pregunta 2
S=choose(447,10)
notA=choose(150,0)*choose(447-150,10)+choose(150,1)*choose(447-150,9)+choose(150,2)*choose(447-150,8)+choose(150,3)*choose(447-150,7)
probA=1-(notA/S)           
print(probA)
#### Pregunta 3

P=(0.8*0.65*0.2+0.2*0.15*0.05)/(0.8*0.65+0.2*0.15)

#### Pregunta 4
PE2dA=(0.3*0.35)/(0.4*0.2+0.35*0.3+0.05*0.15)
print(PE2dA)

### pregunta 5
p=1-0.36

proba=1-((p^3+p^2+p^2)/3)
print(proba)

## pregunta 8
Demanda_GAS <- rio::import("R-studying/Estudio i1/Ensayo 1/Demanda_GAS.xlsx")

gas<-c(tapply(Demanda_GAS$PRECIO,INDEX = list(Demanda_GAS$AÑO,Demanda_GAS$MES),mean))

iqrgas<-IQR(gas,na.rm=T)
medianagas<-median(gas,na.rm=T)
coefgas<-sd(gas,na.rm = T)/mean(gas,na.rm=T)
modagas<-modeest::mlv(gas,na.rm=T)
perceltilgas<-quantile(gas,na.rm=T,prob=0.65)

print(iqrgas)
print(medianagas)
print(coefgas)
print(modagas)
print(perceltilgas)

gas2<-dplyr::filter(Demanda_GAS,Demanda_GAS$MES>=3,Demanda_GAS$MES<5)
