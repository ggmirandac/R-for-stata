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


base=demanda_gas
head(base)
## Aplico la función mean a la columna base$PRECIO filtrada por los indices base$MES y base$AÑo
X=c(tapply(X=base$PRECIO,INDEX=list(base$MES,base$AÑO),mean)) ## Index son los valores que se usan para filtrar
print(X)
Tabla<-round(rbind(IQR(X,na.rm=T),median(X,na.rm=T),sd(X,na.rm=T)/mean(X,na.rm=T),mlv(X,na.rm=T),quantile(X,prob=0.65,na.rm=T)),2)
rownames(Tabla)<-c("IQR","Mediana","c.o.v.","moda","percentil 65")
colnames(Tabla)<-"Precio"
print(Tabla)
