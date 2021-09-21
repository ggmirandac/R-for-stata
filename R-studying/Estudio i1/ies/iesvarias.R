library(rio)
library(dplyr)

ens<-import("C:/Users/Gabri/Desktop/Universicosas/R-for-stata/R-studying/Estudio i1/ies/ENS.xlsx")

ens_fil_peso<-ens$PESO
x50<-quantile(ens_fil_peso, prob=0.5)
x75<-quantile(ens_fil_peso,probs = 0.75)

a<-log(2)/x50
b<-log(3)/log(exp(a*x75)-1)

Fx<-function(x){
  g<-((exp(a*x)-1)^b)/(1+((exp(a*x)-1)^b))
  return(as.numeric(g))
}

Fx(80)

print(1-Fx(80))
