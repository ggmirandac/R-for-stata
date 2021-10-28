##### Laboratorio 5 #####

#### set.seed() ####
# Esta función sirve para evaluar en una misma semilla de inicio de un proceso aleatório
# de manera que la secuencia aleatória de números es la misma para una misma semilla
# Ejemplo

set.seed(1113)

x<-rnorm(10,mean=10,sd=2)

# El x siempre contendrá los mismos valores si se mantiene la misma seed en ese orden

#### Distribución Hipergeométrica ####

# En un lote de tamañoo N tengo m objetos defectuosos y N−m que no
# son defectuosos, obtengo una muestra aleatoria de tamaño n y luego la
# probabilidad de que x objetos sean defectuosos está dada por la función
# de probabilidad de la distribución hipergeométrica

# X: cantidad de objetos defectuosos de la muestra
# X=0,1,...,min(m,n)

### en R ###

# Se define como una urna con m bolas blancas y n negras. Se realiza una extracción de tamaño
# k y x representa el número de bolas blancas extraídas.
# Aquí: N = m+n y n = k
# si X: cantidad de bolas balncas que se obtienen la distribución es:
# dhyper(x,m,n,k)
# phyper(q,m,n,k)
# qhyper(p,m,n,k)
# rhyper(nn,m,n,k)

# De forma teórica E(X)=k*p, p=(m/(m+n)) y Var(X)=k*p*(1-p)*(m+n-k)/(m+n-1)

#### Medidas Descriptivas Teóricas vs Empíricas ####

# Una variable aleatórica puede ser descrita por su función de distribución de densidad o 
# de probabilidad acumulada. Pero no siempre son conocidas. Aquí se pueden usar las medidas 
# para conocer la forma de una distribución

### Medidas ###

# Medidas centrales
# Medidas de posición
# Medidas de disperción
# Medidas de Asimetría y Forma

# Si hay una urna con 17 bolas blancas y 23 negras. Si se extraen 15 al azar ¿Cuál es la 
# distribución de las bolas blancas extraídas?
# X~Hipergeométrica(m=17,n=23,k=15)

nmuestra=120
m=17
n=23
k=15
set.seed(1113)
X=rhyper(nn=nmuestra,m=m,n=n,k=k)

## Gráfico de distribución empírica vs teórica ##

maximo = min(m,k);maximo
table(X)

prop.table(table(X)) #las probabilidades que salga cada elemento de la tabla 

proptable=prop.table(table(X))

sum(proptable) # se puede verificar que da 1 la suma de las probabilidades

# gráfica de las probablidades de proptable, con los eje x xlim, lwd->ancho de barras
plot(proptable, xlim= c(0,maximo),col="orange",lwd=4) 
x<-0:maximo
axis(side=1,at=x) # ampliamos los valores en el eje x

distriprop<-dhyper(x,m=m,n=n,k=k) # probabilidades a partir de la distribución de probabilidad

sum(distriprop) # verificamos que da 1

points(x,dhyper(x,m=m,n=n,k=k),lwd=10,pch=16,col="darkblue") # gráficamos los puntos dados por dhyper

# Valor esperado #
# comparación de la media a partir de datos rhyper y los valores de la distribución

# media muestral

mean(X)
abline (v=mean(X), col="red",lty=2,lwd=2)

#media teórica

p=m/(m+n)

abline(v=k*p,col="darkgreen",lty=2,lwd=2)

# moda #

# empírica
library(modeest)

mlv(X)
# teórica
x[dhyper(x,m=m,n=n,k=k) == max(dhyper(x,m=m,n=n,k=k))]


# mediana #

# mediana muestral

median(X)

# mediana teórica

qhyper(0.5, m=m,n=n,k=k)


# Esperanza matemática
# esta se define como E(g(x)) es decir mean(g(x)) donde g(x) puede ser cualquier función

# varianza y desviación estandar

# en R es: var(x) para varianza y sd(x) para la desviación estandar

var(X)

sd(X)

#Rangos

Rango=function(X){
  max(X)-min(X)
}

# Rango intercuartilico

IQR=function(X){
  quantile(X,0.75)-quantile(X,0.25)
}

# Coeficiente de variaciín

cov= sd(X)/mean(X)

# Skewness E(X-mu**3) y se mide por el coeficiente de asimetría y se calcula como

library(moments)

skewness(X) # nos provee del coeficiente de asimetría
 
# Kurtosis E(X-mu**4) achatamiento de la curva. Este se usa por el coeficiente de kurtosis

kurtosis (X)-3

# Con dos variables se puede relaciones entre ellas

# la covarianza se define como 
# Cov(X,Y)=E[(X-mux)(Y-muy)]=E(X*Y)-mux*muy

# Esta variable puede normalizarse al utilizar la correlación entre estas dos variables
# Corr(X,Y)=Cov(X,Y)/sd(X)*sd(Y)
# Esta varible toma valores entre -1 y 1
# Este valor se puede calcular por medio de las funciones cov() y cor()

# para este caso se utilizan dos variables

# Se toma Y~Binomial(k,p)

set.seed(1113)
Y=rbinom(120,size=k,prob=p)
maximo=k
table(Y)
protable=prop.table(table(Y))
plot(protable)
x=0:maximo
axis(1,at=x)

dbinom(x,size=k,prob=p)
points(x,dbinom(x,size=k,prob=p),lwd=10,pch=16,col="darkblue")
plot(X,Y,pch=16)

cov(X,Y)

cor(X,Y)
