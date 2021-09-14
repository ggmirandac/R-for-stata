
########### Useful Functions ########### 
# sum(x): sum all the components of the x vector
# prod(x): multiply all the components of the x vector
# choose(n,r): get the combination of n over r (combinatoria de n sobre r)
# factorial(n): n factorial
# sample(x,n,replace=): obtain a sample of size n of the vector x
# round(x,n): round the number x with n decimals
##########################################

########### Descriptive Statistics ########### 
# Tendencia central: Media (mean())
                  #: Moda (no hay función nativa)
                  #: Mediana (median())
# Posición: Percentil: Cuantil (quantil())
         #: Mínimo (min())
         #: Máximo (max())
# Disperción: Varianza (var())
           #: Desviación Estandar (sd())
           #: Coeficiente de variación
           #: Rango (range())
           #: Rango intercuartil
# Fomra: Coeficiente de asimetría
     #: Kurtosis
# Funciones bacanosas: summary() genera un resumen de vectores numericos
 
######### if, else and else if #############
x<- -10
if (x>=10){
  print("este numero es mayor o igual a 10")
}else if(0<x&&x<10){
  print("Este numero es menor a 10")
}else{
  "este no es un numero positivo"
}


########## While and for loops ##########

### While loop ###
y<-0
su<-0
while (su<100){
  print(paste0("y vale : ",y,"y la suma es ",su))
  y<-y+1
  su<-su+y
  if(su>=100){
    print(paste0("La suma es mayor a o igual a 100 en ", y," con un total de ", su))
  }
}

# tambien se pueden romper while loops con break

### for loop ###

x<-1:10
for (i in x){
  print(i)
}

# We can also use for loop to read a matrix

matriz<-matrix(1:20,ncol=4)
for(i in 1:nrow(matriz)){
  for(j in 1:ncol(matriz)){
    print(paste0(matriz[i,j]," está en la coordenada ",i,",",j))
  }
}  

### If Else For ###
# The function ifelse(test,A,B) is a function where, test is a logic expression, 
# A es what is executed when test is true, and B when is false

a<-c(-2,-1,1,2)
ifelse(a>0,"positivo","negativo") #creates a vector

########## Funciones #########
# To create a function we use the command function
j<-"holis"
f1 <- function(x){
  print(paste0("Ella dijo ",j))
}
f1(j)

d<-c(1,2,3,4,5,6,7,8)
f2<-function(vector){
  suma=sum(vector)
  return(suma)
}
f2(d)

# returning a list

f3<-function(x){
  transpuesta<-t(x)
  inversa<-solve(x)
  list(tr=transpuesta,inv=inversa)
}
matri<-matrix(c(1,3,2,5),ncol=2)
resultados<-f3(x=matri)
resultados$tr
resultados$inv

######### other important functions #########
# seq(from=a,to=b,by=d): sequence from a to b by d steps of d
# rep(x,n): replicate x n times
# sort(x): order the x vector from lower to bigger
# rev(x): reverse the x vector
# pmin(x1,...,xn): minimum of each component of the vectors
# pmax(x1,...,xn): maximum of each component of the vectors

#### sapply apply tapply lapply ####
# This functions are used o to apply a function to vectors or matrices

### sapply ###

# sapply(X, FUN, ...) is a function that applies the FUN function to each component of X,
# if FUN receives more than one argument, you can pass it in the ...

x1<-1:1000/1000
sapply(x1,round,digits=2) # in ... you have to declare the name of the argument

### apply ###

# apply(X,MARGIN,FUN,...) calculate for each row or col of the matrix X the function FUN.
# in the ... you can add the extra arguments of FUN function. The MARGIN argument define 
# if the function is applied to the rows (MARGIN=1) or the cols (MARGIN=2).

matrix3<-matrix(1:30, ncol=3,nrow=10)

apply(matrix3,2,mean) #calcula el primedio por columna

apply(matrix3,2,mean,trim=0.1)

### tapply ###

# tapply(X, INDEX, FUN, ...) calculate the function FUN to the vector X depending 
# on the value of the arguments INDEX, by deffault is a factor of the dame lenght of X.
# If FUN has more than one argument is passed in ...}

edad <- 30:59
genero <- rep(c("F","M","NB"),10) ## This index is used to organize the information
tapply(edad,genero,mean)

### lapply ###

# lapply(X, FUN, ...) operates a function over each columns of the X data.frame.
# if FUN has more than one argument it's passed in ...

m<-matrix(genero,ncol=3)
m<-as.data.frame(m)
lapply(m,as.factor)
