
# Laboratorio 9 -----------------------------------------------------------


# Ejercicio ---------------------------------------------------------------

edad <- c(rep(9,1),rep(11,4),rep(13,1),rep(12,2),rep(14,1),rep(9,1),rep(10,3),rep(11,1),
          rep(12,1),rep(13,3),rep(14,2))
sexo <- c(rep(0,1+4+2+1+1),rep(1,1+3+1+1+3+2))

data <- data.frame(Sexo=sexo, Edad=edad)

tabla<- table(data)

p.prob.conjunta=prop.table(tabla) # probabilidad de los elementos de las tablas
p.prob.conjunta

# 2 # 
p.B <- sum(p.prob.conjunta["1",c("10","12","14")])
p.B

mean(data$Sexo==1 & data$Edad%%2==0)

# 3 #
p.prob.conjunta

p.X = apply(X=p.prob.conjunta, MARGIN = 1, FUN=sum)
p.X

# 4 #
p.Y = apply(X=p.prob.conjunta,MARGIN =2 , FUN = sum)
p.Y

# 5 #

p.Y.dado.X <- prop.table(x=tabla, margin = 1) # el valor dado la fila 1, en este caso el sexo
p.Y.dado.X

# 6 #
p.prob.conjunta["0","9"]
p.X["0"]
p.Y["9"]
# No son independientes

# 7 #

Costo <- function(genero,edad){
  ifelse(genero==0, 2500+120*edad,3000+150*edad)
  return(costo)
}

mean(Costo(data$Sexo,data$Edad))


# Poisson Binomial --------------------------------------------------------

p=0.6
lambda=15
p.x.y<-function(x,y){
  p.conjunta=ifelse(x<=y, dbinom(x,size=y,prob=p)*dpois(y,lambda=lambda),0)
  return(p.conjunta)
}

x=seq(0,30)
y=seq(0,30)
z=outer(x,y, prob=p.x.y)
X=rep(x,times=length(y));X
Y=rep(y,each=length(x))
Z=c(z)



# Múltiples Variables -----------------------------------------------------

alpha=1
beta=2
f.x.y<-function(x,y){
  densidad<-ifelse(x>0 & y>0,alpha*beta*exp(-alpha*x-beta*y),0)
}
x=seq(0,1,0.01);x
y=seq(0,1,0.01);y
z=outer(x,y,FUN=f.x.y);z


#rgl.surface(x=x,y=z,z=y,color="red",back="lines")

X=rep(x,times=length(y))
Y=rep(y,each=length(x))
Z=c(z)

scatterplot3d(X,Y,Z,type="h",lwd=2,pch="",xlab="x",ylab="y",zlab=expression(f(X==x,Y==y)),highlight.3d=TRUE,angle=45)
              


# Función de variables aleatorias -----------------------------------------





