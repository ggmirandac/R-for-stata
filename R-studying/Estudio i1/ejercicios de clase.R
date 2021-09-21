#Los n ́umeros aleatorios son la base esencial de la sim-ulaci ́on
#de  variables  aleatorias  que  veremos  m ́as  adelante. 
#Comoprimera experiencia supongamos que cada alumno de la universidadva a 
#elegir totalmente al azar un n ́umero entre 2 y 5.
set.seed(1113)
U=runif(20000,2,5)
hist(U,freq=F,col="gray",border="white",las=1,nclass=5,main="",font.main=1,xlab=expression(Theta[U]))


#Sean X e Y dos vectores de datos simulados:
X=rgamma(1000,rate=1/2,shape=1/2)
Y=rgamma(1000,rate=1,shape = 1)

par(mfrow=c(1,2),cex=0.8)

hist(X, freq = FALSE, ylim = c(0,0.9), xlim = c(0,10),breaks = seq(0,25,1), main = "Histograma de X")
hist(Y, freq = FALSE, ylim = c(0,0.9), xlim = c(0,10),breaks = seq(0,25,1), main = "Histograma de Y")

Tabla=rbind(c(mean(X),sd(X),sd(X)/mean(X),skewness(X)),c(mean(Y),sd(Y),sd(Y)/mean(Y),skewness(Y)))
colnames(Tabla)<-c("mu","sigma","delta","skewness")
rownames(Tabla)<-c("X","Y")
