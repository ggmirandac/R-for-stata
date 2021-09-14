##########
## Data manipulation ##
Tipo<-c("I1","I2","Ex")
Nota<-c(3.5,6.0,5.5)
Azul<-c(F,T,T)

## Creation of a data frame, via data.frame command
Libreta<-data.frame(Tipo,Nota,Azul)
str(Libreta)
summary(Libreta)

## The names of the different col vectors can be made in this way
Libreta2<-data.frame(Tipo2=Tipo,Nota2=Nota,Azul2=Azul)

##We can access to subset of data

Libreta[c(1,3),c("Azul","Nota")] ## I want the 1 and 3 row and the columns Azul and Nota

## For example if I want to access to a certain column I can call it by the name with
## The command $ and []

## This command generates an vector
Libreta$Nota

## This generates a column vector
Libreta["Nota"]

## Subset function generates subsets of an data frame with a certain condition

subset(Libreta, subset = Azul==TRUE) ## Sybset the data in which Azul is True

subset(Libreta, subset = Nota > 5) ## Subset the data in wich Nota is bigger than 5

## The same effect of the subset function can be obtain ussing the []

Libreta[Libreta$Azul==TRUE,] ## Remeber to put the , after the condition
Libreta[Libreta$Nota>5,]

## To order a data base we can use the order() function that takes a vector to order
## a certain data frame, it goes from smaller to bigger. 
Libreta[order(Libreta$Nota),] ## Dont forget the ,
##If we put a - the order changes direction
Libreta[order(-Libreta$Nota),]

##Adding new rows. To add new rows we can use the function rbind(). In order to do it
## We have to bind together two data.frame that have the same colnames

nuevafila<-data.frame(Tipo="I4",Nota="4.5",Azul="TRUE")
nuevaLibreta<-rbind(Libreta,nuevafila)

## We can also add a column this directly on the base
nuevaLibreta$nuevacolumna <- nuevaLibreta$Nota


## the name of the rows and the columns can be change with the commands rownames()
## and colnames()
colnames(nuevaLibreta)<-c("Tipo2","Nota2","Azul2","Columna2")
colnames(nuevaLibreta)[4]<-"NuevaColumna2" ## With this command I assing the name 
                                           ## NuevaColumna2 to the 4th col of the df


## We can use the command list() to create a list that contains different types
## of objects
v3 <- 1:20
m1 <- matrix(v3,nrow=4,ncol=5,byrow=TRUE)
opiniones <- c("Bueno", "Malo", "Neutro", "Bueno","Malo","Malo", "Neutro", "Neutro")
opiniones <- factor(opiniones)
opiniones <- factor(opiniones, levels=c("Malo", "Neutro","Bueno"))

lista<-list(opiniones,m1,Libreta)

## We can assign names to the elements inside a list
lista2 <- list(v=opiniones, m=m1, bd=Libreta)

## To access to a certain object we can use the coordinates or the name
lista[1]
lista2$v

##COMANDS
##  getOption(): Gives a descriotion of the applications of the packages
## library : load a package
## install.package: install a package

################################################################################
# To read a data base there are different options
# read.table(): imports a TXT, DAT and CSV data
# read. csv(): import databases from a CSV file
# read_excel(); import data from XLS and XLSX (it requeris the packages readxl or tidyverse)
# scan(): import a vector of data
# import() : import different types of data (rio package)

################################################################################
# Working with data

# First look for its location, the file is in databases
location<-paste(toString(getwd()),"/R-lab-codes/databases/",sep="")

# Secondly we put the name of the file, in this case Tenis.txt

file<-toString(paste(location,"Tenis.txt",sep=""))

# Now we import the database using the import function
tenis <- import(file)

# To attach() function allows us to work directly with the data base
# after running this line we can work with the data contained in every column
# calling them by their names
attach(tenis)

# We can extract the names of the columns of a dataframe using the names() function

names(tenis)

# Using the str() function, unlike python that is used to create an string object
# in R this function creates a descriptive statistinc of the database

str(tenis)

# When R read a database, the decimal comma is used as "." so when its reading a 
# data base where this comma is "," we have to add the argument dec=",".

tenis_with_comma=import(file,dec=",")

# Now we can extract the different data col of a dataframe using as. function
# With this we extract it as a factor vector
as.factor(data$Juega_Tenis)

