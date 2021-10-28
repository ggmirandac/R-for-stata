#### Laboratorio 6 ####


# dplyr es un paquete que permite formatear datos para su análisis o visualización

library(rio)
library(dplyr)
data<- import("R-lab-codes/databases/RentasMunich.dta")

# Filter: permite generar subconjuntos de filas de los datos aplicando ciertos filtros.
# primero se entrega la base de datos, y luego los filtros

head(data)
filter(data,bath==1,kitchen==1,location==3)

# Slice: permite seleccionar un subconjunto de filas de los datos según la posición
# en las filas

slice(data,1:20)  # elementos de la fila 1 hasta la 20

# arrange: Permite ordenar el conjunto de datos a partir del orden de sus columnas
# primero se entrega la base de datos y luego las columnas a ordenar.

head(arrange(data, kitchen), n=10)

head(arrange(data, kitchen, bath, location, cheating),
     n=10)
# Al agregar el orden desc() se indica que es en orden descendiente

# Select: permite obtener un subconjunto de datos según sus columnas

head(select(data, kitchen, bath),n=10) # Solo se obtienen las columnas kitchen y bath

# Rename: permite renombrar columnas
 
head(rename(data, renta=rent),n=10) # re renombra rent por renta. x=y donde x es 
# el futuro nombre e y el nombre original
# Entrega una lista nueva

# Distint: permite obtener valores únicos de una variable

distinct(select(data, location)) # los valores únicos de la localización

# Mutate: permite agregar nuevas culiumnas que son funciones de las columnas existentes

head(mutate(data, rentam2=rent/area),n=10)

# Transmute: transmutete hace que queden sólo las nuevas columnas creadas


head(transmute(data, rentam2=rent/area),n=10)

# Summarise: permite obtener reúmenes de datos de una muestra de base de datos

summarise(data, min_renta=min(rent),
media_renta=mean(rent),
max_renta=, 
sd_renta=sd(rent)) # se encuentra una forma resumida de presentar datos

# Sample_n y Sample_frac
# sample_n -> obtener el número de filas aleatorias de la base de datos

sample_n(data,size=10)
# sample_frac -> obtener un porcentaje de filas aleatorias
sample_frac(data,size=0.003)

# Operador Pipe: %>%
# permite realizar múltiples funciones y operaciones dentro de la base de datos.
# como se usa: En primer ligar se entrega la base de datos y luego las operaciones 
# separadas por %>%

data2<- data %>% filter(rent>600)%>% sample_n(size=10)%>%arrange(desc(rent))

# Group_by: permite agrupar datos de una base de colas

resumen<- data %>% group_by(bath,kitchen)

summarise(n=n(), minimo=min(rent), media=mean(rent),
         maximo=max(rent), sd=sd(rent))

# Funciones útiles en R: which() -> ayyda a obtener los índices de las filas con una base de datos que s
# que tengo acá.

# %in%: indica si un valor o componente de un vector se encuentra dentro de los valores de otro

# Ejemplo:

which(data$bath & data$kitchen)
head(data$location %in% c(2, 3),n, rent=3082)

