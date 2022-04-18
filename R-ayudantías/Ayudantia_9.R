library(readxl)
ENS_Muestra <- read_excel("R-ayudantías/ENS_Muestra.xlsx")

colesterol <- ENS_Muestra$COLESTEROL
imc <- ENS_Muestra$IMC

mean(colesterol>180 & 20<imc & imc<30)

mu_vector <- c(mean(colesterol),mean(imc))
SIGMA <- cov(cbind(colesterol,imc))

mvtnorm::pmvnorm(lower = c(180,20),upper = c(Inf,30),mean = mu_vector, sigma=SIGMA)

Pi
