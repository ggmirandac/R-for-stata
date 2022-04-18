
# Pregunta 3 -------------------------------------------------------------

mu=5000
cov=0.3
sigma=mu*cov


# 3a ----------------------------------------------------------------------

# busco el promedio

n=1345

mu=mu
sigma_mup=sigma/sqrt(n)

1-pnorm(4950,mean=5000,sd=sigma_mup)



# 3b ----------------------------------------------------------------------

#Aquí hablo de un total

mup=n*mu
sigma_s=sqrt(n)*sigma

-pnorm(6900000,mean=mup,sd=sigma_s)+pnorm(7000000,mean=mup,sd=sigma_s)
