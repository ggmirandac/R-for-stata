# Pregunta 1

mu1 = 30

sigma1 = 0.4*mu1

ra = pnorm(40,mean=mu1,sd=sigma1,lower.tail = FALSE)

rb = qnorm(0.4, mean=mu1,sd=sigma1)

rc = pnorm(36, mean = mu1, sd= sigma1)- pnorm(24, mean = mu1, sd= sigma1)

# Pregunta 2

# Pregunta 3

mu3=60/(qnorm(2/3)*1/3+1)
sigma3=mu3*1/3

r3=qnorm(0.75,mean=mu3,sd=sigma3)

# Pregunta 4

