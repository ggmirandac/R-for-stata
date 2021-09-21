cafavA<-choose(4,3)*choose(8,3)+choose(4,2)*choose(2,2)*choose(6,2)
castotS<-choose(12,6)

prob<-cafavA/castotS
print(prob)
###############################################################
DataI1_1 <- read_excel("R-studying/Ensayos i1/Ensayo 2/DataI1-1.xlsx")

sept_maipu<-dplyr::filter(DataI1_1,Mes==9,Comuna=="Maipú")$Energia


### median(sept_maipu)=log(2)/lambda
lambda=log(2)/median(sept_maipu)

qexp(0.45, rate = lambda)/quantile(sept_maipu,prob=0.45)
## qexp calcula la el percentil dado p con un lambda dado por rate


#####
DataI1_1 <- rio::import("R-studying/Estudio i1/Ensayo 2/DataI1-1.xlsx")
sep_mai<-dplyr::filter(DataI1_1,Mes==9, Comuna=="Maipú")$Energia
lambda <- log(2)/median(sep_mai)

xp <- qexp(p=0.45,rate=lambda)
ener45 <- quantile(sep_mai,prob=0.45)

print(xp/ener45)
