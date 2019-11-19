##Teste de Mantel



############################################
##Lendo os dados:

df <- read.csv("C:/Users/NinaScarpelli/Documents/Masters_Project/Cap2/Pontos_ind_metricas.csv", header = T)

attach(df)

str(df)

##############################################
library(vegan)


espcies<-dados[,9:40]
dist.jac<-vegdist(esp?cies, method="jaccard", binary=T)

ambiente<-dados[,3:6]
ambiente.pad<-decostand(ambiente, method="standardize")
dist.amb<-vegdist(ambiente.pad, method="euclid")


mantel(dist.amb, dist.jac)