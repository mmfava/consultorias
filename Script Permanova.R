####################PERMANOVA
library(vegan)
?adonis ##nome no R


###Abrir a planilha de bióticos e das categorias

x<-read.csv2(file.choose(),header=T, dec=',',sep=";")
x
detach(x)
names(x)
cat<-read.csv2(file.choose(),header=T, dec=',',sep=";")# categorias devem estar juntas na mesma planilha
cat
attach(cat)
names(cat)

spp.per1<-adonis(x~Ponto,method='bray')#um fator
spp.per1

spp.per2<-adonis(x~Mês,method='bray')#um fator
spp.per2

spp.per3<-adonis(x~cat$Ponto*cat$Mês,method='bray', permutation=999) #dois fatores com interação
spp.per3


