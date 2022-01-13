dados=read.csv2(file.choose(),header=TRUE,dec=".",sep=";")
attach(dados)
library(ExpDes.pt)
library(plyr)
library(ggplot2)
names(dados)

fat2.dic(Grupos1,aplicaçao1,g.por.dia1,quali=c(T,T),mcomp="tukey",fac.names=c("Grupos","Aplicação"),
         sigT=0.05, sigF=0.05)

interaction.plot(Grupos1, aplicaçao1, g.por.dia1)

aplic1=data.frame(ddply(dados,~Grupos1*aplicaçao1,summarise,mean=mean(g.por.dia1),sd=(sd(g.por.dia1))))
aplic1

pd <- position_dodge(.3)
dados$Grupos1=factor(dados$Grupos1)
dados$aplicaçao1=factor(dados$aplicaçao1)

ggplot(aplic1,aes(x=Grupos1,y=mean, colour=aplicaçao1, group=aplicaçao1))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width =.2, size=0.25,
                colour="black", position= pd)+geom_line(position=pd)+
  geom_point(position=pd, size=1)+ylab("ganho de peso por gramas/dia")
