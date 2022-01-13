#### CHAMAR OS DADOS ####

dados<-read.csv2(file.choose(),header=T,dec=",") #dados para correlação
attach(dados)
list(dados)
names(dados)
library(ggplot2)

### PRESSUPOSTOS ###

shapiro.test(abund) #não está em normalidade
shapiro.test(s) #em normalidade
shapiro.test(bmwp) #em normalidade
shapiro.test(rur) #em normalidade
shapiro.test(veg) #em normalidade
shapiro.test(temp) #em normalidade
shapiro.test(od) #em normalidade
shapiro.test(ph) #em normalidade
shapiro.test(st) #em normalidade
shapiro.test(cond) #não está em normalidade

### CORRELAÇÃO ###
##Abundância em área rural##
cor(abund,rur,method="spearman") #0.107
cor.test(rur,abund,method="spearman",alternative="less",conf.level=0.95) #p=0.6003 rho=0.107
model1=ggplot(dados,aes(x=rur,y=abund))+
  geom_point(shape=1)+
  geom_smooth(method=lm)+
  theme(axis.title.x=element_text())+
  xlab("% Área Rural")+
  ylab("Abundância de Macroinvertebrados")+
  theme(panel.grid.minor=element_blank())+
  annotate("text",x=25,y=30,label="rho=0.10") #Como a correlação é de Spearman, usa-se rho.
model1  

#O resultado final não foi significativo. A correlação é muito fraca, portanto não pode-se afirmar que a abundância de macroinvertebrados aumenta ou diminui com a área rural

##Abundância em área com vegetação##
cor(abund,veg,method="spearman") #-0.071
cor.test(veg,abund,method="spearman",alternative="greater",conf.level=0.95) #p=0.5799 rho=-0.071
model2=ggplot(dados,aes(x=veg,y=abund))+
  geom_point(shape=1)+
  geom_smooth(method=lm)+
  theme(axis.title.x=element_text())+
  xlab("% Área de Vegetação")+
  ylab("Abundância de Macroinvertebrados")+
  theme(panel.grid.minor=element_blank())+
  annotate("text",x=50,y=30,label="rho=-0.071")
model2

#Resultado final não significativo

##Riqueza em Área Rural##
cor(s,rur) #-0.534
cor.test(rur,s,alternative="less",conf.level=0.95) #p=0.086 r=-0.534
model3=ggplot(dados,aes(x=rur,y=s))+
  geom_point(shape=1)+
  geom_smooth(method=lm)+
  theme(axis.title.x=element_text())+
  xlab("% Área Rural")+
  ylab("Riqueza de Macroinvertebrados")+
  theme(panel.grid.minor=element_blank())+
  annotate("text",x=15,y=20,label="r=-0.534")
model3

#Resultado final não significativo

##Riqueza em Área com Vegetação##
cor(s,veg) #0.541
cor.test(veg,s,alternative="greater",conf.level=0.95) #p=0.083 r=0.541
model4=ggplot(dados,aes(x=veg,y=s))+
  geom_point(shape=1)+
  geom_smooth(method=lm)+
  theme(axis.title.x=element_text())+
  xlab("% Área de Vegetação")+
  ylab("Riqueza de Macroinvertebrados")+
  theme(panel.grid.minor=element_blank())+
  annotate("text",x=50,y=20,label="r=0.541")
model4  

#Resultado final não significativo

##BMWP em Área Rural##
cor(bmwp,rur) #-0.735
cor.test(rur,bmwp,alternative="less",conf.level=0.95) #p=0.018 r=-0.735
model5=ggplot(dados,aes(x=rur,y=bmwp))+
  geom_point(shape=1)+
  geom_smooth(method=lm)+
  theme(axis.title.x=element_text())+
  xlab("% Área Rural")+
  ylab("BMWP")+
  theme(panel.grid.minor=element_blank())+
  annotate("text",x=20,y=30,label="r=-0.735")
model5

#Resultado final foi significativo

##BMWP em Área com Vegetação##
cor(bmwp,veg) #0.734
cor.test(veg,bmwp,alternative="greater",conf.level=0.95) #p=0.02 r=0.734
model6=ggplot(dados,aes(x=veg,y=bmwp))+
  geom_point(shape=1)+
  geom_smooth(method=lm)+
  theme(axis.title.x=element_text())+
  xlab("% Área de Vegetação")+
  ylab("BMWP")+
  theme(panel.grid.minor=element_blank())+
  annotate("text",x=75,y=30,label="r=0.734")
model6

#Resultado final foi significativo 

##BMWP com fatores abióticos##
cor(bmwp,od)
model7=ggplot(dados, aes(x=od,y=bmwp))+
  geom_point(shape=1)+
  geom_smooth(method=lm)+
  theme(axis.title.x=element_text())+
  xlab("OD")+
  ylab("BMWP")+
  theme(panel.grid.minor=element_blank())+
  annotate("text",x=10,y=30,label="r=-0.097")
model7


### GLM ###
##Todas as variáveis##
mod1=glm(formula=bmwp~veg+temp+od+ph+cond+st)
summary(mod1) #nada significativo
AIC(mod1) #71.54

##Sem ph##
mod2=glm(formula=bmwp~veg+temp+od+cond+st)
summary(mod2) #veg*
AIC(mod2) #69.73

##Sem od##
mod3=glm(formula=bmwp~veg+temp+cond+st)
summary(mod3) #veg*
AIC(mod3) #79.16

##Sem temp e ph, mas com od##
mod4=glm(formula=bmwp~veg+od+cond+st)
summary(mod4) #veg,cond,st*
AIC(mod4) #70.97

##Sem temp e od##
mod5=glm(formula=bmwp~veg+cond+st)
summary(mod5) #veg,cond,st*
AIC(mod5) #77.74


##Sem temp e od, com ph##
mod6=glm(formula=bmwp~veg+ph+cond+st)
summary(mod6) #veg,cond,st*
AIC(mod6) #77.11

#Vou usar o Modelo 5, pois é o mais simples, com significância em Vegetação, Condutividade e ST.
#Ou seja, os locais com mais vegetação, maior condutividade e menos sólidos totais, têm maiores valores de BMWP.

### CCA ###
##Chamar o pacote necessário##
library(vegan)

##Chamar os dados##
abundancia<-read.csv2(file.choose(),header=T,dec=",") #abundancia riachos
abiotico<-read.csv2(file.choose(),header=T,dec=",") #cca-abióticos

##Rezar pra dar certo##
vare<-cca(abundancia,abiotico)
vare
plot(vare)
anova(vare)
summary(vare)

#Editar o gráfico#
plot(vare, type="n", las=1, font=6, font.axis=3, font.lab=1, cex.lab=1.5, cex.lab=1.5)
stems <- colSums(abundancia)
orditorp(vare, "sp", priority=stems, pch="+", pcol="red", font=8, cex=1)
text(vare, dis="cn", font=6, cex=0.8)
text(vare, dis="sites", cex=1, font=6)

#Testes#
plot(vare, display=c("sp","cn"))
plot(vare, display=c("wa"),type="n") 
text(vare,display=c("cn"))
text(6,7,"Seu.Nome")
text(c(2,3),c(8,6),c("nome","sobrenome"))
text(locator(1),"Texto")
