
## TRABALHO GEOESTATÍSTICA 1
## MARÍLIA M. FAVALESSO

## -- Pacotes para a análise de dados

# Requerimento do pacote de dados
library(geoR)  
library(e1071) 
library(stats)
library(MASS)
library(moments)
library(graphics)
require(gstat)
library(sp)
require(classInt) 

## A) Análise exploratória completa dos dados e interpretação dos resultados:

## a - abrir os arquivo do diretório com valores variável resposta e 
##     respectiva coordenada desta. Aparentemente as coordenadas já
##     estão em KM.
k= read.geodata("potassio-espaço.txt",head=T,coords.col=2:3,data.col=4) 
k


## b - coordenadas de metros para km (com a finalidade de melhorar os ajustes)
dnewcoord=(k$coords)/1000
k$coords=dnewcoord
k$coords
k

## c - selecionar somente os valores da variável resposta
k$data 

## d - Estatística descritiva dos dados
summary(k$data) # min, 1Q, 2Q, média, 3Q, maximo
boxplot(k$data, ylab="Valores de K") # representação da distribuição dos dados
var(k$data) # Variância de K
sd(k$data) # desvio-padrão de K
CV = sd(k$data)*100/mean(k$data) 
CV # Coeficiente de variação = 33,73% - Dados heterogêneos
skewness(k$data) # Calculo da assimetria dos dados 
kurtosis(k$data) # Calculo da curtose = 0,01 -- > platicúrtica
plot(k) # Gráfico plot para mostrar tendência direcional
agostino.test(k$data, alternative = "two.sided") # agostinho para teste de  assimentria
anscombe.test(k$data, alternative = "two.sided") # ascombe para testar a curtose
hist(k$data, xlab=NULL, ylab="FR%", main=NULL) # Histograma para visualizar a distribuição dos dados
shapiro.test(k$data)

## B) Análise exploratória espacial do K. 
## - Existem pontos discrepantes? Onde estão?

## b - abrir arquivo borda
borda=read.table("borda.txt")
borda

## c - Coordenadas de metros para km na borda
borda
bordanewV1=(borda$V1)/1000
borda$V1=bordanewV1
borda$V1
borda

bordanewV2=(borda$V2)/1000
borda$V2=bordanewV2
borda$V2
borda
dev.off()
# b - Post-plot para o estudo de tendência direcional (Classifica por quartis)
p=points(k,pt.div="quartile",col=c("yellow","green","red","blue"),main=NULL,
       xlab="Coordenadas X", ylab="Coordenadas Y")
p
legend(239.3, 7238.5,c("Min - Q1","Q1 - Mediana","Mediana - Q3","Q3 - Max"),
       fill=c("yellow", "green", "red", "blue"), cex=1)
polygon(borda, lwd=1)
text(239.4, 7238.5,expression(bolditalic(NA%up%N)),cex=3)

points(k, pt.div= "quint", cex.max=1, cex.min=1, bord=borda)
points(k, pch="+")

# D - gráfico h-scatterplot (mede a variância ent

hscat=read.csv2(file.choose(),dec=".",header=TRUE,strip.white=TRUE)
hscat
coordinates(hscat) = ~x+y
hscat(log(hscat$K)~1, hscat, breaks=c(0.1:0.8)*100)
hscat(log(K)~1, hscat, c(0.1: 0.8)*100, as.table=T, mirror=T)


# E - Existe anisotropia?

dev.off()
max(dist(k$coords)) # maior distância
min(dist(k$coords))
(max(dist(k$coords))-min(dist(k$coords)))/3
max(dist(k$coords)/2) # cutoff de 50% = 0.88 km

plot(variog4(k,l=1,max.dist= 0.88, xnames="Distâncias", ylab="γ (h)")) # Variograma com cutoff de 50%
v=variog4(k,l=1,max.dist= 0.88, pairs.min = 30)       # l refere-se a transformação dos dados, que não fiz.
plot(v, xlab="Distâncias", ylab="γ (h)", legend=F)
variog4(k,l=1,max.dist= 0.88) 

# semivariograma experimental ondimensioal de Matheron,
# com 10 semivariâncias e com 30 pares minimo
# inicia e 0 e vai até 88
k.var<- variog(k,uvec=seq(0,0.88,l=14), estimator.type="classical",pairs.min=30) 
variog(k,uvec=seq(0,0.88,l=14))
k.var # semivariograma - calculo para construção do gráfico
plot(k.var, main= '', xlab="Distância", ylab="γ (h)") # visualização de gráfico plot - existe pouca variação espacial
# patamar não aparece - sem estabilidade
# efeito pepita >0.003

### Informações do semivariograma experimental construído
distancia <-  k.var$u 
semivariancia <- k.var$v
pares <- k.var$n
tabela <- cbind(distancia,semivariancia,pares)
tabela
attach(tabela)

## Autocorrelação espacial
semivariancia/var(k$data) # Autocorrelação espacial a diferentes distâncias
# é só dividir a semivariância (data "tabela") pela
# variância dos dados.
autorrelação=semivariancia/var(k$data)
tabela=cbind(distancia,semivariancia, autorrelação,pares)
tabela
attach(tabela)


# I - Construção de modelos teóricos ao semivariograma experimental utilizando os modelos expo...

var(k$data)-0.0018 #0.001743856
0.0038-0.0018 # 0.0022
# 1. Modelo exponencial
plot(k.var,xlab='Distancia',ylab="γ (h)",main='Modelo exponencia') 
dexp.ols<-variofit(k.var,ini=c(0.0017,0.15),weights= "equal",cov.model="exp") 
dexp.ols 
lines(dexp.ols,col="BLUE")
summary(dexp.ols)

round(0.001960083, 4) # 0,002
round(0.001657349,4) # 0,0017
round(0.001960083+0.001657349, 4) #  0,0036
round( 0.149874487,4) # 0,1499


# 2. Modelo gauss
plot(k.var,xlab='Distância',ylab='semivariância',main='Semivariograma dgaus.ols') 
dgaus.ols<-variofit(k.var,ini=c(0.0017,0.15),weights= "equal",cov.model="gaus") 
dgaus.ols 
lines(dgaus.ols,col="red")
summary(dgaus.ols)

round(0.002082397 , 4) # 0,0021
round(0.001478663, 4) # 0,0015
round(0.002082397+0.001478663,4) # 0,0036
round(0.149777355, 4) # 0,1498

# 3. Modelo Matérn k = 1
plot(k.var,xlab='Distância',ylab='semivariância',main='Semivariograma dmatern1.ols') 
dmatern1.ols<-variofit(k.var,ini=c(0.0017,0.15),weights= "equal",cov.model= "matern", kappa = 1) 
dmatern1.ols
lines(dmatern1.ols,col="black")
summary(dmatern1.ols)

round(0.0026420266, 4) # 0,0026
round(0.0009599176, 4) # 0,0026
round(0.0026420266+ 0.0009599176, 4) # 0,0036
round(0.1499617354, 4)# 0,15

# 3. Modelo Matérn k = 1.5
plot(k.var,xlab='Distância',ylab='semivariância',main='Semivariograma dmatern1.ols') 
dmatern15.ols<-variofit(k.var,ini=c(0.0017,0.15),weights= "equal",cov.model= "matern", kappa = 1.5) 
dmatern15.ols
lines(dmatern15.ols,col="black")
summary(dmatern15.ols)

round(0.0028456714, 4) # 0,0028
round(0.0006955084, 4) # 0,0007
round(0.0028456714+ 0.0006955084, 4) # 0,0035
round(0.1499695788 , 4) # 0,15

# 4. Modelo Matérn k = 2.5
plot(k.var,xlab='Distância',ylab='semivariância',main='Semivariograma dmatern1.ols') 
dmatern25.ols<-variofit(k.var,ini=c(0.0017,0.15),weights= "equal",cov.model= "matern", kappa = 2.5) 
dmatern25.ols
lines(dmatern25.ols,col="black")
summary(dmatern25.ols)
dmatern25.ols$tausq  

round(0.0030839985, 4) # 0,0031
round(0.0005370634, 4) # 0,0005
round(0.0030839985 + 0.0005370634, 4) # 0,0036
round(0.1499840296, 4) # 0,15

## Semivariogramas com envelope

par(mfrow=c(2,3))
# Envelopes para o Exponencial
k.var.env.mod<-variog.model.env(k,model.pars= dexp.ols, obj.variog= k.var)  
plot(k.var,env= k.var.env.mod,xlab='Distância',ylab="γ (h)",main='Modelo Exponencial') 
lines(dexp.ols,col="BLUE")

# Envelopes para o Gaussiano
k.var.env.mod<-variog.model.env(k,model.pars= dgaus.ols, obj.variog= k.var)  
plot(k.var,env= k.var.env.mod,xlab='Distância',ylab="γ (h)",main='Modelo Gaussiano') 
lines(dgaus.ols,col="red")

# Envelopes para o modelo da família Matérn com k=1
k.var.env.mod<-variog.model.env(k,model.pars= dmatern1.ols, obj.variog= k.var)  
plot(k.var,env= k.var.env.mod,xlab='Distância',ylab="γ (h)",main='Família Matérn, k=1') 
lines(dmatern1.ols,col="black")

# Envelopes para o modelo da família Matérn com k=1.5
k.var.env.mod<-variog.model.env(k,model.pars= dmatern15.ols, obj.variog= k.var)  
plot(k.var,env= k.var.env.mod,xlab='Distância',ylab="γ (h)",main='Família Matérn, k=1.5')
lines(dmatern15.ols,col="black")

# Envelopes para o modelo da família Matérn com k=2.5
k.var.env.mod<-variog.model.env(k,model.pars= dmatern25.ols, obj.variog= k.var)  
plot(k.var,env= k.var.env.mod,xlab='Distância',ylab="γ (h)",main='Família Matérn, k=2.5')
lines(dmatern25.ols,col="black")

dev.off()

# J - Qual o melhor ajuste segundo o método de validação cruzada?

# exponencial
vcdexpols=xvalid(k,model=dexp.ols)
EAexpols=sum(abs(vcdexpols$predicted-vcdexpols$data))
EAexpols
summary(vcdexpols)
round(-0.0050100, 4) # -0.005
round(1.07294439, 4) # 1.07

# gauss
vcdgausols=xvalid(k,model=dgaus.ols)
EAgausols=sum(abs(vcdgausols$predicted-vcdgausols$data))
EAgausols
summary(vcdgausols)
round(-0.007710, 4)
round(1.08082701, 4)

library(stats)

# Matérn 1
vcdmatern1ols=xvalid(k,model=dmatern1.ols)
EAmatern1ols=sum(abs(vcdmatern1ols$predicted-vcdmatern1ols$data))
EAmatern1ols
summary(vcdmatern1ols)
round(-0.0030940, 4)
round(1.07116812, 4)

# Matérn 1.5
vcdmatern1ols=xvalid(k,model=dmatern15.ols)
EAmatern15ols=sum(abs(vcdmatern1ols$predicted-vcdmatern1ols$data))
EAmatern15ols
summary(vcdmatern1ols)
round(-0.002135, 4)
round(1.07201335, 4)

# Matérn 2.5
vcdmatern25ols=xvalid(k,model=dmatern25.ols)
EAmatern25ols=sum(abs(vcdmatern1ols$predicted-vcdmatern1ols$data))
EAmatern25ols
summary(vcdmatern25ols)
round(-0.0014790, 4)
round(1.05110588, 4)

# K -Construir por krigagem ordinário um mapa do nível K, considerando cinco classes de igual amplitude.
# Calcular a área e a porcentagem de cada classe. Interpretar os resultados. 

## interpolando
require(splancs)  

apply(borda,2,range)
gr<-expand.grid(x=seq(239.1828, 241.0279, by=0.005),y=seq(7236.744, 7238.593, by=0.005)) 
points(k, pt.div= "quint", cex.max=1, cex.min=1, bord=borda, lam=0)
points(k, pch="+")

gi <- polygrid(gr, bor=borda)
points(gi, pch="+", col=3)

KC <- krige.control(obj.mo=dmatern25.ols, lam=1)
ctc.k <- krige.conv(k, loc=gi, krige=KC)

require(classInt)

valores<-ctc.k$predict

classIntervals(ctc.k$predict, 5, style="equal", intervalClosure="right")
class=c("int1"=11171,"int2"= 11695, "int3"= 9440, "int4"= 19905, "int5"=14724)
class
sum(class)
ccc=round((prop.table(class)),4)
ccc
ccc*167.35
sum(ccc*167.35)
image(ctc.k, loc=gr, border=borda, col=gray(seq(1,0,l=5)), zlim=range(ctc.k$predict), xlab="Coord x", ylab="Coord Y")
text(40.8,37.8,expression(bolditalic(NA%dblup%N)),cex=2.5)
legend("bottomright", fill=gray(c(1,0.8,0.6,0.4,0.2,0)),
       c("0,1543 |-- 0,1625","0,1625 |-- 0,1706","0,1706 |-- 0,1787","0,1787 |-- 0,1868",
         "0,1868 |-- 0,1950"),cex=1)

classIntervals(ctc.k$predict, 6, style="equal", intervalClosure="right")
image(ctc.k, loc=gr, border=borda, col=gray(seq(1,0,l=5)), 
      breaks=c(0.1543,0.1611,0.1679, 0.1747,0.1814, 0.1882), xlab="Coord x", ylab="Coord Y")
text(40.8,37.8,expression(bolditalic(NA%dblup%N)),cex=2.5)
legend("bottomright", fill=gray(c(1,0.8,0.6,0.4,0.2,0)),
       c("0,1543 |-- 0,1611","0,1611 |-- 0,1679","0,1679|-- 0,1746","0,1746 |-- 0,1814",
         "0,1814 |-- 0,1882", "0,1882 |--0,1950 "),cex=1)


classIntervals(ctc.k$predict, 4, breaks=c(0, 0.10, 0.20, 0.30, 0.40), intervalClosure="right")
image(ctc.k, loc=gr, border=borda, col=gray(seq(1,0,l=4)), 
      breaks=c(0, 0.10, 0.20, 0.30, 0.40), xlab="Coord x", ylab="Coord Y")
text(40.8,37.8,expression(bolditalic(NA%dblup%N)),cex=2.5)
legend("bottomright", fill=gray(c(1,0.8,0.6,0.4,0.2,0)),
       c("0 |--11","11 |-- 0,20","0,20|-- 0,30","0,30 |-- 0,40"),cex=1, main="COODETEC")


