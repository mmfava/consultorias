############################################################################# 
##       M�TODO DOS M�NIMOS QUADRADOS E CORRELA��O DE PEARSON              ##
##                           Profa. Ana Tereza                             ##
##                           setembro de 2014                              ##
#############################################################################

## O Coeficiente de correla��o de Pearson � uma estat�stica que determina a 
## rela��o entre duas vari�veis quantitativas. 

## Como todas as estat�sticas, ela tamb�m tem restri��es quanto ao uso:
## Normalidade dos dados: caso os dados estejam em  normalidade, � poss�vel 
## fazer o c�lculo do coeficiente de correla��o de Pearson, assim como aplicar
## o m�todo dos m�nimos quadrados.
## Caso os dados n�o estejam em normalidade, voc� deve fazer uma correla��o 
## n�o param�trica, denominada Correla��o de Spearman.

## EXEMPLO DAS FAVAS

Peso=c(0.7,1.2,0.9,1.4,1.2,1.1,1,0.9,1,0.8)
Comprimento=c(1.7,2.2,2,2.3,2.4,2.2,2,1.9,2.1,1.6)


## Verifica��o da normalidade
shapiro.test(Comprimento)
shapiro.test(Peso)


## Teste de signific�ncia da correla��o
cor.test(Comprimento,Peso,
         alternative=c("greater"),
         method="pearson",
         conf.level=0.95)


## Modelo dos M�nimos Quadrados
library(ISwR)

lm(Peso~Comprimento)

## � poss�vel verificar se os coeficientes do modelo s�o significativos (p<0,05)
## ou n�o. Para isso, continuaremos a usar a fun��o lm (linear model).

summary(lm(Peso~Comprimento))


## Diagrama de dispers�o e demonstra��o da reta de regress�o (obtida pelo 
## m�todo dos m�nimos quadrados)
plot(Comprimento,Peso,
     pch=1,
     ylab="Peso (g)",
     xlab="Comprimento (cm)")

abline(lm(Peso~Comprimento))


## Mas, como estamos fazendo uma modelagem, � importante avaliar o quanto
## este modelo explica os dados observados. Sendo assim, devemos fazer uma
## An�lise de Res�duos em rela��o aos preditos.

modelo=lm(Peso~Comprimento)

predito=fitted(modelo) ## Valores estimados a partir do modelo
predito

residuos=resid(modelo) ## Dist�ncia entre os valores estimados e os observados
residuos

plot(Comprimento,Peso)

abline(lm(Peso~Comprimento))

segments(Comprimento,predito,Comprimento,Peso)


## CONFIRMA��O DO EXERC�CIO DE ONTEM ######

altura=c(1.60,1.68,1.54,1.76,1.64)
pressao=c(10,9.1,12.2,14.8,12.0)


## Verifica��o da normalidade
shapiro.test(altura)
shapiro.test(pressao)


## Teste de signific�ncia da correla��o
cor.test(altura,pressao,
         alternative=c("greater"),
         method="pearson",
         conf.level=0.95)


## Modelo dos M�nimos Quadrados
library(ISwR)

lm(Peso~Comprimento)

## � poss�vel verificar se os coeficientes do modelo s�o significativos (p<0,05)
## ou n�o. Para isso, continuaremos a usar a fun��o lm (linear model).

summary(lm(Peso~Comprimento))


## Diagrama de dispers�o e demonstra��o da reta de regress�o (obtida pelo 
## m�todo dos m�nimos quadrados)
plot(altura,pressao,
     pch=1,
     ylab="Press�o (cmHg)",
     xlab="Altura (m)")

abline(lm(pressao~altura))










## EXERC�CIO 1: Exemplo de soja
area=c(411,650,471,393,427,431,492,371,470,419,407,489,439)
weigth=c(2,2.47,2.11,1.89,2.05,2.30,2.46,2.06,2.25,2.07,2.17,2.32,2.12)

plot(area,weigth,
     pch=1,
     ylab="weigth",
     xlab="area")
     
abline(h=mean(weigth),col="blue")
abline(v=mean(area),col="blue")

shapiro.test(area)
shapiro.test(weigth)

cor.test(weigth,area,
         alternative=c("greater"),
         method="spearman",
         conf.level=0.95,
         exact=FALSE)

## A correla��o linear de Pearson e de Spearman s�o comutativas em rela��o
## as vari�veis, ou seja, a cor(A,B)==cor(B,A)
## O diagrama de dispers�o segue os princ�pios cartesianos, onde o eixo x recebe
## a vari�vel independente e o eixo y recebe a vari�vel dependente.



## EXERC�CIO 2 - Exemplo dos grilos

Cricrilos=c(882,1188,1104,864,1200,1032,960,900)
Temp=c(69.7,93.3,84.3,76.3,88.6,82.6,71.6,79.6)

plot(Temp,Cricrilos,
     pch=1,
     ylab="Cricrilos",
     xlab="Temperatura (F)")
abline(h=mean(Cricrilos),col="blue")
abline(v=mean(Temp),col="blue")

shapiro.test(Cricrilos)
shapiro.test(Temp)
