############################################################################# 
##       MÉTODO DOS MÍNIMOS QUADRADOS E CORRELAÇÃO DE PEARSON              ##
##                           Profa. Ana Tereza                             ##
##                           setembro de 2014                              ##
#############################################################################

## O Coeficiente de correlação de Pearson é uma estatística que determina a 
## relação entre duas variáveis quantitativas. 

## Como todas as estatísticas, ela também tem restrições quanto ao uso:
## Normalidade dos dados: caso os dados estejam em  normalidade, é possível 
## fazer o cálculo do coeficiente de correlação de Pearson, assim como aplicar
## o método dos mínimos quadrados.
## Caso os dados não estejam em normalidade, você deve fazer uma correlação 
## não paramétrica, denominada Correlação de Spearman.

## EXEMPLO DAS FAVAS

Peso=c(0.7,1.2,0.9,1.4,1.2,1.1,1,0.9,1,0.8)
Comprimento=c(1.7,2.2,2,2.3,2.4,2.2,2,1.9,2.1,1.6)


## Verificação da normalidade
shapiro.test(Comprimento)
shapiro.test(Peso)


## Teste de significância da correlação
cor.test(Comprimento,Peso,
         alternative=c("greater"),
         method="pearson",
         conf.level=0.95)


## Modelo dos Mínimos Quadrados
library(ISwR)

lm(Peso~Comprimento)

## É possível verificar se os coeficientes do modelo são significativos (p<0,05)
## ou não. Para isso, continuaremos a usar a função lm (linear model).

summary(lm(Peso~Comprimento))


## Diagrama de dispersão e demonstração da reta de regressão (obtida pelo 
## método dos mínimos quadrados)
plot(Comprimento,Peso,
     pch=1,
     ylab="Peso (g)",
     xlab="Comprimento (cm)")

abline(lm(Peso~Comprimento))


## Mas, como estamos fazendo uma modelagem, é importante avaliar o quanto
## este modelo explica os dados observados. Sendo assim, devemos fazer uma
## Análise de Resíduos em relação aos preditos.

modelo=lm(Peso~Comprimento)

predito=fitted(modelo) ## Valores estimados a partir do modelo
predito

residuos=resid(modelo) ## Distância entre os valores estimados e os observados
residuos

plot(Comprimento,Peso)

abline(lm(Peso~Comprimento))

segments(Comprimento,predito,Comprimento,Peso)


## CONFIRMAÇÃO DO EXERCÍCIO DE ONTEM ######

altura=c(1.60,1.68,1.54,1.76,1.64)
pressao=c(10,9.1,12.2,14.8,12.0)


## Verificação da normalidade
shapiro.test(altura)
shapiro.test(pressao)


## Teste de significância da correlação
cor.test(altura,pressao,
         alternative=c("greater"),
         method="pearson",
         conf.level=0.95)


## Modelo dos Mínimos Quadrados
library(ISwR)

lm(Peso~Comprimento)

## É possível verificar se os coeficientes do modelo são significativos (p<0,05)
## ou não. Para isso, continuaremos a usar a função lm (linear model).

summary(lm(Peso~Comprimento))


## Diagrama de dispersão e demonstração da reta de regressão (obtida pelo 
## método dos mínimos quadrados)
plot(altura,pressao,
     pch=1,
     ylab="Pressão (cmHg)",
     xlab="Altura (m)")

abline(lm(pressao~altura))










## EXERCÍCIO 1: Exemplo de soja
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

## A correlação linear de Pearson e de Spearman são comutativas em relação
## as variáveis, ou seja, a cor(A,B)==cor(B,A)
## O diagrama de dispersão segue os princípios cartesianos, onde o eixo x recebe
## a variável independente e o eixo y recebe a variável dependente.



## EXERCÍCIO 2 - Exemplo dos grilos

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
