######################################################################################
##                           An�lise Correspond�ncia (CA)                           ##
##                  http://www.statmethods.net/advstats/ca.html                     ##
##                                                                                  ##
######################################################################################

library(ca)
library(vegan)

## Abertura da base de dados (Tabela de conting�ncia)
dados=read.csv2(file.choose(),header=T,row.names=1,dec=".",sep = ";",strip.white = T)
attach(dados)
## An�lise de Correspond�ncia
ca(dados)

## Locais
## O componente 1 (x) apresenta maior explicabilidade para os locais
## 2 e 5; o componente 2 (y) apresenta maior explicabilidade para os
## locais 1, 3 e 4.

## Esp�cies
## O componente 1(x) apresenta maior explicabilidade para as esp�cies
## 1, 2, 4; o componente 2(y) apresenta maior explibilidade para as
## esp�cies 3, 5 e 6.

## H� associa��o das esp�cies 1, 2 e 4 com os pontos 2 e 5; e h�
## associa��o das esp�cies 3,5 e 6 com os pontos 1, 3 e 4.

## Plot Sim�trico:� a representa��o sim�trica padr�o da an�lise de coorespond�ncia 
## simples, com linhas e colunas representadas por pontos.
## Os pontos que representam as linhas e que se encontram pr�ximos apresentam-se mais similares �s 
## caracter�sticas  descritas nas colunas. Contudo, tenha em mente que voc� n�o pode interpretar 
## a dist�ncia entre os pontos de linha e coluna diretamente.

plot(ca(dados)) 

## Plot Assim�trico: o gr�fico assim�trico apresenta as linhas nas coordenadas principais e 
## as colunas s�o recalculadas como res�duos estandartizados. Adicionalmente, a massa � 
## representada por pontos e as colunas s�o representadas por flechas. A intensidade do ponto
## (sombreamento) corresponde a contribui��o absoluta das linhas. 

plot(ca(dados), mass = TRUE, contrib = "absolute", arrows = c(FALSE, TRUE)) # asymmetric map
data("varespec")

## Exemplo 2
data("varespec")
ca(varespec)
plot(ca(varespec))
plot(ca(varespec), mass = TRUE, contrib = "absolute", arrows = c(FALSE, TRUE)) # asymmetric map
