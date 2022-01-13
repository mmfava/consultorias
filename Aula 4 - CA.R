######################################################################################
##                           Análise Correspondência (CA)                           ##
##                  http://www.statmethods.net/advstats/ca.html                     ##
##                                                                                  ##
######################################################################################

library(ca)
library(vegan)

## Abertura da base de dados (Tabela de contingência)
dados=read.csv2(file.choose(),header=T,row.names=1,dec=".",sep = ";",strip.white = T)
attach(dados)
## Análise de Correspondência
ca(dados)

## Locais
## O componente 1 (x) apresenta maior explicabilidade para os locais
## 2 e 5; o componente 2 (y) apresenta maior explicabilidade para os
## locais 1, 3 e 4.

## Espécies
## O componente 1(x) apresenta maior explicabilidade para as espécies
## 1, 2, 4; o componente 2(y) apresenta maior explibilidade para as
## espécies 3, 5 e 6.

## Há associação das espécies 1, 2 e 4 com os pontos 2 e 5; e há
## associação das espécies 3,5 e 6 com os pontos 1, 3 e 4.

## Plot Simétrico:é a representação simétrica padrão da análise de coorespondência 
## simples, com linhas e colunas representadas por pontos.
## Os pontos que representam as linhas e que se encontram próximos apresentam-se mais similares às 
## características  descritas nas colunas. Contudo, tenha em mente que você não pode interpretar 
## a distância entre os pontos de linha e coluna diretamente.

plot(ca(dados)) 

## Plot Assimétrico: o gráfico assimétrico apresenta as linhas nas coordenadas principais e 
## as colunas são recalculadas como resíduos estandartizados. Adicionalmente, a massa é 
## representada por pontos e as colunas são representadas por flechas. A intensidade do ponto
## (sombreamento) corresponde a contribuição absoluta das linhas. 

plot(ca(dados), mass = TRUE, contrib = "absolute", arrows = c(FALSE, TRUE)) # asymmetric map
data("varespec")

## Exemplo 2
data("varespec")
ca(varespec)
plot(ca(varespec))
plot(ca(varespec), mass = TRUE, contrib = "absolute", arrows = c(FALSE, TRUE)) # asymmetric map
