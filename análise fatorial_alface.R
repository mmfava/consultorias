## DADOS ALFACE ##

dados <- read.table("acp_alface.txt", header = T)   
dados

dados1 <- dados[,2:7]

dados1


row.names(dados1) = dados[,1]

dados1
attach(dados1)

R <- cor(dados1,dados1)
R

auto <- eigen(R) # autovalores e autovetores
auto
auto$values
auto$values[1:2]

prop_acu <- sum(auto$values[1:2]) / sum(auto$values) # proporção acumulada do 1º e do 2º ACP
prop_acu

## cargas fatoriais pelo m?todo de CP ##

carga1 <- sqrt(auto$values[1]) * auto$vectors[,1]
carga2 <- sqrt(auto$values[2]) * auto$vectors[,2]

carga1 # calculo com base nos auto-valores e auto-vetores para primeira segunda coluna
carga2

L <- cbind(carga1,carga2)
L # matriz de carga fatorial - loading
  # HCO3 fica meio a meio nos dois CP em razão da carga >0.05

com <- carga1^2 + carga2^2
com # comunalidade - é a proporção em que as variáveis são explicadas pelos dois fatores
    #                em conjunto - por exemplo, a HCO3 é explicada em 71,26% pelos dois fatores. 
    #                Ele mostra a eficiência da análise - quanto >%, mais "verdadeiro" é o peso
    #                daquela variável.

plot(carga1,carga2) # pelo gráfico é visivel o "agrupamento" das variáveis para formação das cargas
                    # fatoriais. Neste caso vemos quatro pontos mais associados entre sí e mais pró
                    # ximos ao eixo y. Os outros dois pontos estão proximos do eixo x. 

# Rotação varimax
a=varimax(L, normalize = F) # F é pq estamos trabalhando com a matriz R. Se estivessemos trabalhando
                            # com a matriz S teriamos que normalizada, e seria T.
a # olhar os "loadings" - agora a distinção dos pesos das variáveis para a carga fatorial é mais  
  # distinto.

plot(a$loadings)

########################################################################
# PESQUISA: TESTES EM UMA ESCOLA PREPARAT?RIA, FORAM REALIZADOS,
# COM 220 MENINAS

#VARI?VEIS: X1 = NOTA EM GA?LICO            X2 =  NOTA EM INGL?S
#           X3 = NOTA EM HIST?RIA           X4 = NOTA EM ARITM?TICA
#           X5 = NOTA EM ?LGEBRA            X6 = NOTA EM GEOMETRIA
########################################################################

dados_cor <- c(1, 0.439, 0.410, 0.288, 0.329, 0.248, 0.439, 1, 0.351, 0.354, 0.32,
0.329, 0.41, 0.351, 1, 0.164, 0.19, 0.181, 0.288, 0.354, 0.164, 1, 0.595, 0.47,
0.329, 0.32, 0.19, 0.595, 1, 0.464, 0.248, 0.329, 0.181, 0.47, 0.464, 1)

R <- matrix(dados_cor, ncol = 6, nrow = 6)
R

auto <- eigen(R)
auto

prop_acu <- sum(auto$values[1:2]) / sum(auto$values)
prop_acu

## cargas fatoriais pelo m?todo de CP ##

carga1 <- sqrt(auto$values[1]) * auto$vectors[,1]
carga2 <- sqrt(auto$values[2]) * auto$vectors[,2]

L <- cbind(carga1,carga2)
L

com <- carga1^2 + carga2^2
com

par(mfrow = c(1,2))
plot(carga1,carga2)


## rota??o de 45? sentido anti-hor?rio ##

T <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)), ncol = 2, nrow = 2)
T

LT <- L %*% T
LT

plot(LT[,1],LT[,2])

## rota??o varimax ##

varimax(L, normalize = F)

