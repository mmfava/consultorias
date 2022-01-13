############################################################################################
##                           Análise Fatorial (FA)                                        ##
##            http://wiki.icmc.usp.br/images/b/b5/Factanalysis.pdf/                       ##
##http://carloscollares.blogspot.com.br/2011/01/introducao-analise-fatorial-e-analise.html##
############################################################################################

## A diferença conceitual importante aqui é que na análise de componentes principais 
## a variância a ser considerada para a extração dos fatores é a variância total, e na 
## análise de fatores comuns considera-se apenas a variância comum entre as variáveis.

## Abertura da base de dados e ajuste
dados=read.csv2(file.choose(),header=F,dec=".",sep = ";",strip.white = T)
dados=dados[,-1]
p=ncol(dados)

## Estatísticas descritivas
summary(dados)

## Matriz de correlação
matcor=cor(dados)
print(matcor,digits = 2)
pairs(cbind(dados), pch="+", col="blue")

## Diagramas de correlação
library(corrgram)
corrgram(matcor,type = "cor")
corrgram(matcor,type="cor",lower.panel = panel.shade,
         upper.panel = panel.pie)

## Deve-se realizar o procedimento de extração de fatores em análise fatorial exploratória. 
## Nem todos os fatores são aproveitáveis numa análise fatorial e há controvérsias sobre os 
## critérios que determinam quando um fator é estatisticamente importante.

## Teste de esfericidade de Bartlett: tem como objetivo verificar se há dependência entre colunas e linhas
bartlett.test(dados)

## Critério de Kayser-Meyer-Olkin
Tabela 1. Adequação amostral segundo a medida KMO.
KMO Adequação
> 0,9       Excelente
(0,8; 0,9]  Meritória
(0,7; 0,8]  Intermediária
(0,6; 0,7]  Medíocre
(0,5; 0,6]  Mísera
< 0,5       Inaceitável

## Cálculo do Critério de KMO para a base de dados
library(Rcmdr)
partial.cor=function(X,...)
{
  R <- cor(X, ...)
  RI <- solve(R)
  D <- 1/sqrt(diag(RI))
  Rp <- -RI * (D %o% D)
  diag(Rp) <- 0
  rownames(Rp) <- colnames(Rp) <- colnames(X)
  Rp
}
matcorp <- partial.cor(dados)

idiag <- seq(1, by = p + 1, length = p)
somar2 <- sum((as.numeric(matcor)[-idiag])^2)
cat("\n KMO = ",somar2 / (somar2 + sum((as.numeric(matcorp)[-idiag])^2)))

## A adequação amostral é aceitável (> 0,5) e meritória. As medidas MAA são calculadas para cada
## variável.

for (j in 1:p) {
  somar2j <- sum(matcor[j, -j]^2)
  cat("\n MAA", j, "=", somar2j / (somar2j + sum(matcorp[j, -j]^2)))
}
## Todas as variáveis têm adequação superior a 0,7. 

## A determinação do número de fatores pode também ser facilitada por meio da análise do gráfico 
## de scree plot, técnica advogada por Cattell (1966).
## O scree plot mostra o número de componentes fatoriais extraídos em relação aos autovalores 
## ("eigenvalues") associados a esses fatores. Para entender o melhor o scree plot e o conceito 
## de autovalor, é necessário conhecer o conceito de autovetor ("eigenvector"). Os autovetores 
## de uma matriz de correlações consistem em representações lineares que são identificáveis no 
## gráfico de scatterplot pelos maiores e menores diâmetros da elipse visualizável a partir dos
## pontos formados neste gráfico. 
## Os autovalores são, por sua vez, medidas do comprimento dos autovetores na elipse, ou da figura 
## tridimensional elipticóide - se considerarmos correlações multivariadas. Portanto, ao analisarmos
## os autovalores de um conjunto de dados, consegue-se conhecer de que forma as variâncias da matriz 
## de correlações estão distribuídas. Em outras palavras, é possível visualizar as grandezas da 
## figura elipsóide formada na distribuição espacial das variáveis. O autovalor é calculado pela soma 
## dos quadrados dos carregamentos de cada variável para a variável latente representada pelo fator 
## obtido. O entendimento do conceito de autovalor é facilitado ao lembrarmos dois pontos importantes:
## 1) o carregamento (loading) é o valor do coeficiente de correlação entre a variável e o fator obtido; 
## e 2) o quadrado do coeficiente de qualquer correlação é igual a porcentagem da variância de uma 
## variável que é explicada pela outra (FIELD, 2009).
## Portanto, quando se analisa os autovalores de um conjunto de dados, consegue-se conhecer de que 
## forma as variâncias da matriz de correlações estão distribuídas. Em outras palavras, os autovalores 
## representam o quanto da variância é explicada pelo fator.

## O número de fatores a ser utilizado pode suscitar dúvidas pois é comum haver divergência entre os 
## métodos empregados. É praxe buscar-se o menor número possível de fatores. Geralmente, esse número 
## equivale ao número de fatores anterior ao ponto de inflexão da curva, os quais apresentam autovalores 
## consideravelmente mais elevados à esquerda. Note nessa figura que o único fator com autovalor maior
## que 1,0 é o primeiro, demonstrando que o conjunto de dados é unidimensional.

## Análise fatorial utilizando a análise de componentes principais
acpcor <- prcomp(dados, scale = TRUE)
summary(acpcor)
plot(1:ncol(dados), acpcor$sdev^2, type = "b", xlab = "Componente",
     ylab = "Variância", pch = 20, cex.axis = 1.3, cex.lab = 1.3)

k <- 2
carfat <- acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
colnames(carfat) <- paste("Fator", 1:k, sep = " ")
carfat

## Rotação Varimax
carfatr=varimax(carfat)

## Plot padrão e plot varimax
plot(carfat, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfat, rownames(carfat), adj = 1)
plot(carfatr$loadings, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfatr$loadings, rownames(carfat), adj = 1)


## Exemplo com IRIS

## Matriz de correlação
matcor=cor(iris[-5])
print(matcor,digits = 2)
pairs(cbind(iris[-5]), pch="+", col="blue")

## Diagramas de correlação
library(corrgram)
corrgram(matcor,type = "cor")
corrgram(matcor,type="cor",lower.panel = panel.shade,
         upper.panel = panel.pie)

## Teste de esfericidade de Bartlett: tem como objetivo verificar se há dependência entre colunas e linhas
bartlett.test(iris[-5])

## Critério de Kayser-Meyer-Olkin
Tabela 1. Adequação amostral segundo a medida KMO.
KMO Adequação
> 0,9       Excelente
(0,8; 0,9]  Meritória
(0,7; 0,8]  Intermediária
(0,6; 0,7]  Medíocre
(0,5; 0,6]  Mísera
< 0,5       Inaceitável

## Cálculo do Critério de KMO para a base de dados
library(Rcmdr)
partial.cor=function(X,...)
{
  R <- cor(X, ...)
  RI <- solve(R)
  D <- 1/sqrt(diag(RI))
  Rp <- -RI * (D %o% D)
  diag(Rp) <- 0
  rownames(Rp) <- colnames(Rp) <- colnames(X)
  Rp
}
matcorp <- partial.cor(dados)

idiag <- seq(1, by = p + 1, length = p)
somar2 <- sum((as.numeric(matcor)[-idiag])^2)
cat("\n KMO = ",somar2 / (somar2 + sum((as.numeric(matcorp)[-idiag])^2)))

## A adequação amostral é aceitável (> 0,5) e meritória. As medidas MAA são calculadas para cada
## variável.

for (j in 1:p) {
  somar2j <- sum(matcor[j, -j]^2)
  cat("\n MAA", j, "=", somar2j / (somar2j + sum(matcorp[j, -j]^2)))
}
## Todas as variáveis têm adequação superior a 0,7. 

## Análise fatorial utilizando a análise de componentes principais
acpcor <- prcomp(iris[-5], scale = TRUE)
summary(acpcor)
plot(1:ncol(iris[-5]), acpcor$sdev^2, type = "b", xlab = "Componente",
     ylab = "Variância", pch = 20, cex.axis = 1.3, cex.lab = 1.3)

k <- 2
carfat <- acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
colnames(carfat) <- paste("Fator", 1:k, sep = " ")
carfat

## Rotação Varimax
carfatr=varimax(carfat)

## Plot padrão e plot varimax
plot(carfat, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfat, rownames(carfat), adj = 1)
plot(carfatr$loadings, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfatr$loadings, rownames(carfat), adj = 1)
