############################################################################################
##                           An�lise Fatorial (FA)                                        ##
##            http://wiki.icmc.usp.br/images/b/b5/Factanalysis.pdf/                       ##
##http://carloscollares.blogspot.com.br/2011/01/introducao-analise-fatorial-e-analise.html##
############################################################################################

## A diferen�a conceitual importante aqui � que na an�lise de componentes principais 
## a vari�ncia a ser considerada para a extra��o dos fatores � a vari�ncia total, e na 
## an�lise de fatores comuns considera-se apenas a vari�ncia comum entre as vari�veis.

## Abertura da base de dados e ajuste
dados=read.csv2(file.choose(),header=F,dec=".",sep = ";",strip.white = T)
dados=dados[,-1]
p=ncol(dados)

## Estat�sticas descritivas
summary(dados)

## Matriz de correla��o
matcor=cor(dados)
print(matcor,digits = 2)
pairs(cbind(dados), pch="+", col="blue")

## Diagramas de correla��o
library(corrgram)
corrgram(matcor,type = "cor")
corrgram(matcor,type="cor",lower.panel = panel.shade,
         upper.panel = panel.pie)

## Deve-se realizar o procedimento de extra��o de fatores em an�lise fatorial explorat�ria. 
## Nem todos os fatores s�o aproveit�veis numa an�lise fatorial e h� controv�rsias sobre os 
## crit�rios que determinam quando um fator � estatisticamente importante.

## Teste de esfericidade de Bartlett: tem como objetivo verificar se h� depend�ncia entre colunas e linhas
bartlett.test(dados)

## Crit�rio de Kayser-Meyer-Olkin
Tabela 1. Adequa��o amostral segundo a medida KMO.
KMO Adequa��o
> 0,9       Excelente
(0,8; 0,9]  Merit�ria
(0,7; 0,8]  Intermedi�ria
(0,6; 0,7]  Med�ocre
(0,5; 0,6]  M�sera
< 0,5       Inaceit�vel

## C�lculo do Crit�rio de KMO para a base de dados
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

## A adequa��o amostral � aceit�vel (> 0,5) e merit�ria. As medidas MAA s�o calculadas para cada
## vari�vel.

for (j in 1:p) {
  somar2j <- sum(matcor[j, -j]^2)
  cat("\n MAA", j, "=", somar2j / (somar2j + sum(matcorp[j, -j]^2)))
}
## Todas as vari�veis t�m adequa��o superior a 0,7. 

## A determina��o do n�mero de fatores pode tamb�m ser facilitada por meio da an�lise do gr�fico 
## de scree plot, t�cnica advogada por Cattell (1966).
## O scree plot mostra o n�mero de componentes fatoriais extra�dos em rela��o aos autovalores 
## ("eigenvalues") associados a esses fatores. Para entender o melhor o scree plot e o conceito 
## de autovalor, � necess�rio conhecer o conceito de autovetor ("eigenvector"). Os autovetores 
## de uma matriz de correla��es consistem em representa��es lineares que s�o identific�veis no 
## gr�fico de scatterplot pelos maiores e menores di�metros da elipse visualiz�vel a partir dos
## pontos formados neste gr�fico. 
## Os autovalores s�o, por sua vez, medidas do comprimento dos autovetores na elipse, ou da figura 
## tridimensional eliptic�ide - se considerarmos correla��es multivariadas. Portanto, ao analisarmos
## os autovalores de um conjunto de dados, consegue-se conhecer de que forma as vari�ncias da matriz 
## de correla��es est�o distribu�das. Em outras palavras, � poss�vel visualizar as grandezas da 
## figura elips�ide formada na distribui��o espacial das vari�veis. O autovalor � calculado pela soma 
## dos quadrados dos carregamentos de cada vari�vel para a vari�vel latente representada pelo fator 
## obtido. O entendimento do conceito de autovalor � facilitado ao lembrarmos dois pontos importantes:
## 1) o carregamento (loading) � o valor do coeficiente de correla��o entre a vari�vel e o fator obtido; 
## e 2) o quadrado do coeficiente de qualquer correla��o � igual a porcentagem da vari�ncia de uma 
## vari�vel que � explicada pela outra (FIELD, 2009).
## Portanto, quando se analisa os autovalores de um conjunto de dados, consegue-se conhecer de que 
## forma as vari�ncias da matriz de correla��es est�o distribu�das. Em outras palavras, os autovalores 
## representam o quanto da vari�ncia � explicada pelo fator.

## O n�mero de fatores a ser utilizado pode suscitar d�vidas pois � comum haver diverg�ncia entre os 
## m�todos empregados. � praxe buscar-se o menor n�mero poss�vel de fatores. Geralmente, esse n�mero 
## equivale ao n�mero de fatores anterior ao ponto de inflex�o da curva, os quais apresentam autovalores 
## consideravelmente mais elevados � esquerda. Note nessa figura que o �nico fator com autovalor maior
## que 1,0 � o primeiro, demonstrando que o conjunto de dados � unidimensional.

## An�lise fatorial utilizando a an�lise de componentes principais
acpcor <- prcomp(dados, scale = TRUE)
summary(acpcor)
plot(1:ncol(dados), acpcor$sdev^2, type = "b", xlab = "Componente",
     ylab = "Vari�ncia", pch = 20, cex.axis = 1.3, cex.lab = 1.3)

k <- 2
carfat <- acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
colnames(carfat) <- paste("Fator", 1:k, sep = " ")
carfat

## Rota��o Varimax
carfatr=varimax(carfat)

## Plot padr�o e plot varimax
plot(carfat, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfat, rownames(carfat), adj = 1)
plot(carfatr$loadings, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfatr$loadings, rownames(carfat), adj = 1)


## Exemplo com IRIS

## Matriz de correla��o
matcor=cor(iris[-5])
print(matcor,digits = 2)
pairs(cbind(iris[-5]), pch="+", col="blue")

## Diagramas de correla��o
library(corrgram)
corrgram(matcor,type = "cor")
corrgram(matcor,type="cor",lower.panel = panel.shade,
         upper.panel = panel.pie)

## Teste de esfericidade de Bartlett: tem como objetivo verificar se h� depend�ncia entre colunas e linhas
bartlett.test(iris[-5])

## Crit�rio de Kayser-Meyer-Olkin
Tabela 1. Adequa��o amostral segundo a medida KMO.
KMO Adequa��o
> 0,9       Excelente
(0,8; 0,9]  Merit�ria
(0,7; 0,8]  Intermedi�ria
(0,6; 0,7]  Med�ocre
(0,5; 0,6]  M�sera
< 0,5       Inaceit�vel

## C�lculo do Crit�rio de KMO para a base de dados
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

## A adequa��o amostral � aceit�vel (> 0,5) e merit�ria. As medidas MAA s�o calculadas para cada
## vari�vel.

for (j in 1:p) {
  somar2j <- sum(matcor[j, -j]^2)
  cat("\n MAA", j, "=", somar2j / (somar2j + sum(matcorp[j, -j]^2)))
}
## Todas as vari�veis t�m adequa��o superior a 0,7. 

## An�lise fatorial utilizando a an�lise de componentes principais
acpcor <- prcomp(iris[-5], scale = TRUE)
summary(acpcor)
plot(1:ncol(iris[-5]), acpcor$sdev^2, type = "b", xlab = "Componente",
     ylab = "Vari�ncia", pch = 20, cex.axis = 1.3, cex.lab = 1.3)

k <- 2
carfat <- acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
colnames(carfat) <- paste("Fator", 1:k, sep = " ")
carfat

## Rota��o Varimax
carfatr=varimax(carfat)

## Plot padr�o e plot varimax
plot(carfat, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfat, rownames(carfat), adj = 1)
plot(carfatr$loadings, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfatr$loadings, rownames(carfat), adj = 1)
