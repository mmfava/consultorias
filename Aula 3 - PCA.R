######################################################################################
##                An?lise de Componentes Principais (PCA)                           ##
##http://www.instantr.com/2012/12/18/performing-a-principal-component-analysis-in-r/##
##                  http://rpubs.com/sinhrks/plot_pca                               ##
######################################################################################

## An?lise de componentes principais ? uma forma de de simplificar uma base de
## dados multivariada. Esta an?lise auxilia na representa??o de uma vari?vel
## estat?stica reduzida, baseada na varia??o de um banco de dados complexo.

## O banco de dados deve conter apenas vari?veis quantitativas. Se a base de
## dados contiver uma vari?vel qualitativa (n?o-num?rica), voc? deve exclui-la.

## A fun??o que usaremos a 'princomp', a qual calcular? os desvios-padr?o dos
## componentes principais. Entretanto, h? outras informa??es que devem ser
## geradas em uma PCA, como por exemplo, as cargas fatoriais e os escores 
## padronizados. 

library(vegan)
## Banco de dados - Iris
data(iris)
iris
summary(iris)
attach(iris)
names(iris)
shapiro.test(Sepal.Length)
shapiro.test(Sepal.Width)
shapiro.test(Petal.Length)

## An?lise de Componentes Principais
iris.pca=princomp(pres.sa.v.na)


## Resultados da an?lise: visualiza a propor??o da vari?ncia total explicativa
## de cada componente principal.
summary(iris.pca)
## Standard deviation = Autovalor
## Proportion of Variance = o quanto cada componente explica a varia??o dos dados
## Cumulative Proportion = % acumulada de explicabilidade de todos os fatores

## Cargas fatoriais: coeficientes das combina??es lineares das vari?veis con-
## t?nuas.
iris.pca$loadings

## A PARTIR DAQUI OCORRE A DIFERENCIA??O ENTRE PCA E AN?LISE FATORIAL
## Escores Padronizados
iris.pca$scores

## M?todo Plot: este plot representa as vari?ncias (y) associadas com os 
## componentes principais (x). ? um m?todo interessante para decidir quantos
## componentes principais ser?o utilizados na an?lise. 
plot(iris.pca)

## Crit?rio de Broken-Stick: ser?o selecionados os componentes principais que 
## apresentarem autovalores superiores a 1.

## Diagrama de ordena??o: representa??o gr?fica da PCA
library(ggfortify)
autoplot(iris.pca)

## Neste exemplo, o diagrama resultou apenas os valores num?ricos. Se voc?
## quiser colorir os dados n?o-num?ricos da base de dados original, podemos
## utilizar o comando 'colour'.
autoplot(iris.pca, data=iris, colour = 'Species')

## Se utilizarmos o comando 'label=TRUE', colocaremos o n?mero do objeto.
autoplot(iris.pca, data=iris, colour = 'Species', label=TRUE)

## Podemos tamb?m apresentar os autovetores. 
autoplot(iris.pca, data=iris, colour = 'Species', loadings=TRUE)

## Podemos nomear os autovetores e modificar suas cores.
autoplot(iris.pca, data = iris, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)



library(vegan)
ordiplot(iris.pca,type="n")
orditorp(iris.pca,display="species",col="red",air=0.01)
orditorp(iris.pca,display="sites",cex=0.5,air=0.01)

especie=c(rep("setosa",50),rep("veriscolor",50),rep("virginica",50))
ordiplot(iris.pca,type="n")
ordihull(iris.pca,groups=especie,draw="polygon",col="grey90",label=F)
orditorp(iris.pca,display="species",col="red",air=0.01)
orditorp(iris.pca,display="sites",col=c(rep("red",50),rep("green",50),rep("blue",50)),
         air=0.01,cex=0.4)

autoplot(iris.pca, data=iris, colour = 'Species', loadings=TRUE)

## Podemos nomear os autovetores e modificar suas cores.
autoplot(iris.pca, data = iris, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

