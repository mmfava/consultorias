######################################################################################
##                Testes de Normalidade Multivariada                                ##
##      https://cran.r-project.org/web/packages/MVN/MVN.pdf                         ##
##                                                                                  ##
######################################################################################

## Henze-Zirkler's Multivariate Normality Test
library(MVN)
result=hzTest(dados)
result
setosa = iris[1:50, 1:4] # Iris data only for setosa and four variables
result = hzTest(setosa, qqplot = TRUE)
result

versicolor=iris[51:100,1:4]
result.versicolor = hzTest(versicolor, qqplot = TRUE)
result.versicolor

virginica=iris[101:150,1:4]
result.virginica = hzTest(virginica, qqplot = TRUE)
result.virginica


## Class "mardia"
setosa = iris[1:50, 1:4] # Iris data only for setosa and four variables
result = mardiaTest(setosa, qqplot = TRUE)
result

versicolor=iris[51:100,1:4]
result.versicolor = mardiaTest(versicolor, qqplot = TRUE)
result.versicolor

virginica=iris[101:150,1:4]
result.virginica = mardiaTest(virginica, qqplot = TRUE)
result.virginica

## Detecção de outliers na base de dados
setosa = iris[1:50, 1:3] # Iris data only for setosa and three variables
result = mvOutlier(setosa, qqplot = TRUE, method = "quan", label = TRUE)
result
