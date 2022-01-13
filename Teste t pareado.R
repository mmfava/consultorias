## *****************************************************************##
##                                                                  ##
##                Teste de comparacao de medias                     ##
##                      Amostras Pareadas                           ##
##                    Profa. Ana Tereza                             ##
##                                                                  ##
##               Modificado em: agosto de 2014                      ##
##                                                                  ##
## *****************************************************************##

## PRESSUPOSTOS ESTATÍSTICOS

## O teste t compara a média de duas amostras. No caso do teste t pareado, nós comparamos a média das diferenças
## com a média (mu) zero, indicando que a ausência de diferenças é a equivalência das duas medidas.
## Pode ser empregado se atender os seguintes pressupostos:
## a) As diferenças devem ter distribuição normal. 
##    Pode ser verificado com o box plot
##    e o teste de normalidade de Shapiro-Wilk.
## b) Homogeneidade de variâncias: As medidas devem apresentar  
##    variâncias aproximadamente iguais. Pode ser verificado com box-plot 
##    ou teste F.


## Exemplo: Para uma amostra de doze pessoas foi perguntado 
## o peso, sendo esta medida denominada de 
## Peso relatado (Relat). Nesta mesma amostra, 
## todos os voluntários foram pesados em balança digital, sendo
## esta medida denominada de Pesos medidos (Medido).
## ----------------------------------------

Relat = c(68, 71, 63, 70, 61, 60, 65, 64, 54, 63, 66, 72)
Medido = c(67.9, 69.9, 64.9, 68.3, 70.3, 60.6, 64.5, 67.0,
           55.6, 74.2, 65.0, 70.8)

## Análise exploratória: esta análise permitirá que analisemos 
## a normalidade e a homogeneidade dos dados. Contudo,
## ela não descarta as análises estatísticas de normalidade 
## e homogeneidade das variâncias.
## ----------------------------------------

summary(Medido)
summary(Relat)
boxplot(Relat,Medido, 
        ylab='Peso (em kg)',
        names = c('Relatado','Medido'),
        col="yellow")

## Verificando normalidade
shapiro.test(Relat) # p-valor > 0.05 ==> normalidade
shapiro.test(Medido) # idem

## Homogeneidade de variâncias  
var.test(Relat,Medido) # p-valor > 0.05 ==> homogeneidade

## Teste t para amostras pareadas
## ----------------------------------------------------
## t.test(x, y = NULL,
##       alternative = c("two.sided", "less", "greater"),
##       mu = 0, 
##       paired = FALSE, 
##       var.equal = FALSE,
##       conf.level = 0.95, ...)
## ----------------------------------------------------
t.test(Relat, Medido, 
       var.equal = TRUE, 
       alternative = "less", 
       paired=TRUE,
       conf.level = 0.90)

## wilcox.test(x, y = NULL,
##            alternative = c("two.sided", "less", "greater"),
##            mu = 0, paired = FALSE, exact = NULL, 
##            correct = TRUE,conf.int = FALSE, 
##            conf.level = 0.95, ...)

wilcox.test(Relat, Medido,
            alternative = c("less"),
            paired = TRUE, exact = NULL, correct = TRUE,
            conf.level = 0.95)
