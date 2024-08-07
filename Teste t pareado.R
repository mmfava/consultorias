## *****************************************************************##
##                                                                  ##
##                Teste de comparacao de medias                     ##
##                      Amostras Pareadas                           ##
##                    Profa. Ana Tereza                             ##
##                                                                  ##
##               Modificado em: agosto de 2014                      ##
##                                                                  ##
## *****************************************************************##

## PRESSUPOSTOS ESTAT�STICOS

## O teste t compara a m�dia de duas amostras. No caso do teste t pareado, n�s comparamos a m�dia das diferen�as
## com a m�dia (mu) zero, indicando que a aus�ncia de diferen�as � a equival�ncia das duas medidas.
## Pode ser empregado se atender os seguintes pressupostos:
## a) As diferen�as devem ter distribui��o normal. 
##    Pode ser verificado com o box plot
##    e o teste de normalidade de Shapiro-Wilk.
## b) Homogeneidade de vari�ncias: As medidas devem apresentar  
##    vari�ncias aproximadamente iguais. Pode ser verificado com box-plot 
##    ou teste F.


## Exemplo: Para uma amostra de doze pessoas foi perguntado 
## o peso, sendo esta medida denominada de 
## Peso relatado (Relat). Nesta mesma amostra, 
## todos os volunt�rios foram pesados em balan�a digital, sendo
## esta medida denominada de Pesos medidos (Medido).
## ----------------------------------------

Relat = c(68, 71, 63, 70, 61, 60, 65, 64, 54, 63, 66, 72)
Medido = c(67.9, 69.9, 64.9, 68.3, 70.3, 60.6, 64.5, 67.0,
           55.6, 74.2, 65.0, 70.8)

## An�lise explorat�ria: esta an�lise permitir� que analisemos 
## a normalidade e a homogeneidade dos dados. Contudo,
## ela n�o descarta as an�lises estat�sticas de normalidade 
## e homogeneidade das vari�ncias.
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

## Homogeneidade de vari�ncias  
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
