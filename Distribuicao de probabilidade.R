##################################################################################################################
##                                                                                                              ##
##                                    DISTRIBUIÇÕES DE PROBABILIDADE                                            ##
##                                                                                                              ##
##################################################################################################################

##################################################################################################################

## Podemos criar distribuições de probabilidade de diferentes famílias (Gaussiano ou Normal, Binomial,
## Poisson) usando valores personalizados dos parâmetros das distribuições.
## A criação de distribuições de probabilidade,  permite testar a dispersão de organismos no seu ambiente 
## conforme os modelos Binomial (dispersão regular), Poisson (dispersão aleatória) 
## ou Binomial Negativa (dispersão contagiosa ou agregada).



## DISTRIBUIÇÃO NORMAL

## Para visualizar uma distribuição normal de probabilidade (parâmetros: ?? e ??) com a média da amostra =74.0 
## e s =2,34 (estimativas de ?? e ??, respectivamente) para as observações entre 68 e 80 use:

barplot(dnorm(68:80, 74.0, 2.34), col=2, names=c(68:80),xlab="comprimento da planta", ylab="Probabilidade")

## dnorm(x, mean = 0, sd = 1, log = FALSE)- função de densidade de probabilidade
## pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) - função de distribuição


dnorm(78, 74, 2.34)
pnorm(78,74,2.34)


###################################################################################################################
## DISTRIBUIÇÃO BINOMIAL

## Para visualizar uma distribuição Binomial de probabilidade com k=8 ensaios, p=q=0,5 e para o número de
## resultados nomeados entre 0 a 8 use:

barplot(dbinom(0:8,8,0.5), names=c(0:8), xlab="Número de Machos", ylab="Probabilidade")

## dbinom(x, size, prob, log = FALSE) - função de densidade de probabilidade
## pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE) - função de distribuição

dbinom(4,8,0.5)
pbinom(4,8,0.5, lower.tail=FALSE)
pbinom(4,8,0.5, lower.tail=TRUE)

## Para visualizar uma distribuição Binomial de probabilidade com k=8 ensaios, p=0,8 e q=0,2 e para o
## número de resultados nomeados entre 0 e 8 use:

barplot(dbinom(0:8,8,0.8), names=c(0:8),xlab="Número de indivíduos de D. autumnalis", ylab="Probabilidade")


#####################################################################################################################
## DISTRIBUIÇÃO DE POISSON

## Para visualizar uma distribuição Poisson de probabilidade com ??=4 (a média é uma estimativa do
## parâmetro ??) e para observações entre 0 e 10 use:

barplot(dpois(0:10,4.0),col=3,names=c(0:10), 
        xlab="Número de indivíduos por unidade de amostragem", ylab="Probabilidade")

## dpois(x, lambda, log = FALSE) - função de densidade de probabilidade
## ppois(q, lambda, lower.tail = TRUE, log.p = FALSE) - função de distribuição

dpois(2,4)
ppois(2,4,lower.tail=TRUE)
ppois(2,4,lower.tail=FALSE)

######################################################################################################################
## DISTRIBUIÇÃO BINOMIAL NEGATIVA

barplot(dnbinom(0:8, mu=2, size=1.33), col=3, names=c(0:8),
        xlab="Número de indivíduos por unidade de amostragem",
        ylab="Probabilidade")

## dnbinom(x, size, prob, mu, log = FALSE) - função de densidade de probabilidade
## pnbinom(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE) - função de distribuição

dnbinom(2,size=1.33,mu=2)
pnbinom(2,size=1.33,mu=2,lower.tail=TRUE)
pnbinom(2,size=1.33,mu=2,lower.tail=FALSE)
