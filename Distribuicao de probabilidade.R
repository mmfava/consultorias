##################################################################################################################
##                                                                                                              ##
##                                    DISTRIBUI��ES DE PROBABILIDADE                                            ##
##                                                                                                              ##
##################################################################################################################

##################################################################################################################

## Podemos criar distribui��es de probabilidade de diferentes fam�lias (Gaussiano ou Normal, Binomial,
## Poisson) usando valores personalizados dos par�metros das distribui��es.
## A cria��o de distribui��es de probabilidade,  permite testar a dispers�o de organismos no seu ambiente 
## conforme os modelos Binomial (dispers�o regular), Poisson (dispers�o aleat�ria) 
## ou Binomial Negativa (dispers�o contagiosa ou agregada).



## DISTRIBUI��O NORMAL

## Para visualizar uma distribui��o normal de probabilidade (par�metros: ?? e ??) com a m�dia da amostra =74.0 
## e s =2,34 (estimativas de ?? e ??, respectivamente) para as observa��es entre 68 e 80 use:

barplot(dnorm(68:80, 74.0, 2.34), col=2, names=c(68:80),xlab="comprimento da planta", ylab="Probabilidade")

## dnorm(x, mean = 0, sd = 1, log = FALSE)- fun��o de densidade de probabilidade
## pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) - fun��o de distribui��o


dnorm(78, 74, 2.34)
pnorm(78,74,2.34)


###################################################################################################################
## DISTRIBUI��O BINOMIAL

## Para visualizar uma distribui��o Binomial de probabilidade com k=8 ensaios, p=q=0,5 e para o n�mero de
## resultados nomeados entre 0 a 8 use:

barplot(dbinom(0:8,8,0.5), names=c(0:8), xlab="N�mero de Machos", ylab="Probabilidade")

## dbinom(x, size, prob, log = FALSE) - fun��o de densidade de probabilidade
## pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE) - fun��o de distribui��o

dbinom(4,8,0.5)
pbinom(4,8,0.5, lower.tail=FALSE)
pbinom(4,8,0.5, lower.tail=TRUE)

## Para visualizar uma distribui��o Binomial de probabilidade com k=8 ensaios, p=0,8 e q=0,2 e para o
## n�mero de resultados nomeados entre 0 e 8 use:

barplot(dbinom(0:8,8,0.8), names=c(0:8),xlab="N�mero de indiv�duos de D. autumnalis", ylab="Probabilidade")


#####################################################################################################################
## DISTRIBUI��O DE POISSON

## Para visualizar uma distribui��o Poisson de probabilidade com ??=4 (a m�dia � uma estimativa do
## par�metro ??) e para observa��es entre 0 e 10 use:

barplot(dpois(0:10,4.0),col=3,names=c(0:10), 
        xlab="N�mero de indiv�duos por unidade de amostragem", ylab="Probabilidade")

## dpois(x, lambda, log = FALSE) - fun��o de densidade de probabilidade
## ppois(q, lambda, lower.tail = TRUE, log.p = FALSE) - fun��o de distribui��o

dpois(2,4)
ppois(2,4,lower.tail=TRUE)
ppois(2,4,lower.tail=FALSE)

######################################################################################################################
## DISTRIBUI��O BINOMIAL NEGATIVA

barplot(dnbinom(0:8, mu=2, size=1.33), col=3, names=c(0:8),
        xlab="N�mero de indiv�duos por unidade de amostragem",
        ylab="Probabilidade")

## dnbinom(x, size, prob, mu, log = FALSE) - fun��o de densidade de probabilidade
## pnbinom(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE) - fun��o de distribui��o

dnbinom(2,size=1.33,mu=2)
pnbinom(2,size=1.33,mu=2,lower.tail=TRUE)
pnbinom(2,size=1.33,mu=2,lower.tail=FALSE)
