## *****************************************************************##
##                                                                  ##
##                Teste de comparacao de medias                     ##
##                        2 amostras                                ##
##                    Profa. Ana Tereza                             ##
##                    Prof. Edson Silva                             ##
##               Modificado em: julho de 2014                       ##
##                                                                  ##
## *****************************************************************##

## PRESSUPOSTOS ESTAT�STICOS

## O teste t compara a m�dia de duas amostras. 
## Pode ser empregado se atender os seguintes pressupostos:
## a) Os dados devem ter distribui��o normal. 
##    Pode ser verificado com o box plot,
##    o grafico qq-plot e o teste de normalidade de Shapiro-Wilk.
## b) Homogeneidade de vari�ncias: As amostras devem apresentar  
##    vari�ncias aproximadamente iguais. Pode ser verificado com box-plot 
##    ou teste F.
## c) Amostras independentes: A independ�ncia dos dados n�o � cumprido se,
##    por exemplo, sao tomados dados sobre as mesmas �rvores frut�feras, 
##    em dois anos consecutivos. Neste caso as amostras s�o tratadas como
##    PAREADAS e o teste � feito sobre a m�dia das diferencas.

## Exemplo "Wisconsin Fast Plant" (2)
## ----------------------------------------

## A Brassica campestris � uma planta patenteada, desenvolvida
## pela Universidade de Wisconsin, usada em salas de aula para
## demonstrar o crescimento e a gen�tica em estudos com plantas.
## Tem um ciclo de vida de 40 dias. Com sete dias ela ter�
## 5 cent�metros de altura e folhas vis�veis, e com 13 dias 
## florescer�. Entre 28 e 35 dias, a planta ter� 20 cent�metros
## de altura e produzir� sementes.
## O Ancymidol � um supressor de crescimento, usado na agricultura
## como herbicida. Em um estudo sete plantas tratadas com essa
## subst�ncia (Ancy) foram comparadas com oito plantas controle
## (Ctrl) tratadas com �gua comum. Suas alturas foram medidas
## ap�s 14 dias de crescimento (Ahern, 1998). Os dados desse
## exerc�cio correspondem um subconjunto dos dados originais,
## selecionado aleatoriamente. 

Ctrl=c(10,13.2,19.8,19.3,21.2,13.9,20.3,9.6)
Ancy=c(13.2,19.5,11,5.8,12.8,7.1,7.7)

boxplot(Ctrl,Ancy)
hist(Ctrl)
hist(Ancy)

## Hip�teses
## Ho: m�dia controle = m�dia ancy  (as m�dias sao iguais)
## Ha: m�dia ancy < m�dia controle  (a m�dia de controle � maior)
##

## TESTE DE VARI�NCIA (Teste F)
var.test(Ctrl,Ancy) 

## como p-valor > 0.05 aceita-se a hipotese de var iguais

## Como p-valor supera 0.05 (p.ex.) o teste nao eh significativo
## (nao se rejeita Ho), portanto podemos supor que as variancias
## amostrais sejam iguais.

## TESTE DE NORMALIDADE (Teste de Shapiro-Wilk)
shapiro.test(Ctrl) # como p-valor > 0.05 aceita-se a hipotese de normalidade
shapiro.test(Ancy)# como p-valor > 0.05 aceita-se a hipotese de normalidade

## O teste de normalidade nao foi significativo logo pode-se 
## supor que as amostras sejam provenientes de populacoes
## normais. 

## TESTE T PARA AMOSTRAS INDEPENDENTES

## O teste t padrao pode ser aplicado aos dados 
##
##  t.test(x, y = NULL,
##         alternative = c("two.sided", "less", "greater"),
##         mu = 0, 
##         paired = FALSE, 
##         var.equal = FALSE,
##         conf.level = 0.95, ...)

t.test(Ancy,Ctrl,
       var.equal = TRUE,paired=FALSE,
       alternative = "less",conf.level=0.95)

## Criterio de julgamento:
## se p-valor for maior do que 0,05 aceita-se Ho

## Como no teste p-valor resultou menor do que 5%
## dizemos que o teste foi significativo (para a 
## hipotese alternativa) e afirma-se que ha evid�ncias
## na amostra a favor da diferen�a de m�dias. Em outras
## palavras, dizemos que as m�dias s�o significativamente
## diferentes.

boxplot(Ctrl,Ancy,ylab="Altura", col="blue")
atividadef�sica=c(15,13,12,14)
n�oatividade=c(20,24,25,20,15,18)

boxplot(atividadef�sica,n�oatividade)

var.test(atividadef�sica,n�oatividade)

shapiro.test(atividadef�sica)
shapiro.test(n�oatividade)

t.test(atividadef�sica,n�oatividade
       var.equal = TRUE,paired=FALSE,
       alternative = "less",conf.level=0.95)


Ctrl=c(15,17,17,11,16,14,12,20,12)
AF=c(18,23,19,14,14,16,24,26,23,16)

boxplot(Ctrl,AF)
hist(Ctrl)
hist(AF)

## TESTE DE VARI�NCIA (Teste F)
var.test(Ctrl,AF) 

## TESTE DE NORMALIDADE (Teste de Shapiro-Wilk)
shapiro.test(Ctrl) # como p-valor > 0.05 aceita-se a hipotese de normalidade
shapiro.test(AF)# como p-valor > 0.05 aceita-se a hipotese de normalidade

## TESTE T PARA AMOSTRAS INDEPENDENTES

t.test(AF,Ctrl,
       var.equal = TRUE,paired=FALSE,
       alternative = "greater",conf.level=0.95)

sd(AF)
sd(Ctrl)
## Foi poss�vel verificar que a m�dia do grupo que realizou
## atividade f�sica foi significativamente maior (19.3+4.4) que
## a m�dia do grupo controle (14.9+2.9) (t=2.5394; p=0.01058; fig.1).