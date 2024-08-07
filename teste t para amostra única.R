## *****************************************************************##
##                                                                  ##
##                Teste t para amostra �nica                        ##
##                    Profa. Ana Tereza                             ##
##               Modificado em: agosto de 2014                      ##
##                                                                  ##
## *****************************************************************##

## PRESSUPOSTOS ESTAT�STICOS

## O teste t compara a m�dia de duas amostras. 
## No caso do teste t para amostra �nica, comparamos uma m�dia amostral a uma m�dia populacional,
## considerada como um valor de refer�ncia.

## O teste t faz parte da fam�lia dos testes param�tricos. 
## Para que eu possa empregar qualquer tipo de teste param�trico, 
## deve-se atender os seguintes pressupostos:
## a) Os dados devem ter distribui��o normal. 
##    Pode ser verificado com o box plot e o teste de normalidade de Shapiro-Wilk.
## b) Homogeneidade de vari�ncias: As amostras devem apresentar  
##    vari�ncias aproximadamente iguais. Pode ser verificado com box-plot 
##    ou teste F.
## c) Amostras independentes: A independ�ncia dos dados n�o � cumprido se,
##    por exemplo, sao tomados dados sobre as mesmas �rvores frut�feras, 
##    em dois anos consecutivos. Neste caso as amostras s�o tratadas como
##    PAREADAS e o teste � feito sobre a m�dia das diferencas.

## Exemplo: Teste de Hemoglobina Glicada

## Em um grupo de 10 pessoas foi avaliada a vari�vel Hemoglobina Glicada. Esta vari�vel � 
## mensurada quando h� suspeita de Diabetes em um paciente. O valor de refer�ncia � 5.3%, 
## estando no intervalo de confian�a de 4.1 a 6.5%. Teste a hip�tese nula de que esta amostra
## de pacientes apresenta m�dia equivalente ao valor de refer�ncia.

hemog=c(4.9,6.9,12.9,5.4,5.8,7.5,6.4,5.3,8.1,6.8)

summary(hemog)
boxplot(hemog)
shapiro.test(hemog)

## Hip�teses
## Ho: m�dia hemog = valor de refer�ncia  (as m�dias sao iguais)
## Ha: m�dia hemog > valor de refer�ncia  (a m�dia de controle � maior)

## O teste t padrao pode ser aplicado aos dados 
##  t.test(x, y = NULL,
##         alternative = c("two.sided", "less", "greater"),
##         mu = 0, 
##         paired = FALSE, 
##         var.equal = FALSE,
##         conf.level = 0.95, ...)
t.test(hemog,y=NULL, 
       alternative=c("greater"),
       mu=5.3,paired=FALSE,var.equal=TRUE,
       conf.level=0.95)

## wilcox.test(x, y = NULL,
##            alternative = c("two.sided", "less", "greater"),
##            mu = 0, paired = FALSE, exact = NULL, 
##            correct = TRUE,conf.int = FALSE, 
##            conf.level = 0.95, ...)

wilcox.test(hemog, y = NULL,
            alternative = c("greater"),
            mu = 5.3, paired = FALSE, exact = NULL, 
            correct = TRUE,conf.level = 0.95)
