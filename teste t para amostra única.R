## *****************************************************************##
##                                                                  ##
##                Teste t para amostra única                        ##
##                    Profa. Ana Tereza                             ##
##               Modificado em: agosto de 2014                      ##
##                                                                  ##
## *****************************************************************##

## PRESSUPOSTOS ESTATÍSTICOS

## O teste t compara a média de duas amostras. 
## No caso do teste t para amostra única, comparamos uma média amostral a uma média populacional,
## considerada como um valor de referência.

## O teste t faz parte da família dos testes paramétricos. 
## Para que eu possa empregar qualquer tipo de teste paramétrico, 
## deve-se atender os seguintes pressupostos:
## a) Os dados devem ter distribuição normal. 
##    Pode ser verificado com o box plot e o teste de normalidade de Shapiro-Wilk.
## b) Homogeneidade de variâncias: As amostras devem apresentar  
##    variâncias aproximadamente iguais. Pode ser verificado com box-plot 
##    ou teste F.
## c) Amostras independentes: A independência dos dados não é cumprido se,
##    por exemplo, sao tomados dados sobre as mesmas árvores frutíferas, 
##    em dois anos consecutivos. Neste caso as amostras são tratadas como
##    PAREADAS e o teste é feito sobre a média das diferencas.

## Exemplo: Teste de Hemoglobina Glicada

## Em um grupo de 10 pessoas foi avaliada a variável Hemoglobina Glicada. Esta variável é 
## mensurada quando há suspeita de Diabetes em um paciente. O valor de referência é 5.3%, 
## estando no intervalo de confiança de 4.1 a 6.5%. Teste a hipótese nula de que esta amostra
## de pacientes apresenta média equivalente ao valor de referência.

hemog=c(4.9,6.9,12.9,5.4,5.8,7.5,6.4,5.3,8.1,6.8)

summary(hemog)
boxplot(hemog)
shapiro.test(hemog)

## Hipóteses
## Ho: média hemog = valor de referência  (as médias sao iguais)
## Ha: média hemog > valor de referência  (a média de controle é maior)

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
