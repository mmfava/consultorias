############################################################################# 
##                          COMPARAÇÕES MULTIPLAS                          ##
##                   Profa. Ana Tereza e Prof. Edson                       ##
##                           setembro de 2014                              ##
#############################################################################


## Restrições
## - normalidade dentro dos tratamentos
## - homogeneidade das variâncias segundo tratamentos
## - independencia de observações dentro do tratamento e entre tratamentos
##
## Testes
## - Tukey
## - Scheffe (quanto o teste F for significativo)
## - Duncan (teste prova para Tukey)
## - Bonferroni (equivalente a um teste t para 2 amostras)
## - Dunnett (todos tratamentos vs testemunha)
## -----------------------------------------------------------

melão = data.frame(
  variedade = rep(c("A","B","C","D"),each=6),
  produção = c(25.12,17.25,26.42,16.08,22.15,15.92,
            40.25,35.25,31.98,36.52,43.32,37.10,
            18.30,22.60,25.90,15.05,11.42,23.68,
            28.55,28.05,33.20,31.68,30.32,27.58))

boxplot(formula = produção ~ variedade, data = melão,
        col = "lightblue",
        ylab = "produção (toneladas)",
        names = c("A","B","C","D"))


## Teste de homogeneidade das variancias
# instalar o pacote car
library(car)

leveneTest(y = melão$produção, group = melão$variedade)
## OU
bartlett.test(produção ~ variedade, data = melão)



## Teste de normalidade: a análise dos residuos é mais importante, então é 
## necessário realizar o ajuste de um modelo linear:

melão.model = lm(formula = produção ~ variedade,data = melão)

melão.predito=fitted(object=melão.model)

melão.residuos=resid(object=melão.model)

shapiro.test(melão.residuos)


## ANOVA: uma vez que os dados são homocedásticos e os resíduos estão em 
## normalidade, devemos fazer a Análise da Variância

anova(melão.model)
## ou
summary(aov(produção ~ variedade, data = melão))


## COMPARACAO MULTIPLA
## -------------------

## Sempre é importante fazer a demonstração gráfica dos resultados - Box-whisker

## Desvio padrão
(desvio=with(melão, tapply(produção,variedade, sd)))

## Mádias
(medias = with(melão, tapply(produção, variedade, mean)))

plot(medias, pch=15,ylim=c(0,ceiling(max(medias)+max(desvio))))
arrows(1:4, medias + desvio, 1:4, medias - desvio, angle = 90,
       code = 3, length = 0.1)

## Contagem (n)
with(melão, tapply(produção, variedade, length))



## Teste de Tukey
melão.aov= aov(produção ~ variedade, data = melão)

(melon.TukeyHSD = TukeyHSD(melão.aov,
                           which= "variedade",
                           conf.level=0.95,
                           ordered = TRUE))

plot(melon.TukeyHSD)


## Teste de Dunnett
library(multcomp)
library(splines)

test.dunnett=glht(melão.aov,linfct=mcp(variedade="Dunnett"))

## glht - comando de execução dos testes de hipótese lineares generalizados
## melão.aov - neste exemplo é o teste de ANOVA realizado para o modelo
## linfct - uma especificação da hipótese linear a ser testada. Relaciona-se a função linear da matriz
## mcp - procedimentos de comparações múltiplas.

confint(test.dunnett)
plot(test.dunnett)


## EXERCICIOS
## ---------------------------------------

## -----------------------
## EXERCICIO 1: Colesterol
## -----------------------
## Um estudo clinico foi conduzido para avaliar o efeito de tres 
## formulacoes do mesmo farmaco para reduzir o colesterol. 
## As formulacoes foram de 20 mg de uma so vez (1vez), 
## 10 mg duas vezes por dia (2vezes) e 5 mg quatro vezes por dia (4times). 
## Alem disso, dois farmacos competitivos foram utilizados como grupo 
## de controlo (drugD e drugE). O objetivo do estudo foi o de identificar 
## qual das formulacoes, se houver, eh eficaz e como estas formulações se 
## comparam com as drogas existentes. Os dados abaixo representam 
##
##        uma    duas  quatro       D       E
##     3.8612 10.3993 13.9621 16.9819 21.5119
##    10.3868  8.6027 13.9606 15.4576 27.2445
##     5.9059 13.6320 13.9176 19.9793 20.5199
##     3.0609  3.5054  8.0534 14.7389 15.7707
##     7.7204  7.7703 11.0432 13.5850 22.8850
##     2.7139  8.6266 12.3692 10.8648 23.9527
##     4.9243  9.2274 10.3921 17.5897 21.5925
##     2.3039  6.3159  9.0286  8.8194 18.3058
##     7.5301 15.8258 12.8416 17.9635 20.3851
##     9.4123  8.3443 18.1794 17.6316 17.3071

## P. H. Westfall, R. D. Tobias, D. Rom, R. D. Wolfinger, Y. Hochberg (1999). 
## Multiple Comparisons and Multiple Tests Using the SAS System. 
## Cary, NC: SAS Institute Inc., page 153.

##
## ---------------------------------------
## EXERCICIO 2: Acidos Graxos
## ---------------------------------------
## Os dados seguintes se referem ao teor de acidos graxos de diferentes
## ecotipos (presenca de populacoes geneticamente unicas que sao adaptadas
## ao seu ambiente local) putativos (indevidos) de Bacillus simplex.

## PE: fator que define o ecotipo
## FA: um vetor numerico que indica o teor de acidos graxos

##
##  PE3  PE4   PE5   PE6   PE7	 PE9
## 0.65		    0.77	0.85	0.73	0.95
## 0.92	0.80	0.82	0.94		    0.95
## 0.73	0.78	0.83	0.90	0.67	1.04
## 1.03	0.71	0.71	0.90	0.82	1.01
## 0.81	0.85	0.67	0.81	0.84	0.86
## 0.75	0.76	0.77	0.89	0.80	0.93
## 0.82	0.86	0.74	0.87	0.80	0.96
## 1.00	0.88	0.75		    0.88	0.82
## 0.82	0.93	0.76		    0.98	0.97
## 0.95	0.86	0.80		    0.86	0.84
## 0.83	0.85	0.70		    0.89	0.99
## 0.83	0.81	0.71		    0.88	0.89
## 1.02	1.01	0.83		    0.90	0.86
## 0.89	0.81	0.86		    0.88	1.02
##      0.86	0.82		    0.96	0.93
##      0.88	0.82		    0.87	0.96
##            0.77			
##      0.83	0.79			
##      0.88	0.83			
##      0.85				
##      0.90				

## J. Sikorski, E. Brambilla, R. M. Kroppenstedt, B. J. Tindal (2008), 
## The temperature adaptive fatty acid content in Bacillus simplex 
## strains from "Evolution Canyon", Israel. Microbiology 154, 2416-2426.

##
## ---------------------------------------
## EXERCICIO 3: Cobertor
## ---------------------------------------
## Uma empresa desenvolve cobertores de aquecimento especializados, projetados
## para ajudar no calor corporal dos pacientes apos um procedimento cirurgico. 
## Quatro tipos de cobertores foram experimentados em pacientes cirurgicos com
## o objetivo de comparar o tempo de recuperacao deles. Um dos cobertores era 
## do tipo padrao, que estava em uso em varios hospitais. Os dados contem as
## seguintes variaveis:

## Cobertor: Fator que define o tipo de cobertor utilizado
## Tempo: Tempo de recuperacao apos o procedimento cirurgico

## P. H. Westfall, R. D. Tobias, D. Rom, R. D. Wolfinger, Y. Hochberg (1999). 
## Multiple Comparisons and Multiple Tests Using the SAS System. 
## Cary, NC: SAS Institute Inc., page 66.

## Cobertor 0: 15 13 12 16 16 17 13 13 16 17 17 19 17 15 13 12 16 10 17 12
## Cobertor 1: 13 16  9
## Cobertor 2:  5  8  9
## Cobertor 3: 14 16 16 12  7 12 13 13  9 16 13 18 13 12 13


## ---------------------------------------
## EXERCICIO 4: Residuos
## ---------------------------------------

## Os dados sao de um experimento concebido para estudar o efeito da temperatura 
## (temp: Low, Medium e High) e do meio ambiente (env) sobre a producao de 
## residuos em uma fabrica. Foram tomadas duas medidas repetidas em 
## cada combinacao de temperatura / ambiente.

## P. H. Westfall, R. D. Tobias, D. Rom, R. D. Wolfinger, Y. Hochberg (1999). 
## Multiple Comparisons and Multiple Tests Using the SAS System. 
## Cary, NC: SAS Institute Inc., page 177.
##
##         env1  env2  env3  env4  env5
##    Low  7,09  7,94  9,23  5,43  9,43
##         5,90  9,15  9,85  7,73  6,90
## Medium  7,01  6,18  7,86  8,49  9,62
##         5,82  7,19  6,33  8,67  9,07
##   High  7,78 10,39  9,27 12,17 13,07 
##         7,73  8,78  8,90 10,95  9,76

## Davidian e Giltinan (1995, 1.1, p. 2) descrevem os dados obtidos durante 
## um estudo piloto para investigar a farmacocinetica do cefamandole 
## (antibiotico). As concentracoes plasmaticas da droga foram medidas em seis 
## voluntarios saudaveis, aos 14 momentos após uma dose intravenosa de 15 mg/kg
## de peso corporal de cefamandol.