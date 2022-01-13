############################################################################# 
##                  TESTE DE QUI QUADRADO PARA ADERÊNCIA                   ##
##                           Profa. Ana Tereza                             ##
##                            outubro de 2014                              ##
#############################################################################


# Teste de X2 para aderência ou bondade de ajustamento: verifica se a distribuição
## de dados ajusta-se a uma distribuição esperada (teórica)

## Este teste apresenta algumas restrições para o seu uso:

## a) Tabelas de entrada única com n>25
## b) Tabelas com apenas duas categorias (k=2): a frequência esperada deve ser >=5
## em cada categoria e deve-se a correção de Yates para o cálculo do X2. Se a 
## frequência esperada for < 5 deve-se utilizar o teste de hipótese com a distri-
## buição binomial;
## c) Tabelas com k>2 e todas as frequências esperadas iguais:para testes com o
## nível de significância (alfa) igual a 0,05 os valores esperados devem ser iguais
## ou maiores do que 1; para alfa igual a 0,01, os valores esperados devem ser
## iguais ou maiores do que 2.
## d) Tabelas com k>2 e as frequências esperadas diferentes: aplica-se o teste de
## X2 após conferir as seguintes condições: n>=10, n^2/k>=10, n/k>=2 para testes com
## alfa =0,05 (para alfa=0,01, a última condição fica n/k>=4).

## EXEMPLO 1 > X2 para aderência

obs=c(10,7,10,6,14,8,11,11,12,11)
sum(obs)
chisq.test(obs,correct=FALSE) ## correct=TRUE é o comando da correção de continuidade
## de Yates.
chisq.test(obs)$expected

## Interpretação: Foi possível observar que as proporções de 
## morbidades foram consideradas iguais entre as crianças 
## da UTI-Neo (x2=5.2, GL=9, p=0.8165).

sexo=c("H"=10,"M"=26)
sexo
chisq.test(sexo,correct=TRUE)

## EXEMPLO 2
## Um exemplo sobre as freqüências de moscas em uma pequena lagoa

moscas=c("D. autumnalis"=24, "D. aestivalis"=32, "D.amphibia"=10, "D. attica"=9)
moscas

## Plotar estas frequências:

barplot(moscas,xlab="Espécie",ylab="Freqüência",cex.names=0.8, col="blue")

## As freqüências observadas são significativamente homogêneas?

chisq.test(moscas,correct=FALSE)
chisq.test(moscas)$expected


## Exercício: Imagine a situação na qual um investigador está estudando a presença dos
## antígenos R e S (fictícios) em tecido humano. O assunto é de grande importância, 
## pois seriam antígenos relacionados com a histocompatibilidade e qualquer informação 
## relativa à herança desses antígenosé de grande valia nos casos de transfusões e
## transplante de tecidos e órgãos. Imagine que existam 3 tipos de heranças:
  
  # TIPO R: pessoas que só possuem o antígeno R
  # TIPO S: pessoas que só possuem o antígeno S
  # TIPO RS: pessoas que possuem os dois antígenos

## O pesquisador faz a hipótese de que estes tipos são determinados geneticamente por
## um par de genes R e S autossômicos co-dominantes, isto é, que estão no mesmo loco
## cromossômico e se expressam ambos, independentemente, no heterozigoto. No caso, 
## os genótipos para cada tipo seriam: genótipo RR (determinando o fenótipo R), 
## genótipo SS (fenótipo SS) e genótipo RS (fenótipo RS). 
## Se esta hipótese estiver correta, então filhos resultantes de cruzamentos de 
## mulheres RS com homens RS devem apresentar os três genótipos possíveis, nas 
## proporções 1/4 para R, 1/2 para RS e 1/4 para S, conforme determina a Primeira Lei
## de Mendel.
## Desejando testar a hipótese de que os alelos R e S são co-dominantes, o pesquisador
## estudou 24 filhos de casamentos do tipo RS x RS, escolhidos aleatoriamente, e 
## obteve 6 crianças do tipo R, 15 do tipo RS e 3 do tipo S. Até que ponto a distri-
## buição das frequências é feita ao acaso?


# Base de Dados
gen_obs=c("R"=6, "RS"=15,"S"=3)
gen_obs

# Plotar os dados em um gráfico de colunas
barplot(gen_obs,xlab="genótipos",ylab="Freqüência",cex.names=0.7)

# Calcular as frequências esperadas
gen_exp=c("R"=24*0.25, "RS"=24*0.5,"S"=24*0.25)
gen_exp

# Etapas do cálculo de Qui Quadrado
desvio=gen_obs-gen_exp
desvio

d.quad=desvio^2/gen_exp
d.quad

qui2=sum(d.quad)
qui2

## Cálculo do p-valor do teste de Qui Quadrado
pchisq(q=qui2, df=2,lower.tail=FALSE)

## Construção da curva de distribuição de qui quadrado
curve(dchisq(x, df=2),-0.5,10, xlab="Qui-quadrado, 2 g.l.", 
      ylab="Densidade probabilística")

## Sobrepõe uma linha vermelha a partir do valor do Qui-quadrado crítico
curve(dchisq(x, df=2), 5.99, 10, add=T, col="red", lwd=2)





## exercício 5

satisfacao=c("mae"=10,"pai"=26,"filha"=16,"filho"=32)

chisq.test(satisfacao,correct=TRUE)
