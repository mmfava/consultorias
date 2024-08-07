############################################################################# 
##                  TESTE DE QUI QUADRADO PARA ADER�NCIA                   ##
##                           Profa. Ana Tereza                             ##
##                            outubro de 2014                              ##
#############################################################################


# Teste de X2 para ader�ncia ou bondade de ajustamento: verifica se a distribui��o
## de dados ajusta-se a uma distribui��o esperada (te�rica)

## Este teste apresenta algumas restri��es para o seu uso:

## a) Tabelas de entrada �nica com n>25
## b) Tabelas com apenas duas categorias (k=2): a frequ�ncia esperada deve ser >=5
## em cada categoria e deve-se a corre��o de Yates para o c�lculo do X2. Se a 
## frequ�ncia esperada for < 5 deve-se utilizar o teste de hip�tese com a distri-
## bui��o binomial;
## c) Tabelas com k>2 e todas as frequ�ncias esperadas iguais:para testes com o
## n�vel de signific�ncia (alfa) igual a 0,05 os valores esperados devem ser iguais
## ou maiores do que 1; para alfa igual a 0,01, os valores esperados devem ser
## iguais ou maiores do que 2.
## d) Tabelas com k>2 e as frequ�ncias esperadas diferentes: aplica-se o teste de
## X2 ap�s conferir as seguintes condi��es: n>=10, n^2/k>=10, n/k>=2 para testes com
## alfa =0,05 (para alfa=0,01, a �ltima condi��o fica n/k>=4).

## EXEMPLO 1 > X2 para ader�ncia

obs=c(10,7,10,6,14,8,11,11,12,11)
sum(obs)
chisq.test(obs,correct=FALSE) ## correct=TRUE � o comando da corre��o de continuidade
## de Yates.
chisq.test(obs)$expected

## Interpreta��o: Foi poss�vel observar que as propor��es de 
## morbidades foram consideradas iguais entre as crian�as 
## da UTI-Neo (x2=5.2, GL=9, p=0.8165).

sexo=c("H"=10,"M"=26)
sexo
chisq.test(sexo,correct=TRUE)

## EXEMPLO 2
## Um exemplo sobre as freq��ncias de moscas em uma pequena lagoa

moscas=c("D. autumnalis"=24, "D. aestivalis"=32, "D.amphibia"=10, "D. attica"=9)
moscas

## Plotar estas frequ�ncias:

barplot(moscas,xlab="Esp�cie",ylab="Freq��ncia",cex.names=0.8, col="blue")

## As freq��ncias observadas s�o significativamente homog�neas?

chisq.test(moscas,correct=FALSE)
chisq.test(moscas)$expected


## Exerc�cio: Imagine a situa��o na qual um investigador est� estudando a presen�a dos
## ant�genos R e S (fict�cios) em tecido humano. O assunto � de grande import�ncia, 
## pois seriam ant�genos relacionados com a histocompatibilidade e qualquer informa��o 
## relativa � heran�a desses ant�genos� de grande valia nos casos de transfus�es e
## transplante de tecidos e �rg�os. Imagine que existam 3 tipos de heran�as:
  
  # TIPO R: pessoas que s� possuem o ant�geno R
  # TIPO S: pessoas que s� possuem o ant�geno S
  # TIPO RS: pessoas que possuem os dois ant�genos

## O pesquisador faz a hip�tese de que estes tipos s�o determinados geneticamente por
## um par de genes R e S autoss�micos co-dominantes, isto �, que est�o no mesmo loco
## cromoss�mico e se expressam ambos, independentemente, no heterozigoto. No caso, 
## os gen�tipos para cada tipo seriam: gen�tipo RR (determinando o fen�tipo R), 
## gen�tipo SS (fen�tipo SS) e gen�tipo RS (fen�tipo RS). 
## Se esta hip�tese estiver correta, ent�o filhos resultantes de cruzamentos de 
## mulheres RS com homens RS devem apresentar os tr�s gen�tipos poss�veis, nas 
## propor��es 1/4 para R, 1/2 para RS e 1/4 para S, conforme determina a Primeira Lei
## de Mendel.
## Desejando testar a hip�tese de que os alelos R e S s�o co-dominantes, o pesquisador
## estudou 24 filhos de casamentos do tipo RS x RS, escolhidos aleatoriamente, e 
## obteve 6 crian�as do tipo R, 15 do tipo RS e 3 do tipo S. At� que ponto a distri-
## bui��o das frequ�ncias � feita ao acaso?


# Base de Dados
gen_obs=c("R"=6, "RS"=15,"S"=3)
gen_obs

# Plotar os dados em um gr�fico de colunas
barplot(gen_obs,xlab="gen�tipos",ylab="Freq��ncia",cex.names=0.7)

# Calcular as frequ�ncias esperadas
gen_exp=c("R"=24*0.25, "RS"=24*0.5,"S"=24*0.25)
gen_exp

# Etapas do c�lculo de Qui Quadrado
desvio=gen_obs-gen_exp
desvio

d.quad=desvio^2/gen_exp
d.quad

qui2=sum(d.quad)
qui2

## C�lculo do p-valor do teste de Qui Quadrado
pchisq(q=qui2, df=2,lower.tail=FALSE)

## Constru��o da curva de distribui��o de qui quadrado
curve(dchisq(x, df=2),-0.5,10, xlab="Qui-quadrado, 2 g.l.", 
      ylab="Densidade probabil�stica")

## Sobrep�e uma linha vermelha a partir do valor do Qui-quadrado cr�tico
curve(dchisq(x, df=2), 5.99, 10, add=T, col="red", lwd=2)





## exerc�cio 5

satisfacao=c("mae"=10,"pai"=26,"filha"=16,"filho"=32)

chisq.test(satisfacao,correct=TRUE)
