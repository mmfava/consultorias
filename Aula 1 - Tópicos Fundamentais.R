#######################################################################
##  Aula 1                                                           ##
## ARITMÉTICA E ÁLGEBRA NO R                                         ##
## Elaborado em abril de 2015                                        ##
## Por Edson Silva e Prof. Ana                                       ##
#######################################################################

## TOPICOS FUNDAMENTAIS

## Linguagem de programação - vamos programar para o R, sendo específico para
## a realização de análises estatísticas

## Como citar o R 
citation ()

## Para limpar o workspace
ctrl+L

hist(rnorm(20))
#
#
## Topicos Fundamentais

## Funcionalidade
#
#
## Distinçao de letras entre MAISCULAS e minusculas
#
# O R e uma calculadora
#
5000-230-567.23+8765.00-1769
#
A<-2## atribui o valor 2 para a variavel A
3->B## atribui o valor 3 para a variavel B
C=A+B## soma as variaveis A e B guardam na variavel C
C## mostra o resultado da variavel C

]]=## comandos entre parenteses mostra o resultado na tela.
## O R usa o ponto (.) como separador decimal
## Se for importar dados que usam virgula como separadores decimais, troqueas
## por pontos (.) usando comandos de localizar e substituir diretamente 
## no arquivo fonte ou observe os parametros de conversao no processo de leitura
#
 cos (A)+log(C)- sqrt ((A+B)/C)

## NA valores faltantes
## NaN valor nao representavel por numero

## Operadores Aritimeticos (reservado)

## + - * /  soma subtracao produto e divisao
## sqrt () raiz quadrada
## abs ()  valor absoluto (positivo)

##############################################################################
## VETORES                                                                  ##
##############################################################################

## Definição: Diz-se que um vetor é uma estrutura atômica quando seus elementos
## são todos do mesmo tipo. Se os elementos não forem do mesmo tipo diremos que
## se trata 

## Banco de dados é um conjunto de dados com informações de mesmo perfil.
## Um banco de dados é uma junção de vetores, ou seja, uma coluna.

## Os conteúdos podem ser:
## número reais, números complexos, lógico, caracteres.

## Vetor de tamanho 1 (escalar)
v1=10
v1

v2=c(1,2,4.6)
v2
  
round(v2,0)

v2[2]=1.34 ## atribui um outro valor à posição
v2

v2[-2]
v2=v2[-2]


set.seed(123);(Saldo=sample(-20:20,15))
