#######################################################################
##  Aula 1                                                           ##
## ARITM�TICA E �LGEBRA NO R                                         ##
## Elaborado em abril de 2015                                        ##
## Por Edson Silva e Prof. Ana                                       ##
#######################################################################

## TOPICOS FUNDAMENTAIS

## Linguagem de programa��o - vamos programar para o R, sendo espec�fico para
## a realiza��o de an�lises estat�sticas

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
## Distin�ao de letras entre MAISCULAS e minusculas
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

## Defini��o: Diz-se que um vetor � uma estrutura at�mica quando seus elementos
## s�o todos do mesmo tipo. Se os elementos n�o forem do mesmo tipo diremos que
## se trata 

## Banco de dados � um conjunto de dados com informa��es de mesmo perfil.
## Um banco de dados � uma jun��o de vetores, ou seja, uma coluna.

## Os conte�dos podem ser:
## n�mero reais, n�meros complexos, l�gico, caracteres.

## Vetor de tamanho 1 (escalar)
v1=10
v1

v2=c(1,2,4.6)
v2
  
round(v2,0)

v2[2]=1.34 ## atribui um outro valor � posi��o
v2

v2[-2]
v2=v2[-2]


set.seed(123);(Saldo=sample(-20:20,15))
