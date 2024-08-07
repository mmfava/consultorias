#######################################################################
#                           FREQU�NCIAS                               #
#                        REGRA DE STURGES                             #
#           Profa. Ana Tereza - 3 de novembro de 2015                 #
#######################################################################

## A Regra de Sturges � uma ferramenta estat�stica que permite que 
## agrupemos valores de vari�veis quantitativas em classes num�ricas.
## A Regra de Sturges consiste em calcularmos o n�mero de classes (k) de 
## uma tabela de distribui��o de frequ�ncias e a amplitude das respectivas
## classes (AC). Para o c�lculo de k usamos o algoritmo:
##                    k = 1+3.322*log(n)
## E para o c�lculo da amplitude de classe (AC), usamos:
##                    AC = (m�ximo-m�nimo)/k
##
## Faremos este procedimento permite que se construa um histograma. O
## histograma � uma representa��o gr�fica da distribui��o de frequ�ncias
## em classes num�ricas. A partir da interpreta��o de um histograma,
## podemos verificar o tipo de distribui��o dos dados (exemplo: normal,
## exponencial, logaritmica etc).
## Abaixo faremos um breve script sobre a aplica��o da Regra de Sturges:


exercicio = data.frame(
  tratamento = rep(1:2,each=6),
  resposta = c(14,14,13,12,15,13,24,25,19,22,18,22))

## data.frame - tabula��o de dados de forma pareada
## rep(1:3, each=6) - s�o 3 tratamentos, cada qual com 6 r�plicas
## c - concatenar
## 'tratamento' � o nome da vari�vel que separa os 3 tratamentos
## 'resposta' � a vari�vel medida, que no caso � batimentos card�acos

attach(exercicio)

## Primeiramente faremos um gr�fico do tipo Boxplot para visualizar a
## tend�ncia dos dados entre os tratamentos.

boxplot(resposta~tratamento, names=c("Sedent�rio","Ap�s 
                                     o exerc�cio", "Ap�s 5 minutos do 
                                     exerc�cio"), ylab="batimentos card�acos")

## Em seguida, faremos a Regra de Sturges ao contr�rio: primeiro,o 
## histograma, depois as classes e em seguida as frequ�ncias.

## Histograma com todos os dados
hist(resposta)

## Histograma apenas com o grupo 1
hist(resposta[exercicio$tratamento==1])

## Histograma apenas com o grupo 2
hist(resposta[exercicio$tratamento==2])

## Histograma apenas com o grupo 3
hist(resposta[exercicio$tratamento==3])

## Contruiremos as classes e as frequ�ncias
tabela=hist(resposta,breaks="Sturges")
tabela

## Exerc�cio

saude=read.csv2(file.choose(),header = T,dec=",",sep = "")

# 1) Construa histogramas com cada uma das vari�veis do arquivo
# 2) Escolha uma das vari�veis e construa histogramas para o sexo
# feminino e masculino
# 3) Construa um boxplot para vari�vel escolhida no exerc�cio anterior,
# fazendo distin��o entre os sexos feminino e masculino
# 4) Interprete




# Aula 1 - introdu��o ao uso do R
# inserir uma base de dados de vetor �nico
altura = c(1.72,1.60,1.65,1.63,1.58,1.59,1.72,1.60,1.67,1.77,1.70,1.68,1.62,1.65,
           1.64,1.63,1.77,1.65,1.68,1.82,1.68,1.64,1.65,1.58,1.65,1.58)
#c = concatenar -> inserir dados dentro de um vetor
#Estat�stica descritiva
# S�o realizadas com a fun��o summary
summary (altura)
hist(altura,col="pink",ylab=c("Frequ�ncias absolutas"))
# Histograma demonstra graficamente a distribui��o das frequ�ncias
boxplot(altura)
pe=c(37,37,37,35,35,36,35,36,36,37,37,36,37,35,36,37,35,35,36,34)
summary(pe)
sd(pe)
hist(pe,col="red",ylab=c("Frequ�ncias absolutas"))
boxplot(pe)
# Em m�dia as pessoas da turma de Bioci�ncias apresentam tamanho do p� equivalente a 37 +-2, ou seja, a maioria da amostra variou entre 35 a 39. 25% das pessoas tem o tamanho do p� variando entre 34