#######################################################################
#                           FREQUÊNCIAS                               #
#                        REGRA DE STURGES                             #
#           Profa. Ana Tereza - 3 de novembro de 2015                 #
#######################################################################

## A Regra de Sturges é uma ferramenta estatística que permite que 
## agrupemos valores de variáveis quantitativas em classes numéricas.
## A Regra de Sturges consiste em calcularmos o número de classes (k) de 
## uma tabela de distribuição de frequências e a amplitude das respectivas
## classes (AC). Para o cálculo de k usamos o algoritmo:
##                    k = 1+3.322*log(n)
## E para o cálculo da amplitude de classe (AC), usamos:
##                    AC = (máximo-mínimo)/k
##
## Faremos este procedimento permite que se construa um histograma. O
## histograma é uma representação gráfica da distribuição de frequências
## em classes numéricas. A partir da interpretação de um histograma,
## podemos verificar o tipo de distribuição dos dados (exemplo: normal,
## exponencial, logaritmica etc).
## Abaixo faremos um breve script sobre a aplicação da Regra de Sturges:


exercicio = data.frame(
  tratamento = rep(1:2,each=6),
  resposta = c(14,14,13,12,15,13,24,25,19,22,18,22))

## data.frame - tabulação de dados de forma pareada
## rep(1:3, each=6) - são 3 tratamentos, cada qual com 6 réplicas
## c - concatenar
## 'tratamento' é o nome da variável que separa os 3 tratamentos
## 'resposta' é a variável medida, que no caso é batimentos cardíacos

attach(exercicio)

## Primeiramente faremos um gráfico do tipo Boxplot para visualizar a
## tendência dos dados entre os tratamentos.

boxplot(resposta~tratamento, names=c("Sedentário","Após 
                                     o exercício", "Após 5 minutos do 
                                     exercício"), ylab="batimentos cardíacos")

## Em seguida, faremos a Regra de Sturges ao contrário: primeiro,o 
## histograma, depois as classes e em seguida as frequências.

## Histograma com todos os dados
hist(resposta)

## Histograma apenas com o grupo 1
hist(resposta[exercicio$tratamento==1])

## Histograma apenas com o grupo 2
hist(resposta[exercicio$tratamento==2])

## Histograma apenas com o grupo 3
hist(resposta[exercicio$tratamento==3])

## Contruiremos as classes e as frequências
tabela=hist(resposta,breaks="Sturges")
tabela

## Exercício

saude=read.csv2(file.choose(),header = T,dec=",",sep = "")

# 1) Construa histogramas com cada uma das variáveis do arquivo
# 2) Escolha uma das variáveis e construa histogramas para o sexo
# feminino e masculino
# 3) Construa um boxplot para variável escolhida no exercício anterior,
# fazendo distinção entre os sexos feminino e masculino
# 4) Interprete




# Aula 1 - introdução ao uso do R
# inserir uma base de dados de vetor único
altura = c(1.72,1.60,1.65,1.63,1.58,1.59,1.72,1.60,1.67,1.77,1.70,1.68,1.62,1.65,
           1.64,1.63,1.77,1.65,1.68,1.82,1.68,1.64,1.65,1.58,1.65,1.58)
#c = concatenar -> inserir dados dentro de um vetor
#Estatística descritiva
# São realizadas com a função summary
summary (altura)
hist(altura,col="pink",ylab=c("Frequências absolutas"))
# Histograma demonstra graficamente a distribuição das frequências
boxplot(altura)
pe=c(37,37,37,35,35,36,35,36,36,37,37,36,37,35,36,37,35,35,36,34)
summary(pe)
sd(pe)
hist(pe,col="red",ylab=c("Frequências absolutas"))
boxplot(pe)
# Em média as pessoas da turma de Biociências apresentam tamanho do pé equivalente a 37 +-2, ou seja, a maioria da amostra variou entre 35 a 39. 25% das pessoas tem o tamanho do pé variando entre 34