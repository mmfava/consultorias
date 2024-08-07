
#####################################################################
##                                                                 ##
## Aula Pr�tica 1 - ESTATISTICA DESCRITIVA (PRINCIPAIS CONCEITOS)  ##
##                                                                 ##
#####################################################################

## REGRAS GERAIS

## ctrl+L - limpar a tela
## Iremos importar os dados de uma planilha externa elaborada com o Excel (Milsa.xls)
## Para isso o R necessita de um pacote externo chamado GData
## Em sua casa, com uma internet livre, voce instala o pacote fazendo, na linha de
## comando da workspace...
## install.packages('gdata', depend=TRUE)
## em seguida, vc precisa carregar o GData, para isso...
require(gdata)

## o comando para ler o arquivo excel no formato de tabela eh read.csv2, conforme apresentado abaixo:
milsa = read.csv2(file.choose(),header=TRUE,dec=",", strip.white=TRUE)

## file.choose() abre janela para escolha de arquivo
## header=TRUE determina que a primeira linha do arquivo eh o cabe�alho
## dec: informa o separador decimal

## com isso obtivemos o objeto 'milsa' do tipo tabela 
## O objeto milsa contem v�rias colunas
head(milsa,3) #mostra as 3 primeiras linhas do arquivo

## Para acessarmos qualquer das colunas, basta separar o nome do objeto 
## do nome da coluna por um sinal de $

milsa$EC ##mostra o vetor estado civil

## para visualizar todos os nomes do objeto, utiliza-se o comando 'names'

names(milsa)

## para disponibilizar todos os objetos para realizar an�lise, devemos "atachar" as informa��es:
attach(milsa)


## TRATAMENTO DOS DADOS

## Substituindo os n�meros pelos seus equivalentes

milsa$EC = ifelse(milsa$EC==1,"solteiro","casado")
milsa$GI = ifelse(milsa$GI==1,"1oGrau",ifelse(milsa$GI==2,"2oGrau","3oGrau"))
milsa$RP = ifelse(milsa$RP==1,"capital",ifelse(milsa$RP==2,"interior","outro"))

## Sum�rio de estat�sticas descritivas

summary(milsa)

## Tabela de distribui��o de frequ�ncias

## o comando do R para construir tabelas de frequencias absolutas eh o table
table(milsa$EC)
table(milsa$GI)
table(milsa$RP)

## o comando do R para construir tabelas de frequencias relativas eh o prop.table
prop.table(table(milsa$EC))
round(prop.table(table(milsa$EC))*100,1)

## Diferentes representa��es gr�ficas
boxplot(SL) ## gr�fico do tipo Boxplot
pie(table(milsa$GI), clockwise=TRUE, density = 10, angle = 15 + 5 * 1:3) ## gr�fico do tipo pizza
hist(SL, breaks="Sturges") ## gr�fico do tipo Histograma, aplicando-se a regra de Sturges

boxplot(SL~EC, xlab="Estado Civil", ylab="Sal�rio")

library(ggplot2)
qplot(milsa$EC,milsa$SL,geom="boxplot",xlab="Estado Civil", ylab="Sal�rio")


