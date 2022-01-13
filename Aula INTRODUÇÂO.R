##criando um vetor
#c significa contatenar
#ponto é virgula
#vírgula é ponto
#todo vetor precisa de um nome, que é precedido de =
idade=c(36,23,21,22,27,24,38,22,
        23,48,25,35,30,28,31,21,28,25,25)
idade
summary(idade)
boxplot(idade)
barplot(idade)
plot(idade,col="red",xlab="pessoas")
tamanhodopé=c(41,37,38,40,37,34,39,38,37,41,36,35,36,40,43,37,36,37,37)
summary(tamanhodopé)
boxplot(tamanhodopé)
summary(idade,tamanhodopé)
##abrindo arquivos do excel


pamela=read.csv2(file.choose(),dec=",",header=TRUE,strip.white=TRUE)
