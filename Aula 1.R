## Abrir arquivos tipo .csv

milsa=read.csv2(file.choose(),header=TRUE,dec=",",
                strip.white=TRUE)

## Apresente as primeiras 3 linhas
head(milsa,3)

## Como substituir n�meros pelos seus equivalentes 
## categ�ricos

attach(milsa)

milsa$EC=ifelse(EC==1,"solteiro","casado")
milsa$GI=ifelse(GI==1,"fundamental",
                ifelse(GI==2,"medio","superior"))
milsa$RP=ifelse(RP==1,"capital",
                ifelse(RP==2,"interior","outro"))

## Estat�sticas descritivas para as vari�veis quantitativas

summary(milsa$NF)

## Elabora��o gr�fica

boxplot(NF,xlab="amostra total",
        ylab="N�mero de filhos")

boxplot(SL~EC,xlab="Estado Civil",
        ylab="Sal�rio")

## Para colocar nome das categorias no eixo
## x, � necess�rio instalar um pacote de
## elementos gr�ficos

## no menu devo clicar em Tool
## em seguida, "Install Packages"

library(ggplot2)

qplot(milsa$EC,milsa$SL,geom="boxplot",
      xlab="Estado Civil",
      ylab="Sal�rio (SM)")

## Nomes das vari�veis
names(milsa)
