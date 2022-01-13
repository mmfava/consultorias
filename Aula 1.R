## Abrir arquivos tipo .csv

milsa=read.csv2(file.choose(),header=TRUE,dec=",",
                strip.white=TRUE)

## Apresente as primeiras 3 linhas
head(milsa,3)

## Como substituir números pelos seus equivalentes 
## categóricos

attach(milsa)

milsa$EC=ifelse(EC==1,"solteiro","casado")
milsa$GI=ifelse(GI==1,"fundamental",
                ifelse(GI==2,"medio","superior"))
milsa$RP=ifelse(RP==1,"capital",
                ifelse(RP==2,"interior","outro"))

## Estatísticas descritivas para as variáveis quantitativas

summary(milsa$NF)

## Elaboração gráfica

boxplot(NF,xlab="amostra total",
        ylab="Número de filhos")

boxplot(SL~EC,xlab="Estado Civil",
        ylab="Salário")

## Para colocar nome das categorias no eixo
## x, é necessário instalar um pacote de
## elementos gráficos

## no menu devo clicar em Tool
## em seguida, "Install Packages"

library(ggplot2)

qplot(milsa$EC,milsa$SL,geom="boxplot",
      xlab="Estado Civil",
      ylab="Salário (SM)")

## Nomes das variáveis
names(milsa)
