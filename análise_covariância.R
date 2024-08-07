# R code from vignette source 'cap14ancova.Rnw'
# Encoding: UTF-8

#===========================================================================================
# code chunk number 1: cap14ancova.Rnw:3-4
#===========================================================================================
options(prompt=" ", continue=" ")


#===========================================================================================
# code chunk number 2: cap14ancova.Rnw:23-88 (eval = FALSE)
#===========================================================================================
.
#------------------------------------------------------------------------------------------
# existe 2 tipos de an�lise de covari�ncia:
# * a covari�vel funciona como bloco, entra no modelo com o objetivo de explicar parte da
# varia��o, � uma vari�vel n�o controlada pelo pesquisador, assume-se n�o interagir com os
# efeitos de interesse, normalmente entra apenas com termo linear. Trataremos isso agora.
# * a covari�vel � controlada (ou parcialmente) pelo pesquisar, h� o interesse de saber sua
# intera��o com outros termos, pode-se estudar diferentes especifica��es de modelo.

#------------------------------------------------------------------------------------------
# dados de experimento com nutri��o de su�nos. Animais foram pesados antes do experimento e
# tinham idade conhecida. Essas vari�veis cont�nuas foram usadas para explicar/corrigir 
# parte da varia��o presente e melhor comparar os n�veis de energia na ra��o fornecidos
#ac <- read.table("http://www.leg.ufpr.br/~walmes/cursoR/ancova.txt", header=TRUE)
sui <- read.table("../dados/ancova.txt", header=TRUE)
str(sui)

#------------------------------------------------------------------------------------------
# n�mero de animais para cada combina��o de n�veis de sexo e energia
with(sui, tapply(peso28, list(sexo, energia), length))

#------------------------------------------------------------------------------------------
# gr�ficos, distribui��o das id e pi nos grupos formados pelos fatores categ�ricos
require(lattice)
xyplot(pi~id|sexo, groups=energia, data=sui, cex=2, pch=19, auto.key=TRUE)
xyplot(pi~id|energia, groups=sexo, data=ac, cex=2, pch=19, auto.key=TRUE)

#------------------------------------------------------------------------------------------
# pode-se fazer uma anova dessas covari�veis para verificar se elas s�o separadas ou
# confundidas com os fatores experimentais
anova(aov(pi~sexo*energia, data=sui)) # testa se pi � separada por sexo*energia
anova(aov(id~sexo*energia, data=sui)) # testa se id � separada por sexo*energia
# n�o significativo � o resultado esperado se os tratamentos foram casualizados aos animais

#------------------------------------------------------------------------------------------
# an�lise de vari�ncia (em experimentos n�o ortogonais a ordem dos termos � importante!)
m0 <- aov(peso28~sexo*energia, data=sui)        # modelo s� com os fatores categoricos
summary(m0)
m1 <- aov(peso28~pi+id+sexo*energia, data=sui)  # modelo com os categ�ricos e cont�nuos
summary(m1) # veja redu��o na SQ, parte da vari��o � explicada por pi e id
m1 <- aov(peso28~id+pi+sexo*energia, data=sui)
summary(m1)
anova(m0, m1) # testa o poder de explica��o dos "blocos" cont�nuos

#------------------------------------------------------------------------------------------
# checagem
par(mfrow=c(2,2)); plot(m1); layout(1)

#------------------------------------------------------------------------------------------
# estimativas
summary.lm(m1)

#------------------------------------------------------------------------------------------
# dado que h� efeito de sexo ap�s corre��o da varia��o para pi e id, fazer teste de m�dias
# deve-se escolher o valor das covari�veis a ser fixado para comparar os n�veis de sexo
mean(sui$pi) # m�dia amostral de peso inicial dos animais do experimento (menor erro padr�o)
mean(sui$id) # m�dia amostral de idade dos animais do experimento (menor erro padr�o)

#------------------------------------------------------------------------------------------
# ajuste do modelo com a fun��o lm
m0 <- lm(peso28~pi+id+sexo*energia, data=ac)
anova(m0)

#------------------------------------------------------------------------------------------
.


#===========================================================================================
# code chunk number 3: cap14ancova.Rnw:93-169 (eval = FALSE)
#===========================================================================================
.
#------------------------------------------------------------------------------------------
# para fazer os contrastes entre n�veis de sexo, para um animal com peso e idade m�dios
require(contrast)
levels(sui$sexo)    # n�veis do fator
levels(sui$energia) # n�veis do fator
pim <- mean(sui$pi)
idm <- mean(sui$id)
m1 <- lm(peso28~id+pi+sexo*energia, data=sui) # modelo deve ser de classe lm para contrast

#------------------------------------------------------------------------------------------
# femea vs macho castrado (observe que os erros padr�es dos contrastes s�o diferentes)
c0 <- contrast(m1, type="average",
               list(sexo="F", energia=levels(sui$energia), pi=pim, id=idm),
               list(sexo="MC", energia=levels(sui$energia), pi=pim, id=idm))
c0

#------------------------------------------------------------------------------------------
# fem�a vs macho imunocatrado
c1 <- contrast(m1, type="average",
               list(sexo="F", energia=levels(sui$energia), pi=pim, id=idm),
               list(sexo="MI", energia=levels(sui$energia), pi=pim, id=idm))
c1

#------------------------------------------------------------------------------------------
# macho castrado vs macho imunocastrado
c2 <- contrast(m1, type="average",
               list(sexo="MI", energia=levels(sui$energia), pi=pim, id=idm),
               list(sexo="MC", energia=levels(sui$energia), pi=pim, id=idm))
c2

#------------------------------------------------------------------------------------------
# passando os contrastes para a multcomp corrigir os p-valores
cm <- rbind(c0$X, c1$X, c2$X)
rownames(cm) <- c("FvsMC","FvsMI","MIvsMC")
require(multcomp)
summary(glht(m1, linfct=cm))

#------------------------------------------------------------------------------------------
# as m�dias marginais populacionais de sexo na m�dia das 3 ra��es com pi e id m�dios
med <- sapply(levels(sui$sexo), simplify=FALSE,
              function(s){
                contrast(m1, type="average",
                         list(sexo=s, energia=levels(sui$energia), pi=pim, id=idm))[1:7]
              })
str(med)
med <- do.call(rbind, lapply(med, data.frame))
med <- med[order(med$Contrast),]

#------------------------------------------------------------------------------------------
# gr�fico de barras com IC para a m�dia
require(gplots)
bp <- with(med, barplot2(Contrast, ylim=c(120, 130), xpd=FALSE, plot.ci=TRUE,
                         ci.l=Lower, ci.u=Upper, ylab="Peso aos 28 dias"))
axis(1, at=bp, labels=rownames(med), tick=FALSE)
box()

#------------------------------------------------------------------------------------------
# gr�fico de barras com as m�dias e resultado da compara��o
bp <- barplot(med$Contrast, ylim=c(120, 130), xpd=FALSE, ylab="Peso aos 28 dias")
text(bp, med$Contrast,
     label=paste(format(med$Contrast, dig=5), c("ab","b","a")), pos=3)
axis(1, at=bp, labels=rownames(med), tick=FALSE)
box()

#------------------------------------------------------------------------------------------
# gr�fico com IC e m�dias
bp <- with(med, barplot2(Contrast, ylim=c(120, 130), xpd=FALSE, plot.ci=TRUE,
                         ci.l=Contrast, ci.u=Upper, ylab="Peso aos 28 dias"))
axis(1, at=bp, labels=rownames(med), tick=FALSE)
text(bp, med$Contrast,
     label=paste(format(med$Contrast, dig=5), c("b","ab","a")), pos=1)
box()

#------------------------------------------------------------------------------------------