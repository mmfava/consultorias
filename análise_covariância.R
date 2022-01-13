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
# existe 2 tipos de análise de covariância:
# * a covariável funciona como bloco, entra no modelo com o objetivo de explicar parte da
# variação, é uma variável não controlada pelo pesquisador, assume-se não interagir com os
# efeitos de interesse, normalmente entra apenas com termo linear. Trataremos isso agora.
# * a covariável é controlada (ou parcialmente) pelo pesquisar, há o interesse de saber sua
# interação com outros termos, pode-se estudar diferentes especificações de modelo.

#------------------------------------------------------------------------------------------
# dados de experimento com nutrição de suínos. Animais foram pesados antes do experimento e
# tinham idade conhecida. Essas variáveis contínuas foram usadas para explicar/corrigir 
# parte da variação presente e melhor comparar os níveis de energia na ração fornecidos
#ac <- read.table("http://www.leg.ufpr.br/~walmes/cursoR/ancova.txt", header=TRUE)
sui <- read.table("../dados/ancova.txt", header=TRUE)
str(sui)

#------------------------------------------------------------------------------------------
# número de animais para cada combinação de níveis de sexo e energia
with(sui, tapply(peso28, list(sexo, energia), length))

#------------------------------------------------------------------------------------------
# gráficos, distribuição das id e pi nos grupos formados pelos fatores categóricos
require(lattice)
xyplot(pi~id|sexo, groups=energia, data=sui, cex=2, pch=19, auto.key=TRUE)
xyplot(pi~id|energia, groups=sexo, data=ac, cex=2, pch=19, auto.key=TRUE)

#------------------------------------------------------------------------------------------
# pode-se fazer uma anova dessas covariáveis para verificar se elas são separadas ou
# confundidas com os fatores experimentais
anova(aov(pi~sexo*energia, data=sui)) # testa se pi é separada por sexo*energia
anova(aov(id~sexo*energia, data=sui)) # testa se id é separada por sexo*energia
# não significativo é o resultado esperado se os tratamentos foram casualizados aos animais

#------------------------------------------------------------------------------------------
# análise de variância (em experimentos não ortogonais a ordem dos termos é importante!)
m0 <- aov(peso28~sexo*energia, data=sui)        # modelo só com os fatores categoricos
summary(m0)
m1 <- aov(peso28~pi+id+sexo*energia, data=sui)  # modelo com os categóricos e contínuos
summary(m1) # veja redução na SQ, parte da varição é explicada por pi e id
m1 <- aov(peso28~id+pi+sexo*energia, data=sui)
summary(m1)
anova(m0, m1) # testa o poder de explicação dos "blocos" contínuos

#------------------------------------------------------------------------------------------
# checagem
par(mfrow=c(2,2)); plot(m1); layout(1)

#------------------------------------------------------------------------------------------
# estimativas
summary.lm(m1)

#------------------------------------------------------------------------------------------
# dado que há efeito de sexo após correção da variação para pi e id, fazer teste de médias
# deve-se escolher o valor das covariáveis a ser fixado para comparar os níveis de sexo
mean(sui$pi) # média amostral de peso inicial dos animais do experimento (menor erro padrão)
mean(sui$id) # média amostral de idade dos animais do experimento (menor erro padrão)

#------------------------------------------------------------------------------------------
# ajuste do modelo com a função lm
m0 <- lm(peso28~pi+id+sexo*energia, data=ac)
anova(m0)

#------------------------------------------------------------------------------------------
.


#===========================================================================================
# code chunk number 3: cap14ancova.Rnw:93-169 (eval = FALSE)
#===========================================================================================
.
#------------------------------------------------------------------------------------------
# para fazer os contrastes entre níveis de sexo, para um animal com peso e idade médios
require(contrast)
levels(sui$sexo)    # níveis do fator
levels(sui$energia) # níveis do fator
pim <- mean(sui$pi)
idm <- mean(sui$id)
m1 <- lm(peso28~id+pi+sexo*energia, data=sui) # modelo deve ser de classe lm para contrast

#------------------------------------------------------------------------------------------
# femea vs macho castrado (observe que os erros padrões dos contrastes são diferentes)
c0 <- contrast(m1, type="average",
               list(sexo="F", energia=levels(sui$energia), pi=pim, id=idm),
               list(sexo="MC", energia=levels(sui$energia), pi=pim, id=idm))
c0

#------------------------------------------------------------------------------------------
# femêa vs macho imunocatrado
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
# as médias marginais populacionais de sexo na média das 3 rações com pi e id médios
med <- sapply(levels(sui$sexo), simplify=FALSE,
              function(s){
                contrast(m1, type="average",
                         list(sexo=s, energia=levels(sui$energia), pi=pim, id=idm))[1:7]
              })
str(med)
med <- do.call(rbind, lapply(med, data.frame))
med <- med[order(med$Contrast),]

#------------------------------------------------------------------------------------------
# gráfico de barras com IC para a média
require(gplots)
bp <- with(med, barplot2(Contrast, ylim=c(120, 130), xpd=FALSE, plot.ci=TRUE,
                         ci.l=Lower, ci.u=Upper, ylab="Peso aos 28 dias"))
axis(1, at=bp, labels=rownames(med), tick=FALSE)
box()

#------------------------------------------------------------------------------------------
# gráfico de barras com as médias e resultado da comparação
bp <- barplot(med$Contrast, ylim=c(120, 130), xpd=FALSE, ylab="Peso aos 28 dias")
text(bp, med$Contrast,
     label=paste(format(med$Contrast, dig=5), c("ab","b","a")), pos=3)
axis(1, at=bp, labels=rownames(med), tick=FALSE)
box()

#------------------------------------------------------------------------------------------
# gráfico com IC e médias
bp <- with(med, barplot2(Contrast, ylim=c(120, 130), xpd=FALSE, plot.ci=TRUE,
                         ci.l=Contrast, ci.u=Upper, ylab="Peso aos 28 dias"))
axis(1, at=bp, labels=rownames(med), tick=FALSE)
text(bp, med$Contrast,
     label=paste(format(med$Contrast, dig=5), c("b","ab","a")), pos=1)
box()

#------------------------------------------------------------------------------------------