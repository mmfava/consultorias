itens=read.csv2(file.choose(),header=TRUE,sep=";")


## Teste de normalidade: a an�lise dos residuos � mais importante, ent�o � 
## necess�rio realizar o ajuste de um modelo linear:

item.model = lm(formula = n ~ Stages,data = itens)

item.predito=fitted(object=item.model)

item.residuos=resid(object=item.model)

shapiro.test(item.residuos)


## ANOVA: uma vez que os dados s�o homoced�sticos e os res�duos est�o em 
## normalidade, devemos fazer a An�lise da Vari�ncia

anova(item.model)




## COMPARACAO MULTIPLA
## -------------------

## Sempre � importante fazer a demonstra��o gr�fica dos resultados - Box-whisker

## Desvio padr�o
(desvio=with(itens, tapply(n,Stages, sd)))

## M�dias
(medias = with(itens, tapply(n, Stages, mean)))

plot(medias, pch=15,ylim=c(-0.5,ceiling(max(medias)+max(desvio))))
arrows(1:4, medias + desvio, 1:4, medias - desvio, angle = 90
       ,
       code = 3, length = 0.1)

library(lattice) 
library(grid) 
grid.newpage() 
pushViewport(viewport(angle = 90))
upViewport() 
plot(medias, pch=15,ylim=c(-0.5,ceiling(max(medias)+max(desvio))))
arrows(1:4, medias + desvio, 1:4, medias - desvio, angle = 90,
       code = 3, length = 0.1)

print(xyplot(medias ~ Stages, auto.key = TRUE), draw.in = "VP")
names(itens$Stages)

x <- 1:10 
print(xyplot(x ~ x, groups = gl(2, 5), auto.key = TRUE), draw.in = "VP")
names(itens$Stages)
## Contagem (n)
with(itens, tapply(n, Stages, length))



## Teste de Tukey

summary(milho.aov <- aov(producao ~ variedade, data = milho))
TukeyHSD(milho.aov, "variedade", ordered = TRUE)
plot(TukeyHSD(milho.aov, "variedade"))


milho.aov= aov(producao ~ variedade, data = milho)