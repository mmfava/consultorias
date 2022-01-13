## COMPARACAO MULTIPLA
## -------------------

## Sempre é importante fazer a demonstração gráfica dos resultados - Box-whisker

## Desvio padrão
(desvio=with(itens, tapply(n,Stages, sd)))

## Mádias
(medias = with(itens, tapply(n, Stages, mean)))

plot(medias, pch=15,ylim=c(-0.5,ceiling(max(medias)+max(desvio))))
arrows(1:4, medias + desvio, 1:4, medias - desvio, angle = 90,code = 3, length = 0.1)

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
