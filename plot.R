## COMPARACAO MULTIPLA
## -------------------

## Sempre é importante fazer a demonstração gráfica dos resultados - Box-whisker

## Desvio padrão
(desvio=with(itens, tapply(n,Stages, sd)))

## Médias
(medias = with(itens, tapply(n, Stages, mean)))

plot(medias, pch=15,ylim=c(-0.5,ceiling(max(medias)+max(desvio))))
arrows(1:4, medias + desvio, 1:4, medias - desvio, angle = 90
       ,
       code = 3, length = 0.1)
