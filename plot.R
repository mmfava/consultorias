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
