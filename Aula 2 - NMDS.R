###############################################################################
##                Non-metric multidimensional scaling (NMDS)                 ##
##         by http://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/          ##
###############################################################################

## Vamos plotar os dados de abund?ncia da esp?cie 1 em tr?s comunidades

plot(0:10,0:10,type="n",axes=F,xlab="Abund?ncia da Esp?cie 1",ylab="") 
axis(1)
points(5,0); text(5.5,0.5,labels="community A")
points(3,0); text(3.2,0.5,labels="community B")
points(0,0); text(0.8,0.5,labels="community C")

## Agora vamos plotar os dados de abund?ncia da esp?cie 2 nestas mesmas tr?s comunidades

plot(0:10,0:10,type="n",xlab="Abundance of Species 1", 
     ylab="Abundance of Species 2")
points(5,5); text(5,4.5,labels="community A")
points(3,3); text(3,3.5,labels="community B")
points(0,5); text(0.8,5.5,labels="community C")

## Vamos pirar um pouco, e vamos colocar um terceiro eixo, mostrando os dados de abund?ncia
## de uma terceira esp?cie. Para isso, teremos que instalar o pacote "scatterplot3D)

install.packages("scatterplot3d")
library(scatterplot3d)
d=scatterplot3d(0:10,0:10,0:10,type="n",xlab="Abundance of Species 1",
                ylab="Abundance of Species 2",zlab="Abundance of Species 3"); d
d$points3d(5,5,0); text(d$xyz.convert(5,5,0.5),labels="community A")
d$points3d(3,3,3); text(d$xyz.convert(3,3,3.5),labels="community B")
d$points3d(0,5,5); text(d$xyz.convert(0,5,5.5),labels="community C")

## O objetivo da NMDS ? representar a posi??o original das comunidades em um espa?o multidimensional da forma 
## mais acurada poss?vel, usando um reduzido n?mero de dimens?es de forma a facilitar a visualiza??o gr?fica
## (and to spare your thinker). Basicamente, ? isso que vamos fazer na NMDS.


## UAUUUUUU, VAMOS BRINCAR DISSO!!!!!!!!!!!!! LET'S GO!!!!!!!!!!

## NMDS n?o usa abund?ncias absolutas das esp?cies nas comunidades, mas sim seus ranqueamentos. O uso de rankings omite
## alguns problemas associados com o uso de dist?ncias absolutas e como resultado ? uma t?cnica muito mais flex?vel
## que aceita a elevada variedade de tipos de dados (It's also where the "non-metric" part of the name comes from).

## O NMDS ? um procedimento iterativo e apresenta uma s?rie de passos de execu??o:

## 1. Definir a posi??o original das comunidades em um espa?o multidimensional;
## 2. Especificar o n?mero de dimens?es reduzidas (tipicamente 2);
## 3. Construir uma configura??o inicial das amostras em 2 dimens?es;
## 4. Construir uma regress?o entre as dist?ncias em suas configura??es iniciais e as dist?ncias observadas (medidas);
## 5. Determinar o estresse, ou o desacordo entre a configura??o 2D e os valores pela regress?o. Se a configura??o
## 2D preservar perfeitamente as ordens do ranqueamento original , ent?o o plot de um contra o outro dever? ter um  
## crescimento monot?nico. Por outro lado, se a configura??o 2D diferir do crescimento monot?nico, ent?o isso define o 
## grau do estresse. Esta rela??o ? frequentemente visualizada em um diagrama denominado de Shepard plot.
## 6. Se o estresse for alto, ? preciso reposicionar os pontos nas 2 dimens?es para diminuir o estresse, e repetir este
## procedimento at? o estresse for menor do que um limiar. Uma boa regra: estresse > 0.05 representa uma excelente 
## representa??o em dimens?es reduzidas, estresse > 0.1 ? ?timo, estresse > 0.2 ? bom/ok, e estresse > 0.3 ? uma 
## representa??o ruim.
 

## Informa??es adicionais: a configura??o final pode variar de acordo com a configura??o inicial (que muitas vezes ?
## aleat?ria) e o n?mero de itera??es. Por isso, ? aconselh?vel rodar a NMDS in?meras vezes e comparar a interpreta??o
## do diagrama que apresenta o menor estresse.

## Para come?ar, NMDS requer uma matriz de dist?ncias, ou matriz de dissimilaridades. A dist?ncia euclidiana bruta 
## n?o ? ideal para este prop?sito, pois ela ? sens?vel ao total de abund?ncias, ent?o ela pode considerar locais 
## com n?meros de esp?cies parecidos como mais similares, ainda que as identidades das esp?cies sejam diferentes. 
## A dist?ncia Euclidiana ? sens?vel tamb?m a aus?ncia de esp?cies, ent?o acaba tratando locais com o mesmo n?mero de
## esp?cies ausentes como mais similares
 
## Consequentemente, ec?logos usam a dissimilaridade de Bray-Curtis, a qual apresenta as seguintes propriedades:
## - N?o ? vari?vel para mudan?as de unidades;
## - N?o ? afetada pela adi??o ou remo??o de esp?cies que n?o est?o presentes em duas comunidades;
## - N?o ? afetada pela adi??o de uma nova comunidade;
## - Pode reconhecer diferen?as no total de abund?ncias quando as abund?ncias relativas s?o as mesmas.
  

install.packages("vegan")
library(vegan)
set.seed(2)
community_matrix=matrix(
  sample(1:100,300,replace=T),nrow=10,
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))

community_matrix
example_NMDS=metaMDS(community_matrix, # Nossa matriz de abund?ncias das esp?cies
                     k=2) # N?mero de redu??o de dimens?es

## Voc? pode ver cada uma das itera??es da NMDS at? que a solu??o seja alcan?ada (i.e., o estreese seja minimizado 
## ap?s algumas reconfigura??es dos pontos em 2 dimens?es). Voc? pode aumentar o n?mero de itera??es, usando um 
## argumento 'trymax=', o qual pode auxiliar os problemas na n?o-converg?ncia. Se um estresse alto ? o seu problema, 
## aumente o n?mero de dimens?es para 3. 

example_NMDS=metaMDS(community_matrix,k=2,trymax=999)

## Voc? poder? ver que a fun??o 'metaMDS' aplicou automaticamente uma transforma??o de raiz quadrada e calculou as
## dist?ncias de Bray-Curtis para a nossa matriz de esp?cies da comunidade.

## Agora vamos examinar o Diagrama de Shepard, o qual mostra a dispers?o dos dados em torno da regress?o das 
## dist?ncias preditas e observadas na configura??o final (i.e. dist?ncias entre os pares de comunidades).

stressplot(example_NMDS)

## Uma elevada dispers?o em torno da linha sugere que as dist?ncias originais n?o est?o sendo bem preservadas no n?mero
## de dimens?es reduzidas. Neste caso est? excelente!!!

## Agora n?s podemos plotar o NMDS. O diagrama nos mostra todas as comunidades (locais = circulos) e esp?cies (cruzes),
## mas n?s n?o sabemos qual c?rculo corresponde a qual local, e qual esp?cie corresponde a cada cruz.

plot(example_NMDS)

## Ent?o, n?s podemos usar as fun??es 'ordiplot' e 'orditorp' para adicionar textos no diagrama no lugar dos pontos,
## e assim fazer mais sentido nessa confus?o.
ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",cex=0.5,air=0.01)

## E ? isso! Existem algumas dicas e truques a mais que podemos demonstrar.
## Vamos propor que as comunidades 1 a 5 tiveram a aplica??o de determinado tratamento, e as comunidades 6 a 10 um outro
## tratamento. Nos podemos desenhar um pol?gono que conectar as respectivas comunidades no diagrama. Essa dica ? intuitiva,
## pois permite compreender como as comunidades e agrupamentos de esp?cies est?o relacionadas com os tratamentos.

genero=c(rep("Rincospora",5),rep("Cyperus",5))
ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups=genero,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=0.8)

## Podemos tamb?m plotar "gr?ficos aranha" usando a fun??o 'orderspider', ou elipse usando a fun??o 'ordiellipse',
## ou uma ?rvore m?nima (minimum spanning tree - MST) usando a fun??o 'orcluster', o qual conecta as comunidades
## similares (utilizada para visualizar se os tratamentos s?o efetivos no controle da estrutura da comunidade).

## N?s podemos tamb?m colorir os pol?gonos dos tratamentos.
## Primeiro, criaremos um vetor de cores com valores que correspondem ao mesmo comprimento do vetor dos tratamentos.
colors=c(rep("red",5),rep("blue",5))
ordiplot(example_NMDS,type="n")

## Plotar os pol?gonos com as cores baseadas nos tratamentos.
for(i in unique(treat)) {
  ordihull(example_NMDS$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),
                                            rep("blue",5)),air=0.01,cex=1.25)

## Se os tratamentos forem cont?nuos, tais como um gradiente ambiental, ent?o ? usual plotar contornos ao inv?s 
## de pol?gonos. Podemos simplificar utilizando, por exemplo, dados de eleva??o para nossa matriz de esp?cies original
## e sobrepor ao nossso diagrama de NMDS por meio da fun??o 'ordisurf'.
## Para isso, devemos definir randomicamente as eleva??es para o exemplo:
elevation=runif(10,20,30)

## Usar a fun??o 'ordisurf' para plotar os contornos:
ordisurf(example_NMDS,elevation,main="",col="forestgreen")

# Finalmente, plotamos as esp?cies
orditorp(example_NMDS,display="species",col="grey30",air=0.1,
         cex=1)

## ? poss?vel fazer este tipo de diagrama com outras vari?veis cont?nuas, tais como temperatura.