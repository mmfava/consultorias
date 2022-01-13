dados <- read.table("acp_alface.txt", header = T)   
dados

x <- dados[,2:7]
x

row.names(x) = dados[,1]
x

###################################
## AGRUPAMENTO DAS VARI?VEIS ##
###################################

R <- cor(x)
R # matriz de correlação Pearson 

r <- as.dist((1-(R^2)))  ## SUGERIDA POR RENCHER (2002) - (1-r person^2) = diferenças
r


## !!! o hclust só utiliza medidas de dissimilaridade no R
##     por isso nos fizemos 1- coeficiente de correlação

## ligação simples - método do vizinho mais próximo
aa_single <- hclust(r, "single")    
aa_single
plot(aa_single, xlab="", sub="", main="(1-r²)", ylab="Dissimilaridade")
aa_single$height       ## valores de junção no dendograma ##
d2 <- cophenetic(aa_single) 
cor(r,d2)              ## coeficiente de correlação cofenética ##
cor.test(r,d2) # ccc < 0,7

par(mfrow=c(1,3))
plot(aa_single, xlab = "Variáveis", ylab = "Função da Correlação", main = "", sub="")
plot(aa_single, xlab = "Variáveis", ylab = "Função da Correlação", main = "", sub="", hang = -1)
plot(aa_single, xlab = "Variáveis", ylab = "Função da Correlação", main = "", sub="", hang = -1, labels = dados$pais)
rect.hclust(aa_single, k = 3)
dev.off()

## !! Usamos o hang para iniciar os indivíduos todos em uma mesma linha - ele não muda 
##    a interpretação do gráfico, apenas o visual. 

## Ligaçãoo completa - vizinho mais distânte
aa_com <- hclust(r, "complete")     
aa_com
aa_com$height       ## valores de junção no dendograma ##
d2 <- cophenetic(aa_com)
cor(r,d2)              ## coeficiente de correlação cofenética ##
cor.test(r,d2)
plot(aa_com, xlab = "Variáveis", ylab = "Função da Correlação", main = "", sub="",hang = -1)

## Método de Ward
aa_ward <- hclust(r, "ward.D2")       ## Ward ##
aa_ward
d2 <- cophenetic(aa_ward)
cor(r,d2)              ## coeficiente de correla??o cofen?tica ##
cor.test(r,d2)
plot(aa_ward, xlab = "Variáveis", ylab = "Função da Correlação", main = "", sub="", hang = -1)


###################################
## AGRUPAMENTO DOS INDIVÍDUOS ##
###################################

disteuc <- dist(x)
disteuc # matriz de distância euclidiana

aa_single <- hclust(disteuc, "single")    ## ligação simples ##
aa_single

d2 <- cophenetic(aa_single)
cor(disteuc,d2)              ## coeficiente de correlação cofenética ##
cor.test(disteuc,d2)


plot(aa_single, xlab = "Parcelas", ylab = "Distância Euclidiana", main = "", 
hang = -1)


aa_cen <- hclust(disteuc, "centroid")        ## centr?ide ##
aa_cen

d2 <- cophenetic(aa_cen)
cor(disteuc,d2)              ## coeficiente de correla??o cofen?tica ##
cor.test(disteuc,d2)

plot(aa_cen, xlab = "Parcelas", ylab = "coeficiente de correlação cofenética", main = "", 
hang = -1, sub="")


require(vegan)

c <- cutree(aa_single, k = 3)     ## classifica os elementos em cada grupo - grupos dos elementos
c  
plot(x, col = c)                  ## disgrama de disperção a cada 2 variáveis, 
                                  ## identificando os grupos ##             


kmeans(x,3)        ## método kmeans ##

#####################
## DADOS BINÁRIOS ##
#####################

dados1 <- read.table("ex_binario.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1? linha ##

dados1

require(fossil)


jac <- ecol.dist(t(dados1), method = jaccard, type = "sim")
jac

jac <- 1-abs(jac)
jac

aa_single <- hclust(jac, "single")    ## ligação simples ##
aa_single

d2 <- cophenetic(aa_single)
cor(jac,d2)              ## coeficiente de correlação cofenética ##
cor.test(jac,d2)

plot(aa_single, xlab = "Fazendas", ylab = "Função de Jaccard", sub="", main = "")

