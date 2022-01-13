
# abrindo o banco de dados
dados <- read.table("acp_alface.txt", header = T)   
dados

# selecionando apenas as var preditoras
dados1 <- dados[,2:7]
dados1 # não pode haver var quali, logo tiro o nome dos indivíduos
row.names(dados1) = dados[,1]
dados1
attach(dados1)
names(dados1)
is.data.frame(dados1)
dim(dados1) # dimensões do objeto
colMeans(dados1) # média das var coluna
S <- cov(dados1,dados1)
S # covariâncias
R <- cor(dados1,dados1)
R # correlações
cor.test(HCO3,SO4) # cor teste entre as var. 

## TESTE DE ESFERICIDADE DE BARTLETT

n <- nrow(dados1)
n
p <- ncol(dados1)
p
chi2 <- -(n-1-((2*p+5)/6))*log(det(R))
ddl <- p*(p-1)/2
print(chi2)
print(ddl)
print(pchisq(chi2,ddl,lower.tail=F))
require(psych)
cortest.bartlett(R,n)  # Teste de esfericidade de bartlett entre a matriz de correlação (R) e 
                       # o tamanho da amostra (n). 
                       # Resultado: X² = 183,7, P < 0,001 --> Existe c

# índice KMO pelo pacote psych
require(psych) 
KMO(R)  # quanto mais próximo a 1 melhor é a aplicação - eu olho o overall MSA = 0,32

## COMANDO ESPECÍFICO DE COMPONENTES PRINCIPAIS ##

## USANDO A MATRIZ S ##

# auto-valores das matrizes
eigen(S) # Auto valores e autovetores - fazendo o componente principal manualmente.
eigen(R)
cp <- prcomp(dados1) # cria os componentes principais usando S
cp
cp <- prcomp(dados1, scale = T) # cria os componentes principais usando R de Pearson
cp 
# eu olho as variáveis que têm maior peso na PC - ou maior associação com o componente.

# escolha do componente
summary(cp) # desvio padrão, proporção e proporção acumulada
# olho a proporção da variância para escolha do componente

# screeplot utilizado na escolha dos componentes
screeplot(cp)
screeplot(cp, type = "lines")     ## gr?fico de cotovelo ##

# criando o gráfico de componente principal
names(cp)

cp$sdev               ## desvio padrão dos CP's: raíz quadrada autovalores ##
cp$rotation           ## coeficientes cada componente principal: autovetores ##
cp$center             ## coordenada central: média amostral ##

cp$rotation[,1]       ## coeficientes do 1º CP ##

score <- t(cp$rotation[,1]) %*% t(dados1)
score                  ## score para cada indivíduo no CP1 ##

cp$x                  ## scores com variáveis centradas ##

cbind(1:19,as.vector(score))

par(mfrow = c(1,2))
plot(1:19, as.vector(score))
plot(1:19, cp$x[,1])


## no comando biplot podemos adicionar o subcomando 
## choices = c(1,3), no qual escolhe quais cps quer plotar ##

biplot(cp)            ## gr?fico biplot ##

cor(as.vector(score), dados1)


require(graphics)

## USANDO A MATRIZ R ##

cp2 <- princomp(dados1, cor = T)   ## usando a matriz de correla??o ##
cp2

summary(cp2)

screeplot(cp2, type = "lines")

names(cp2)
cp2$loa                  ## coeficientes de cada componente principal ##
cp2$score                ## scores com vari?veis centradas ##

cor(cp2$score[,1], HCO3)

par(mfrow = c(1,2))
plot(cp2$sco[,1], cp2$sco[,2])
biplot(cp2)  

