dados <- read.table("ex_corres.txt", header = T)   ## lendo um conjunto de dados em txt ##
dados

attach(dados)

tab <- matrix(freq, ncol = 4, nrow = 3, byrow = F)
tab

colnames(tab) <- c("flo", "pra","pas", "lav")
rownames(tab) <- c("baixo","m�dio","alto")

tab

qui <-chisq.test(tab)     ## teste Qui-Quadrado ##
qui

## AN�LISE DE CORRESPOND�NCIA ##

n <- sum(tab)
n


P <- tab/n        ## matriz de correspond�ncia ##
P

r1 <- sum(tab[1,])/n 
r2 <- sum(tab[2,])/n 
r3 <- sum(tab[3,])/n 

r <- c(r1,r2,r3)       ## vetor coluna ##
r

c1 <- sum(tab[,1])/n 
c2 <- sum(tab[,2])/n 
c3 <- sum(tab[,3])/n 
c4 <- sum(tab[,4])/n 

c <- c(c1,c2,c3,c4)     ## vetor linha ##
c

Dr <- diag(r)
Dr

Dc <- diag(c)
Dc

sDr <- diag((r^(-0.5)))    ## Dr^(-1/2) ##
sDr

sDc <- diag((c^(-0.5)))    ## Dc^(-1/2) ##
sDc

R <- sDr %*% P %*% sDc
R

## COORDENADAS PRINCIPAIS DAS LINHAS ##

W <- t(R) %*% R
W

eigen(W)         ## in�rcias: 2� e 3� autovalores ##

an <- diag((r^(-1))) %*% P %*% diag((c^(-0.5)))
an

score1 <- an %*% eigen(W)$vectors   

score1          ## 2� e 3� colunas s�o as coordenadas principais das  linhas ##


## COORDENADAS PRINCIPAIS DAS COLUNAS ##

T <- R %*% t(R)
T

eigen(T)         ## in�rcias: 2� e 3� autovalores ##

an2 <- diag((c^(-1))) %*% t(P) %*% diag((r^(-0.5)))
an2

score2 <- an2 %*% eigen(T)$vectors   

score2          ## 2� e 3� colunas s�o as coordenadas principais das colunas ##



## PACOTE FactoMineR ##

require(FactoMineR)
corresp <- CA(tab)      ## roda apenas para n� de categorias por vari�vel acima de 2 ##
summary(corresp)



