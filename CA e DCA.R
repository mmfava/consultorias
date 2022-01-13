rm(list=ls(all=TRUE))
#M <- matrix(c(15,25,8,27,37,13,50,12,9,43,8,10),nrow=3)
#dimnames(M)[[1]] <- c("-2000","2000 a 5000","+5000")
#dimnames(M)[[2]] <- c("nfilh=0","nfilh=1","nfilh=2","nfilh>2")
A <-matrix(c(36,4,10,24,15,0,58,9,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,29,67,10,52,6,0,65,0,1,0,3,0,2,0,0,0,9,9,1,1,0,0,5,0,0,28,2,67,20,2,74,2,12,2,1,35,11,4,0,4,2,0,0,0,0,0,0,0), nrow=8)
dimnames(A)[[1]] <- c("MG","Ju","Ped","Arq","Nene","Paz","Paz","Tor")
dimnames(A)[[2]] <- c("Oligochaeta","Ephydridae","Athericidae","Leptophlebiidae",
                      "Megapodagrionidae","Perlidae","Calamoceratidae","Leptoceridae","Odontoceridae")
N <- sum(A)
MP <- A/N
vr <- rep(NA,nrow(A))
vc <- rep(NA,ncol(A))
k <- min(nrow(A)-1,ncol(A)-1)
for(i in 1:nrow(A)){vr[i] <- sum(MP[i,])}
for(i in 1:ncol(A)){vc[i] <- sum(MP[,i])}
Esp <- vr%*%t(vc)
Dr <- diag(vr)
Dc <- diag(vc)
Ptil <- solve(sqrt(Dr))%*%(MP-Esp)%*%solve(sqrt(Dc))
U <- svd(Ptil)$u[,1:k]
V <- svd(Ptil)$v[,1:k]
Lamb <- diag(svd(Ptil)$d[1:k])
H <- sqrt(Dr)%*%U
B <- sqrt(Dc)%*%V
Ycoord <- solve(Dr)%*%H%*%Lamb
Zcoord <- solve(Dc)%*%B%*%Lamb
Inercia <- sum(diag(Lamb)^2)
Quiquadrado <- Inercia*N
x1 <- round(1.1*min(Ycoord[,1],Zcoord[,1],Ycoord[,2],Zcoord[,2]),1)
x2 <- round(1.1*max(Ycoord[,1],Zcoord[,1],Ycoord[,2],Zcoord[,2]),1)
plot(c(x1,x2), c(x1,x2), type="n", xlab="1o. eixo", ylab="2o. eixo", main="1o.
     Plano Fatorial", cex.main=0.8)
lines(c(x1,x2),c(0,0))
lines(c(0,0),c(x1,x2))
points(Ycoord, pch=19, col="red")
text(Ycoord, pos=3, labels = dimnames(A)[[1]],col="red", cex=0.8)
points(Zcoord, pch=15, col="blue")
text(Zcoord, pos=3, labels = dimnames(A)[[2]],col="blue", cex=0.8)

Inercia
Quiquadrado

### DCA ###
library(vegan)

ord<-decorana(A)
ord
plot(ord)
