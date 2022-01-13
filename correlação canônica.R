## fazendo a correlação canonica a mão 
cor
r11=cor[5:7, 5:7]
r11

r22=cor[1:4,1:4]
r22

r12=cor[5:7,1:4]
r12

# calculando os autovalores e autovetores
A <- (solve(r11)) %*% r12 %*% (solve(r22)) %*% (t(r12))
A

B <- (solve(r22)) %*% (t(r12)) %*% (solve(r11)) %*% r12
B

eigen1 <- eigen(A)
eigen2 <- eigen(B)
eigen1
eigen2

# Autovalores
sqrt(eigen1$values) ## autovalores (valor de correlação canônica)
sqrt(eigen2$values) ## autovalores (valor de correlação canônica)

eigen2$vectors
eigen1$vectors