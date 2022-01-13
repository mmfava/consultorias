## MODELO ADITIVO GENERALIZADO (GAM): 

m.gam=gam(tempo.seg~s(frag,k=3))
m.gam
summary(m.gam)
predito2=predict(m.gam)
predito2

plot(predito2,predito1,ylab="sobrevivência",xlab="tempo de ataque")
xv=0:300
yv=predict(glm2,list(predito2=xv))
lines(xv,yv)

attach(gam)
plot(frag,tempo.seg,ylab="sobrevivência",xlab="tempo de ataque")


glm1=glm(log2~log1)
anova(glm1)
summary(glm1)

log1=log(predito1)
log2=log(predito2)

plot(log2,log1)
curve((0.68369+0.84076*x),add=T)

library(mgcv)

tempo=tapply(tempo.seg[ataque==1],frag[ataque==1],mean)
sob=tapply(ataque,frag,mean)

plot(frag,tempo.seg)
