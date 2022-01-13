
setwd("C:/Users/Ana/Dropbox/Predation risk")
setwd("~/Dropbox/Predation risk")
dados = read.table("risco2.csv",h=T,sep=";")

attach(dados)

## Constru??o de um diagrama de dispers?o de tempo em fun??o da idade do fragmento
plot(tempo.seg~frag)

## Constru??o do modelo de sobreviv?ncia
library(survival)

m1=survreg(Surv(tempo.seg,ataque)~frag,dist="exponential")
m1

summary(m1)
par(mar=c(5,6,4,2))
plot(survfit(Surv(tempo.seg,ataque)~frag),ylab="",cex.axis=1.2,,pch=16,cex.lab=1.2,xlab="",lty=c(1:7),lwd=4,col=c("black","grey","antiquewhite2", "antiquewhite3", "antiquewhite4", "green3","forestgreen"),bty="l",font.lab=2)
legend("topright",c("0","","","","","",""),lty=1:7,lwd=4,bty="n",col=c("black","grey","antiquewhite2", "antiquewhite3", "antiquewhite4", "green3","forestgreen"),cex=1.2)

m2=survreg(Surv(tempo.seg,ataque)~frag)
m2

AIC(m1)
AIC(m2)

summary(m2)

m3=survreg(Surv(tempo.seg,ataque)~frag,dist="lognormal")
m3

a = frag
a

levels(a)
levels(a)[1:2]<-"0e6"
levels(um2)

anova(m3)
summary(m3)
AIC(m3)
residuo=resid(object=m3)
residuo
predito1=predict(m3)
predito1