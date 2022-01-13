\documentclass[a4paper,12pt]{article}
\usepackage[brazil,brazilian]{babel}
\usepackage{ae,indentfirst,natbib,graphicx,setspace,url,longtable,float,subfigure}
\usepackage[lmargin=2.5cm, rmargin=2.5cm, tmargin=2.5cm, bmargin=2.5cm]{geometry} 
\usepackage[bf]{caption} 

\usepackage[utf8]{inputenc}
%\usepackage{tabularx}
%opening
\date{}
%opening
\title{Análise do efeito do Risco de predação sobre grilos em florestas em regeneração}
\author{Neucir Szinwelski \& Carlos Sperber}
\date{11 de maio de 2013}

\begin{document}
\SweaveOpts{concordance=TRUE}
\maketitle

<<>>=

setwd("~/Dropbox/Predation risk")

dados<-read.table("risco3.csv",h=T)

attach(dados)

library(survival)

library(mgcv)

names(dados)

str(dados)

summary(dados)

@

<<fig=T,label=1>>=

plot(tempo.seg)

@

<<fig=T,label=2>>=

plot(tempo.seg~idade)

@

<<fig=T,label=3>>=

tiff("KarinArtigo.tiff", width = 10, height = 6, units = 'in', res = 300, compression = 'lzw')

par(mar=c(5,6,4,2))

plot(survfit(Surv(tempo.seg,ataque)~idade2),lty=c(1:7),ylab="Sobrevivência (%)",xlab="Tempo de ataque a isca (seg)",cex=2,cex.lab=1.2,bty="l",font.lab=2)

legend(110,1,c("0","6","15","35","70","130","300"),lty=1:7,bty="n",cex=1)

dev.off()

@


<<>>=



@
<<>>=

tapply(tempo.seg[ataque==1],idade[ataque==1],mean)

tapply(tempo.seg[ataque==1],idade[ataque==1],var)

m1<-survreg(Surv(tempo.seg,ataque)~idade,dist="exponential")

summary(m1)

m1n<-survreg(Surv(tempo.seg,ataque)~idade)

anova(m1,m1n,test="Chi")

summary(m1n)
@

Análise e AMALGAMAMENTO para confirmar

<<>>=

mod1<-survreg(Surv(tempo.seg,ataque)~idade2,dist="exponential")

mod2<-survreg(Surv(tempo.seg,ataque)~idade2)

anova(mod1,mod2,test="Chi")

source("coms.R")

@


Como o parâmetro de escalonamento é menor do que 1 (Scale= 0.508), isto indica que o risco de ataque diminui com o tempo.

<<>>=

m2<-glm(tempo.seg~idade)

m2n<-glm(tempo.seg~1)

@

<<fig=T,label=4>>=

par(mfrow=c(2,2))

plot(m2)

par(mfrow=c(1,1))

@

<<>>=

m3<-glm(tempo.seg~idade+I(idade^2))

m4<-glm(tempo.seg~idade+I(idade^2)+I(idade^3))

summary(m4)

anova(m2,m3,test="F")

anova(m3,m4,test="F")

m5<-glm(tempo.seg~idade+I(idade^2)+I(idade^3)+I(idade^4))

anova(m4,m5,test="F")

AIC(m2,m3,m4,m5)

@

<<fig=TRUE,label=5>>=

par(mfrow=c(2,2))

plot(m2)

par(mfrow=c(1,1))

@

<<fig=TRUE,label=6>>=

par(mfrow=c(2,2))

plot(m3)

par(mfrow=c(1,1))

@

<<fig=TRUE,label=7>>=

par(mfrow=c(2,2))

plot(m4)

par(mfrow=c(1,1))

@

<<fig=TRUE,label=8>>=

par(mfrow=c(2,2))

plot(m5)

par(mfrow=c(1,1))

@

<<>>=

m.gam<-gam(tempo.seg~s(idade,bs="cs",k=4))

anova(m.gam)

@

<<fig=TRUE,label=9>>=

plot(m.gam)

@

FAZENDO O GRÁFICO COM OS PONTOS.

<<>>=

m.gam<-gam(tempo.seg~s(idade,bs="cs",k=4))

@


<<fig=TRUE,label=10>>=

plot(tempo.seg~idade,pch=16,xlab="Regeneration time (years)",ylab="Time to bait contact",bty="l")

text(280,20,"gam",pos=2)

xv<-0:300

yv<-predict(m.gam,list(idade=xv))

lines(xv,yv)
@

Resumo da Geanne para o Simpósio de Entomologia da UFV.

<<fig=TRUE,label=11,eps=T,jpeg=TRUE>>=

plot(tempo.seg~idade,pch=16,xlab="Tempo de regeneração (anos)",ylab="Tempo para o contato com a isca (seg)",bty="l")




xv<-0:300

yv<-predict(m.gam,list(idade=xv))

lines(xv,yv)
@

\end{document}
