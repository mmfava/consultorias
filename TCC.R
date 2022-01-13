library(lawstat)
library(ExpDes.pt)
library(plyr)
library(agricolae)

#->    ###  ARQUIVO "cerebro" NA PASTA DADOS P TCC  ####

cerebro=read.csv2(file.choose(),header=T,dec=",",strip.white = TRUE)
attach(cerebro)
names(cerebro)

############ ---------------------------------------------------- ############

 # LPO CÉREBRO #
levene.test(cerebro[,"LPO"], cerebro[,"trat"], location="mean")
dic(trat, LPO, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model=aov(LPO~trat)
model
summary(model)
residuo=resid(model)
shapiro.test(residuo)

# SOD CÉREBRO #

levene.test(cerebro[,"SOD"], cerebro[,"trat"], location="mean")
dic(trat, SOD, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model1=aov(SOD~trat)
model1
summary(model1)
residuo1=resid(model1)
shapiro.test(residuo1)
 
# CAT CÉREBRO #

levene.test(cerebro[,"CAT"], cerebro[,"trat"], location="mean")
dic(trat, CAT, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model2=aov(CAT~trat)
model2
summary(model2)
residuo2=resid(model2)
shapiro.test(residuo2)

 # ACHE CÉREBRO #

levene.test(cerebro[,"ACHE"], cerebro[,"trat"], location="mean")
dic(trat, ACHE, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model3=aov(ACHE~trat)
model3
summary(model3)
residuo3=resid(model3)
shapiro.test(residuo3)


par(mfrow=c(2,2))
cerebro$trat=factor(cerebro$trat)
out=LSD.test(model,"trat", console = TRUE)
bar.err(out$means,variation="SE",bar=TRUE,
        ylab=c("LPO (nmol TBARS.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,6))

out1=LSD.test(model1,"trat", console = TRUE)
bar.err(out1$means,variation="SE",bar=TRUE,
        ylab=c("SOD (nmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,20))

out2=LSD.test(model2,"trat", console = TRUE)
bar.err(out2$means,variation="SE",bar=TRUE,
        ylab=c("CAT (mmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,30))

out3=LSD.test(model3,"trat", console = TRUE)
bar.err(out3$means,variation="SE",bar=TRUE,
        ylab=c("CHE (nmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,80))

#####################################################################################
#->    ###  ARQUIVO "MUSCULO" NA PASTA DADOS P TCC  ####

musculo=read.csv2(file.choose(),header=T,dec=",",strip.white = TRUE)
detach(cerebro)
attach(musculo)

############ ---------------------------------------------------- ############

# LPO MÚSCULO #
levene.test(musculo[,"LPO"], musculo[,"trat"], location="mean")
dic(trat, LPO, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model=aov(LPO~trat)
model
summary(model)
residuo=resid(model)
shapiro.test(residuo)

# SOD MÚSCULO #

levene.test(musculo[,"SOD"], musculo[,"trat"], location="mean")
dic(trat, SOD, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model1=aov(SOD~trat)
model1
summary(model1)
residuo1=resid(model1)
shapiro.test(residuo1)

# CAT MÚSCULO #

levene.test(musculo[,"CAT"], musculo[,"trat"], location="mean")
dic(trat, CAT, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model2=aov(CAT~trat)
model2
summary(model2)
residuo2=resid(model2)
shapiro.test(residuo2)

# ACHE MÚSCULO #

levene.test(musculo[,"ACHE"], musculo[,"trat"], location="mean")
dic(trat, ACHE, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model3=aov(ACHE~trat)
model3
summary(model3)
residuo3=resid(model3)
shapiro.test(residuo3)

par(mfrow=c(2,2))
musculo$trat=factor(musculo$trat)
out=LSD.test(model,"trat", console = TRUE)
bar.err(out$means,variation="SE",bar=TRUE,
        ylab=c("LPO (nmol TBARS.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,5))

out1=LSD.test(model1,"trat", console = TRUE)
bar.err(out1$means,variation="SE",bar=TRUE,
        ylab=c("SOD (nmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,15))

out2=LSD.test(model2,"trat", console = TRUE)
bar.err(out2$means,variation="SE",bar=TRUE,
        ylab=c("CAT (mmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,18))

out3=LSD.test(model3,"trat", console = TRUE)
bar.err(out3$means,variation="SE",bar=TRUE,
        ylab=c("CHE (nmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,80))

dev.off()
#####################################################################################
#->    ###  ARQUIVO "BRANQUIA" NA PASTA DADOS P TCC  ####

branquia=read.csv2(file.choose(),header=T,dec=",",strip.white = TRUE)
detach(cerebro)
attach(branquia)

############ ---------------------------------------------------- ############
library(ExpDes.pt)
# LPO BRANQUIA #
levene.test(branquia[,"LPO"], branquia[,"trat"], location="mean")
dic(trat, LPO, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model=aov(LPO~trat)
model
summary(model)
residuo=resid(model)
shapiro.test(residuo)

# SOD BRANQUIA #

levene.test(branquia[,"SOD"], branquia[,"trat"], location="mean")
dic(trat, SOD, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model1=aov(SOD~trat)
model1
summary(model1)
residuo1=resid(model1)
shapiro.test(residuo1)

# CAT BRANQUIA #

levene.test(branquia[,"CAT"], branquia[,"trat"], location="mean")
dic(trat, CAT, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model2=aov(CAT~trat)
model2
summary(model2)
residuo2=resid(model2)
shapiro.test(residuo2)

# ACHE BRÂNQUIA #

levene.test(branquia[,"AchE"], branquia[,"trat"], location="mean")
dic(trat, AchE, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model3=aov(AchE~trat)
model3
summary(model3)
residuo3=resid(model3)
shapiro.test(residuo3)

par(mfrow=c(2,2))
branquia$trat=factor(branquia$trat)
out=LSD.test(model,"trat", console = TRUE)
out
bar.err(out$means,variation="SE",bar=TRUE,
        ylab=c("LPO (nmol TBARS.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,4))

out1=LSD.test(model1,"trat", console = TRUE)
bar.err(out1$means,variation="SE",bar=TRUE,
        ylab=c("SOD (nmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,8))

out2=LSD.test(model2,"trat", console = TRUE)
bar.err(out2$means,variation="SE",bar=TRUE,
        ylab=c("CAT (mmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,9))

out3=LSD.test(model3,"trat", console = TRUE)
bar.err(out3$means,variation="SE",bar=TRUE,
        ylab=c("ChE (nmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,22))

dev.off()
###################################################################################
#->    ###  ARQUIVO "FIGADO" NA PASTA DADOS P TCC  ####

figado=read.csv2(file.choose(),header=T,dec=",",strip.white = TRUE)
detach(branquia)
attach(figado)

############ ---------------------------------------------------- ############

# LPO FÍGADO #
levene.test(figado[,"LPO"], figado[,"trat"], location="mean")
dic(trat, LPO, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model=aov(LPO~trat)
model
summary(model)
residuo=resid(model)
shapiro.test(residuo)

# SOD FÍGADO #

levene.test(figado[,"SOD"], figado[,"trat"], location="mean")
dic(trat, SOD, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model1=aov(SOD~trat)
model1
summary(model1)
residuo1=resid(model1)
shapiro.test(residuo1)

# CAT FÍGADO #

levene.test(figado[,"CAT"], figado[,"trat"], location="mean")
dic(trat, CAT, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model2=aov(CAT~trat)
model2
summary(model2)
residuo2=resid(model2)
shapiro.test(residuo2)

# ACHE FÍGADO #

levene.test(figado[,"AchE"], figado[,"trat"], location="mean")
dic(trat, AchE, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
model3=aov(AchE~trat)
model3
summary(model3)
residuo3=resid(model3)
shapiro.test(residuo3)


par(mfrow=c(2,2))
figado$trat=factor(figado$trat)
out=LSD.test(model,"trat", console = TRUE)
bar.err(out$means,variation="SE",bar=TRUE,
        ylab=c("LPO (nmol TBARS.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,18))

out1=LSD.test(model1,"trat", console = TRUE)
bar.err(out1$means,variation="SE",bar=TRUE,
        ylab=c("SOD (nmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,13))

out2=LSD.test(model2,"trat", console = TRUE)
bar.err(out2$means,variation="SE",bar=TRUE,
        ylab=c("CAT (mmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,110))

out3=LSD.test(model3,"trat", console = TRUE)
bar.err(out3$means,variation="SE",bar=TRUE,
        ylab=c("ChE (nmol.min-1.mg de proteína-1"), 
        names=c("0mg","1mg","10mg","100mg","1000mg"),
        ylim=c(0,35))

dev.off()
detach(figado)
###################################################################################