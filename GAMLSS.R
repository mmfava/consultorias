names(dados1)
attach(dados1)

summary(dados1)

tempo=1:length(dados1$PIE)


# Ajuste  

f01=gamlss(PIE~CF+CT+TT+pH+OD+TU+NO+NO2+NO3+ST,family=PIG)

length(dados1$PIE)

f01=gamlss(PIE~CF+CT+TT+pH+OD+TU+NO+NO2+NO3+ST,family=PIG)
summary(f01) #

# Mostrando os resultados

X11()
plot(tempo,dados1$PIE,type="p",main='Ajuste PIE')
lines(tempo,fitted(f01),lwd=2)

# -----------------------------------------------------------------------------------------
Análise de resíduos

X11()
plot(f01)


