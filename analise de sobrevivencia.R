# Script R para Análise de Sobrevivência
# Marília Melo Favalesso - mariliabioufpr@gmail.com

# Carregar dados
dados_sobrevivencia <- read.table("risco.csv", header = TRUE, sep = ";")
attach(dados_sobrevivencia)

# Construção de um diagrama de dispersão de tempo em função da idade do fragmento
plot(tempo.seg ~ frag, main = "Diagrama de Dispersão: Tempo vs Idade do Fragmento", 
     xlab = "Idade do Fragmento", ylab = "Tempo (segundos)", pch = 19, col = "blue")

# Carregar o pacote survival
library(survival)

# Construção do modelo de sobrevivência usando distribuição exponencial
modelo_exp <- survreg(Surv(tempo.seg, ataque) ~ frag, dist = "exponential")
summary(modelo_exp)

# Plot do modelo de sobrevivência
par(mar = c(5, 6, 4, 2))
plot(survfit(Surv(tempo.seg, ataque) ~ frag), ylab = "Probabilidade de Sobrevivência", 
     cex.axis = 1.2, pch = 16, cex.lab = 1.2, xlab = "Tempo (segundos)", 
     lty = 1:7, lwd = 4, col = c("black", "grey", "antiquewhite2", 
                                 "antiquewhite3", "antiquewhite4", 
                                 "green3", "forestgreen"), bty = "l", 
     font.lab = 2, main = "Curva de Sobrevivência")
legend("topright", legend = levels(frag), lty = 1:7, lwd = 4, 
       col = c("black", "grey", "antiquewhite2", "antiquewhite3", 
               "antiquewhite4", "green3", "forestgreen"), 
       cex = 1.2, bty = "n")

# Modelo de sobrevivência sem especificação de distribuição
modelo_default <- survreg(Surv(tempo.seg, ataque) ~ frag)
summary(modelo_default)

# Comparação dos modelos usando AIC
AIC(modelo_exp)
AIC(modelo_default)

# Modelo de sobrevivência usando distribuição lognormal
modelo_lognormal <- survreg(Surv(tempo.seg, ataque) ~ frag, dist = "lognormal")
summary(modelo_lognormal)
AIC(modelo_lognormal)

# Análise dos resíduos e predições do modelo lognormal
residuos_lognormal <- resid(object = modelo_lognormal)
predicoes_lognormal <- predict(modelo_lognormal)

# Visualização dos resíduos e predições
residuos_lognormal
predicoes_lognormal
