# Script R para ANOVA Fatorial Duplo
# Marília Melo Favalesso - mariliabioufpr@gmail.com

# Carregamento dos dados e pacotes necessários:
dados <- read.csv2(file.choose(), header = TRUE, dec = ".", sep = ";")
attach(dados)
library(ExpDes.pt)
library(plyr)
library(ggplot2)
names(dados)

# ANOVA Fatorial Duplo:
# Este script realiza a análise de variância (ANOVA) fatorial com dois fatores qualitativos (Grupos1 e aplicaçao1).
# Além disso, são gerados gráficos para visualização dos resultados.

# Função fat2.dic do pacote ExpDes.pt:
fat2.dic(Grupos1, aplicaçao1, g.por.dia1, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("Grupos", "Aplicação"),
         sigT = 0.05, sigF = 0.05)

# Gráfico de interação:
interaction.plot(Grupos1, aplicaçao1, g.por.dia1)

# Cálculo da média e desvio padrão para cada combinação de fatores:
aplic1 <- data.frame(ddply(dados, ~Grupos1 * aplicaçao1, summarise, mean = mean(g.por.dia1), sd = sd(g.por.dia1)))
aplic1

# Preparação dos dados para o gráfico:
pd <- position_dodge(0.3)
dados$Grupos1 <- factor(dados$Grupos1)
dados$aplicaçao1 <- factor(dados$aplicaçao1)

# Gráfico de barras com erro padrão:
ggplot(aplic1, aes(x = Grupos1, y = mean, colour = aplicaçao1, group = aplicaçao1)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, size = 0.25, colour = "black", position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 1) +
  ylab("Ganho de peso por gramas/dia") +
  labs(title = "Interação entre Grupos e Aplicações",
       x = "Grupos",
       y = "Ganho de peso por gramas/dia",
       colour = "Aplicação")
