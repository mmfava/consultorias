# Script R de ANOVA - Análise de Variância
# Marília Melo Favalesso - mariliabioufpr@gmail.com

## Pequeno Glossário:
# --> Fator: Conjunto de níveis de um tratamento experimental em delineamento de ANOVA.
# --> Nível: Valor individual de um tratamento ou fator experimental em uma análise de variância.
# --> Blocos: Área, intervalo de tempo ou indivíduo dentro do qual fatores não manipulados são considerados homogêneos.
# --> Variável resposta: y (quantitativa) --> meu único fator.
# --> Variável preditora/explicativa/independente/fator: x1, x2, x3... (qualitativa).

## Introdução
# A análise da variância (ANOVA) é uma técnica estatística desenvolvida por R.A. Fisher. 
# Ela decompõe a variação total entre os valores obtidos em um experimento em vários componentes identificáveis, atribuindo a variação a diferentes causas ou fontes de variação. 

# Principais tipos de ANOVA:
# 1. ANOVA - fator único
# 2. ANOVA - fator duplo
# 3. ANOVA - fatorial
# 4. ANOVA - para medidas repetidas
# 5. ANOVA aninhada (nested ANOVA)

# Importância dos Pressupostos:
# As amostras devem ser independentes, normalmente distribuídas e com variâncias homogêneas para que a ANOVA seja válida como teste paramétrico.

## ANOVA com um Critério de Classificação (One-Way)
# Um dos modelos mais simples de ANOVA é o delineamento completamente casualizado ou ANOVA de um critério de classificação (One-Way ANOVA). 
# Neste modelo, a variação global é subdividida em duas frações: 
# 1. Variação entre tratamentos
# 2. Variação dentro dos tratamentos

# O teste de comparação entre os efeitos dos tratamentos baseia-se na premissa de que os k tratamentos podem ter médias diferentes, mas a variação entre os indivíduos (S²) é igual em todas as populações comparadas. 

# Hipóteses:
# H0: Média A = Média B = Média C...
# H1: Pelo menos uma das médias é diferente.

# Modelo Linear Generalizado para One-Way ANOVA:
# Yij = µ + ti + erroij
# onde:
# µ é a média geral,
# ti é o efeito do i-ésimo tratamento,
# erroij é o erro aleatório associado à observação ij.

# Com erros normais e independentes:
modelo <- lm(respostaquanti ~ varquali) # Criação do modelo y (quanti) ~ x quali
ANOVA <- aov(modelo) # Aplicação do teste ANOVA para o modelo
summary(ANOVA) # Resultado da ANOVA

# Com estrutura de erros especificada:
modelo1 <- glm(resposta ~ varquali)

# Verificação dos pressupostos:
# Homocedasticidade:
bartlett.test(respostaquanti ~ varquali)
leveneTest(respostaquanti ~ varquali)

# Normalidade dos resíduos:
shapiro.test(resid(modelo))

## Gráficos:
plot(modelo)

# Importante: Todos os fatores qualitativos devem ser criados como fatores, e não vetores. 
# Use as funções factor() ou as.factor() para criar ou converter vetores em fatores.

## Comparação Múltipla entre Médias:
# Após uma ANOVA significativa, utilizamos testes de comparações múltiplas para identificar quais tratamentos diferem entre si.

## Teste de Tukey:
TukeyHSD(ANOVA, "varquali")
plot(TukeyHSD(ANOVA))

## Teste de Dunnett:
# Comparação de um grupo controle com os demais tratamentos.

## ANOVA de Dois Critérios de Classificação (Two-Way ANOVA):
# Utilizada para experimentos com blocos.
# Modelo: Yij = µ + ti + bj + erroij
# onde:
# µ é a média geral,
# ti é o efeito do i-ésimo tratamento,
# bj é o efeito do j-ésimo bloco,
# erroij é o erro aleatório.

modelo2 <- aov(resposta ~ tratam + blocos)
summary(modelo2)

# Resíduos:
residuos <- resid(modelo2)
sum(residuos)
shapiro.test(residuos)
sum(residuos^2)

# Totais e médias dos tratamentos e blocos:
tapply(resposta, tratamento, sum)
tapply(resposta, tratamento, mean)
tapply(resposta, bloco, sum)
tapply(resposta, bloco, mean)

## ANOVA em Esquema Fatorial:
# Utilizada para estudar o efeito de mais de um fator sobre uma variável de interesse.

modelo3 <- aov(resposta ~ fator1 + fator2 + fator1:fator2)
summary(modelo3)

interaction.plot(fator1, fator2, resposta, fixed = TRUE)

## ANOVA com o Pacote ExpDes:
library(ExpDes.pt)
library(car)

# One-Way ANOVA - DIC Fator Único:
abioticos <- read.csv(file.choose(), header = TRUE, sep = ";", dec = ",")
attach(abioticos)
names(abioticos)

# Estatística Descritiva:
summary(Largura..m.[Substrato == "Grosso"])
summary(Largura..m.[Substrato == "Fino"])
summary(Largura..m.[Substrato == "Folhiço"])
boxplot(Largura..m.[Substrato == "Grosso"], Largura..m.[Substrato == "Fino"], Largura..m.[Substrato == "Folhiço"], names = c("Grosso", "Fino", "Folhiço"))
boxplot(Largura..m. ~ Substrato)

# Modelo:
modelolarg <- lm(Largura..m. ~ Substrato)

# Pressupostos:
bartlett.test(Largura..m. ~ Substrato)
leveneTest(Largura..m. ~ Substrato)
shapiro.test(resid(modelolarg))

# ANOVA - DIC 1 Fator - Pacote ExpDes:
dic(Substrato, Largura..m., quali = TRUE, sigF = 0.05, mcomp = "tukey", sigT = 0.05)

## ANOVA em Esquema Fatorial Duplo:
data(ex4)
attach(ex4)
names(ex4)

# Pressupostos:
leveneTest(zn ~ esterco)
leveneTest(zn ~ as.factor(revol))
shapiro.test(resid(lm(zn ~ esterco)))
shapiro.test(resid(lm(zn ~ revol)))

# ANOVA 2 Fatores:
fat2.dic(revol, esterco, zn, quali = c(TRUE, TRUE), mcomp = "tukey", fac.names = c("Revolvimento", "Esterco"), sigT = 0.05, sigF = 0.05)
interaction.plot(revol, esterco, zn)

## ANOVA com Pacote ExpDes para DBC:
data(ex5)
attach(ex5)
names(ex5)

# Pressupostos:
leveneTest(sabor ~ trat)
leveneTest(sabor ~ genero)
shapiro.test(resid(lm(sabor ~ trat)))
shapiro.test(resid(lm(sabor ~ genero)))

# ANOVA DBC Fator Duplo:
fat2.dbc(trat, genero, bloco, sabor, quali = c(TRUE, TRUE), mcomp = "lsd", fac.names = c("Amostras", "Gênero"))

# Questões Práticas:
# Questão 71:
A <- c(1, 2)
B <- c(5, 4, 5)
C <- c(3, 0, 2)
D <- c(3, 3, 2, 2)

dado <- c(1, 2, 5, 4, 5, 3, 0, 2, 3, 3, 2, 2)
ponto <- c("A", "A", "B", "B", "B", "C", "C", "C", "D", "D", "D", "D")

dano <- data.frame(ponto, dado)
bartlett.test(dado ~ ponto)
shapiro.test(resid(lm(dado ~ ponto)))

modelo <- lm(dado ~ ponto)
Anova <- aov(modelo)
summary(Anova)
TukeyHSD(Anova)
plot(TukeyHSD(Anova))
boxplot(dado ~ ponto)

# Questão 72:
trat <- c("dieta 1", "dieta 1", "dieta 1", "dieta 1", "dieta 1", "dieta 2", "dieta 2", "dieta 2", "dieta 2", "dieta 2", "dieta 3", "dieta 3", "dieta 3", "dieta 3", "dieta 3")
faixa <- c("I", "II", "III", "IV", "V", "I", "II", "III", "IV", "V", "I", "II", "III", "IV", "V")
peso <- c(15, 17, 20, 24, 19, 10, 8, 12, 16, 18, 12, 16, 16, 15, 22)
dieta <- data.frame(trat, faixa, peso)

dbc(trat, faixa, peso, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
boxplot(peso ~ trat)

# Questão 75:
gestante <- c("1", "2", "3", "4", "5", "6", "7", "1", "2", "3", "4", "5", "6", "7", "1", "2", "3", "4", "5", "6", "7", "1", "2", "3", "4", "5", "6", "7")
tempo <- c(275, 292, 281, 284, 285, 283, 290, 273, 283, 274, 275, 294, 279, 265, 273, 285, 270, 272, 278, 276, 292, 244, 329, 252, 258, 275, 279, 295)
trat <- c("UM", "UM", "UM", "UM", "UM", "UM", "UM", "EV", "EV", "EV", "EV", "EV", "EV", "EV", "US", "US", "US", "US", "US", "US", "US", "DO", "DO", "DO", "DO", "DO", "DO", "DO")

data <- data.frame(gestante, trat, tempo)
bartlett.test(tempo ~ trat)
dbc(trat, gestante, tempo, quali = TRUE, mcomp = "lsd", sigT = 0.05, sigF = 0.05)
boxplot(tempo ~ trat)
