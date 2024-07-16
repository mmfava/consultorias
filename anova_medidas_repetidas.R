# Marília Melo Favalesso - mariliabioufpr@gmail.com
# Script para análise estatística de um conjunto de dados de atividade

# Carregando o arquivo de dados (substitua file.choose() pelo caminho do arquivo, se necessário)
atividade <- read.csv2(file.choose(), h = TRUE, dec = ",", sep = ";")

# Carregando pacotes necessários
require(MASS)        # Para o conjunto de dados 'oats'
require(nlme)        # Para lme()
require(multcomp)    # Para testes de comparação múltipla

# Análise de Variância (ANOVA) para medidas repetidas
AVD.aov <- aov(AVD ~ GRUPO + TEMPO + Error(ID/GRUPO), data = atividade)

# Modelo linear misto usando 'lme' para considerar efeitos aleatórios de ID dentro de GRUPO
Lme.AVD <- lme(AVD ~ GRUPO + TEMPO, random = ~1 | ID/GRUPO, data = atividade)

# Testando pressupostos estatísticos de normalidade e homocedasticidade dos resíduos
# Se os dados estiverem em normalidade e homocedasticidade, continuamos com a ANOVA para medidas repetidas e o teste post-hoc
# Se apenas a normalidade for atendida, continuamos com a ANOVA para medidas repetidas e o teste post-hoc
# Se os dados não atenderem aos pressupostos, realizamos o teste de Friedman

attach(atividade)  # Anexando dados para facilitar referências

residuos <- resid(Lme.AVD)  # Obtendo os resíduos do modelo linear misto
shapiro.test(residuos)      # Teste de Shapiro-Wilk para normalidade dos resíduos

# Teste de homocedasticidade usando teste de Bartlett para diferentes níveis de TEMPO e GRUPO
bartlett.test(AVD[TEMPO == "T0"], GRUPO[TEMPO == "T0"])
bartlett.test(AVD[TEMPO == "T1"], GRUPO[TEMPO == "T1"])
bartlett.test(AVD[TEMPO == "T2"], GRUPO[TEMPO == "T2"])

# Sumário da ANOVA para medidas repetidas
summary(AVD.aov)

# Análise de variância do modelo linear misto
anova(Lme.AVD)

# Sumário do modelo linear misto
summary(Lme.AVD)

# Teste de comparações múltiplas usando correção de Tukey
summary(glht(Lme.AVD, linfct = mcp(TEMPO = "Tukey")))

# Gráfico de interação entre TEMPO e GRUPO para variável AVD
interaction.plot(TEMPO, GRUPO, AVD)
