# Carregando a biblioteca 'circular' para manipulação de dados circulares
library(circular)

# Gerando uma amostra de dados de distribuição Von Mises
# com média (mu) de 0 e concentração (kappa) de 1
x <- rvonmises(n = 100, mu = circular(0), kappa = 1)

# Calculando a dispersão angular (angular deviation) dos dados gerados
angular.deviation(x)

# Gerando duas amostras combinadas de distribuição Von Mises:
# 50 pontos com média de 0 e kappa de 1, e 100 pontos com média de pi/3 e kappa de 10
x <- c(rvonmises(50, circular(0), 1), rvonmises(100, circular(pi/3), 10))

# Criando um vetor indicando os grupos (0 e 1) correspondentes às amostras
group <- c(rep(0, 50), rep(1, 100))

# Realizando ANOVA circular para comparar grupos usando F-test
aov.circular(x, group)

# Realizando ANOVA circular para comparar grupos usando likelihood ratio test (LRT)
aov.circular(x, group, method = "LRT")

# Gerando um gráfico de distribuição Von Mises com 10 pontos
plot(rvonmises(10, circular(0), kappa = 1))

# Adicionando setas radiais ao gráfico circular
arrows.circular(rvonmises(10, circular(0), kappa = 1))

# Adicionando setas radiais com cores diferentes e alturas aleatórias
arrows.circular(rvonmises(10, circular(0), kappa = 1), y = runif(10), col = 2)

# Adicionando setas radiais com cores e coordenadas x0/y0 aleatórias
arrows.circular(rvonmises(10, circular(0), kappa = 1), y = runif(10),
                x0 = runif(10, -1, 1), y0 = runif(10, -1, 1), col = 3)

# Carregando um conjunto de dados a partir de um arquivo CSV selecionado pelo usuário
data <- read.csv2(file.choose(), header = TRUE, dec = ",")

# Anexando os dados para facilitar o acesso às variáveis
attach(data)

# Criando uma tabela de frequência para a variável 'botão'
# onde a espécie é 'Cordia trichotoma (Vell.) Arráb. ex Steud.'
# e o mês é 'ago/14'
table(botão[espécie == "Cordia trichotoma (Vell.) Arráb. ex Steud."])[mês == "ago/14"]
