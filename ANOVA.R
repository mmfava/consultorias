##
## ANOVA (Análise de Variância)
## Marília Melo Favalesso - mariliabioufpr@gmail.com
## ------------------------------------------------------
## A análise de variância é um teste de hipóteses para modelos lineares.
##
## Em um modelo, construímos uma expressão na qual elementos (variáveis
## e constantes) determinam as causas de variação de uma variável.
##
## PA <-- sexo + idade + etnia + sedentarismo + elasticidade dos vasos + ignorados
##
## A hipótese nula em um modelo de análise de variância é dada por:
## H0: "A média de cada variável tratamento é igual às demais".
##
## A hipótese alternativa será a seguinte:
## Ha: "Pelo menos uma das médias é diferente das demais".
##
## Assim, a ANOVA compara a variação devida aos tratamentos com
## a variação devida ao resíduo, por isso resulta em um teste
## F-Snedecor, semelhante a comparação de variâncias. O teste
## ainda requer que as variáveis dos tratamentos tenham distribuição
## normal e que as variâncias sejam homogêneas.
##
## ANOVA
## -----------------------------------------------------
## F.V.          G.L.   S.Q.      Q.M          F.
## -----------------------------------------------------
## Tratamentos    k-1   SQt   QMt=SQt/(k-1)   QMt/QMd
## Desconhecido   n-k   SQd   QMd=SQd/(n-k)
## -----------------------------------------------------
## Total          n-1
## ------------------------------------------------------

##########################################################################################################

## EXEMPLO

## Entrada de dados
## --------------------------------------------------------
exercicio = data.frame(
  treatment = rep(1:3, each = 6),
  response = c(14, 14, 13, 12, 15, 13, 24, 25, 19, 22, 18, 22,
               20, 22, 22, 16, 15, 16))

## Escrevendo o modelo
exercicio.model = lm(formula = response ~ treatment, 
                     data = exercicio)

exercicio.model

## Visualização gráfica dos tratamentos
boxplot(formula = response ~ treatment, data = exercicio)

## Validação do modelo
##
## A especificação do modelo estabelece que os resíduos
## (erro ou termo desconhecido) tenham variâncias 
## constantes segundo os tratamentos e distribuição 
## normal. A validação do modelo é essencial para se 
## proceder à análise.
##
## Com o modelo ajustado (coeficientes) pode-se realizar
## predições. Assim, os valores preditos pelo modelo são dados por:

preditos = fitted(object = exercicio.model)
preditos

## Os resíduos (erros) são obtidos pela diferença entre
## o valor observado e o valor predito (estimado) pelo modelo:

residuos = resid(object = exercicio.model)
residuos

## É importante fazer um gráfico que ilustre a dispersão 
## entre os valores reais (observados) e os valores preditos 
## (estimados). Em uma situação ideal, os pontos devem ser
## uniformemente distribuídos ao redor da linha de média zero,
## o que sugere homogeneidade das variâncias:

plot(x = preditos, y = residuos,
     pch = 20,
     col = "black")

abline(h = mean(residuos), col = "azure4")
abline(v = mean(preditos))

## A ausência de padrões sugere homogeneidade das variâncias.

## Teste de homogeneidade de variâncias entre grupos
## (Teste de Levene)

library(car)

## O teste de Levene testa se todas as variâncias são
## homogêneas.

## Ho: Todas as variâncias são iguais.
## Ha: Pelo menos uma das variâncias diverge das demais.

## Se o p-valor for menor do que 0.05 (5%), o teste
## é significativo (rejeita-se Ho).

leveneTest(residuos,
           group = as.factor(exercicio$treatment))

## Como p-valor = 0.02938 foi menor do que 0.05, o teste
## foi significativo para a heterogeneidade (rejeita-se
## Ho).

## Teste para a normalidade dos resíduos
shapiro.test(residuos)

## Como o p-valor foi maior do que 0.05, os resíduos
## têm distribuição normal. Logo, a ANOVA pode ser
## empregada, caso o primeiro pressuposto de 
## homogeneidade das variâncias seja aceito!!!

## Então, quais são os critérios a serem avaliados?
## - Homogeneidade da variância dos resíduos (Teste de Levene).
## - Distribuição normal dos resíduos (Shapiro-Wilk).

## No nosso caso, o que podemos concluir?

## Mas caso fosse possível, como faríamos a ANOVA?

anova(exercicio.model)
