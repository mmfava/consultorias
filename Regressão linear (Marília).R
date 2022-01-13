
## Regressão ##
## Marília Melo Favalesso ##

## VAR. QUANTI ~ VAR. QUANTI

# O estudo de regressão aplica-se àquelas situações em que há razões para supor
# uma relação de causa-efeito entre duas variáveis quantitativas e se deseja
# expressar matematicamente essa relação. Ou seja:

# Y ~ X (Y depende de X, ou Y é função de X)

# Em um estudo de regressão, os valores da variável independente (X) geralmen-
# te são escolhidos, e para cada valor escolhido observa-se o valor de y corre
# -spondente. 

# Dessa forma, o objetivo de um estudo de regressão é:

# a) Avaliar uma possível dependência de y em relação a x;
# b) Expressar matematicamente esta relação por meio de uma equação:
#    y = B0 + B1.x

# Para encontrar os coeficientes B0 e B1 é utilizado o método dos minimos
# quadrados.

###
### Realizando uma regressão linear entre duas variáveis quantitativas:
###

## -- Criando vetores para as variáveis -- ##
peso=c(56, 58, 50, 62, 50, 59, 46, 49)
altura=c(1.63, 1.62,1.63, 1.70, 1.67, 1.65, 1.53, 1.64)

## -- Plot e correlação de Pearson -- #

# Todo estudo de regressão deve iniciar pela elaboração de um gráfico de 
# dispersão de pontos e pela aplicação de um teste de associação de Pearson.
# Esse passo é fundamental, pois o gráfico já dá uma boa idéia da existência
# ou não de regressão, e a correlação evita o erro de aplicar a técnica a da-
# dos para os quais não é adequada. 

# Considerando que peso ~ altura:

# a) Plot:

plot(peso~altura)
abline(h=mean(peso),col="blue")
abline(v=mean(altura),col="blue")

# b) Correlação de Pearson

# Normalidade dos dados:
shapiro.test(peso) # P > 0,05 = Normalidade
shapiro.test(altura) # P > 0,05 = Normalidade

# Correlação
cor(peso, altura) # 0,62 (correlação forte)

# Teste T para correlação:
cor.test(peso, altura, alternative=c("greater")) 
## !! Não foi aceita a correlação para as variáveis, mas para dar continuida-
## de no nosso exemplo, iremos colocar a relação como aceita (vamos supor
## que o P-valor foi abaixo de 0,05 e não exatamente 0,05) !!

## Resposta:
## Existe uma associação forte e significativa entre as variáveis Peso (kg)
## e altura (m) (r= 1,92; P < 0,05), sendo adequado, portanto, a realização
## de uma regressão linear entre os dados. 

## -- A reta de regressão linear (LM) -- ##

## Como dito anteriormente, a equação da reta da LM pode ser dada por:

## y = B0 + B1.x

## Onde: 
## y = variável dependente
## B0 = Parâmetro ou coeficiente linear (valor de y quando x = 0)
## B1 = Parâmetro ou coeficiente angular (inclinação da reta, acréscimo ou
## decréscimo em y para cada acréscimo de uma unidade em x)
## x = variável independente

regressao=lm(peso~altura) # Ajusre da regressão
regressao

# Com base nesse modelo ajustados, temos duas informações: O valor do inter-
# cepto (B0-valor em que a reta de regressão intercepta o eixo das ordenadas)
# e o valor que representa o coeficiente de inclinação da reta (B1), ou seja, a
# relação entre o peso e a altura (o quanto o peso varia para cada variação
# unitária de altura). 

# Onde: Intercept = Valor de B0 = - 63,34
#       Altura = Valor de B1 = 71,67

## Logo, podemos concluir que o modelo de regressão ajustado seria 
## Peso(Kg) = -63,34 + 71,67 x Altura (m)
## A linha reta corta o eixo y no valor -63,34, e para cada aumento de uma
## unidade de Altura (m) o Peso sobre cerca ee 71,67 Kg. 

## -- Os pontos experimentais -- ##

# No modelo matemática recém-indicado, a letra y (ou o Peso) representa um 
# valor que é fixo e dependente de um determinado x (altura), isto é, y é
# uma quantidade que não pode variar quando x assume determinado valor. Com 
# dados biológicos, no entanto, é comum verificar-se variação  na variável
# dependente quando ela é medida para um certo valor da variável independen-
# te. Por exemplo, diferentes valores de peso (kg) são observados em indivi-
# duos de um mesmo tamanho. Assim sendo, os pontos obtidos por um experimen-
# to dificilmente se colocam exatamente em uma linha, embora se possa obser-
# var, muitas vezes, que os dados tendem a um alinhamento. Os "desalinhamen-
# tos" são interpretados como desvios, ao acaso, do comportamento geral do
# fenômeno. É por isso está razão que se pensa em ajustar uma linha reta a 
# pontos que não estão perfeitamentes alinhados: A reta vai representar o 
# comportamento médio dos valores de y à medida que x aumenta de valor. O
# modelo matemático proposto, neste caso, é y = B0 + B1.x + E (erro). o E
# representa a diferença entre o valor observado e o esperado, segundo a re-
# ta de y. 

# Na regressão, a linha reta representa o comportamento de valores de y mé-
# dios esperados para distintos valores de x, isto é, a reta representa uma
# média que se modifica à medida que os valores de x aumentam. Em cada uma
# dessas "subpopulações" os valores de y variam ao redor da média deste
# grupo. 

## -- Normalidade dos resíduos -- #

# Uma pressuposição importante para o teste estatístico da re-
# gressão é a de que a variação dos pontos em relaçaõ a média (reta) seja
# a mesma nas várias "subpopulações", ou para cada valor de X.

# Dificilmente testamos a homocedasticidade e a normalidade para cada
# ums dos valores de y em relação a x, sendo substituida esse tipo de 
# teste por uma normalidade dos resíduos. 

# Para testar tal suposição, nos extraimos os valores de resíduos da amostra
# e então aplicamos um teste de normalidade dos resíduos.

## Preditos e resíduos
preditos=predict(regressao) # Valores esperados de y para cada valor de x
                            # ou média dos valores de y para cada x.
residuos=resid(regressao) # É o "E", ou a diferença entre os valores espera-
                          # dos (média y) e o que foi observado (cada valor).

# A seguinte apresentação tabular pode ser usada para resumir as informações
result=data.frame(peso, altura, preditos, residuos)
result

## Normalidade dos resíduos
shapiro.test(residuos) # Os resíduos são normais, logo é possivel a realiza-
                       # ção de testes de significância para os coeficientes
                       # de regressão.

## -- Teste de significância da Regressão -- ##

# A dependência de y em relação a x é representada pelo coeficiente B1. No 
# entando ele quase sempre é determinado com base em uma amostra de dados. 
# Não se trata, portanto, do valor verdadeiro do coeficiente de regressão,
# mas de sua estimativa. No caso do peso~altura, tal coeficiente foi obtido
# com base em uma amostra de oito pessoas. Para se afirmar que o valor B1 = 
# - 63,34 representa uma dependendência real de y ~ x e justificar previsões
# para y com base na equação obtida, deve-se realizar um teste de hipótese 
# sobre a existência de regressão na população. 

# Raciocínio do teste:

# Quando não existe dependência de y em relação a x, o coeficiente de regre-
# ssão populacionar, B1, é igual a zero. A distribuição de B1 em torno de
# zero será gaussiana se a distribuição de y for normal. Para testar a hi-
# pótese de que B1 não é zero, determina-se o número crítico de erros padrão
# permitido para um afastamento observado (b1 - B1), em unidades de erro pa-
# drão (tcalculado). A decisão sobre a significância do desvio é semelhante 
# àquelas vistas nas comparações entre médias e no teste de significância
# de r: se o valor calculadode t exceder o valor crítico, rejeita-se H0
# e conclui-se pela regressão de y em relação a x.

## Teste do coeficiente de regressão
summary(regressao)

## Desenrolando os resultados:
# 1. Estimate Std = Valor dos interceptos β0 e β1
# 2. Error = Erro padrão da média
# 3. T-valor = Valor de significância do Teste T

# ~O Parâmetro no teste T é o valor do intercept
# ~Logo, o parâmetro será -63.34 ± 61,06 (que é o erro). 
# ~Transformando esses valores será 0 ± 2 erros.
# ~Os valores de T transformados ficam 0 ± 2,45 (tabela T crítico)

# !! Calculando o T, que tem o valor de -1,037, vemos que este valor está 
# dentro do intervalo de confiança (que é de -2,45 e + 2,45), portando é 
# aceita a Hipótese nula de teste do modelo (P = 0,34)!!

# O teste é feito para os dois coeficientes?

# Resposta:
## Por meio dessa análise podemos verificar que o coeficinete B1 não foi
## significativo (P>0,05), ou seja, a altura não influência significativa-
## mente o peso.

## Caso a resposta fosse significativa:
## O coeficiente de regressão (B1) não deve ser zero, logo é admitido que 
## existe regressão de peso sobre a altura(P < 0,05). Pode-se então concluir
## que o peso depende da altura da seguinte forma: Para cada acréscimo de 
## 1 unidade de altura, espera-se que o peso aumente 0,72kg. 

## -- Gráfico de regressão linear -- ##

## Para finalizar a análise, caso essa seja verdadeira, é feito um 
## gráfico plot com a linha de regressão e os resíduos.

plot(altura, peso)
abline(regressao)
segments(altura,peso,altura,preditos)

# Dúvidas:

# O modelo é y = B0.x+B1 ou y = -10.06+71.67*x?
-63.34*1.65+71.67 # -32.841 kg?
-63.34+71.67*1.65 # 54.92 kg


## -- Script rapido para aplicação de regressão -- ##

# Banco de dados
peso=c(56, 58, 50, 62, 50, 59, 46, 49)
altura=c(1.63, 1.62,1.63, 1.70, 1.67, 1.65, 1.53, 1.64)

# Correlação de Pearson é significativa?
shapiro.test(peso)
shapiro.test(altura)
cor(peso,altura) # Coeficiente positivo, posso usar o "greater" no teste
cor.test(peso, altura, alternative = "greater")

# O modelo de regressão linear
model=lm(peso~altura) # ~ = em função de
model
## Peso (y)= -63.34+71.67*altura (x)

# Resíduos e preditos
resid=resid(model)
fitted=fitted(model)
result=data.frame(peso, altura, fitted, resid)
result

# Normalidade dos resíduos
shapiro.test(resid)

# Resultado da regressão
summary(model)

# Gráfico
plot(peso~altura)
abline(model)
segments(altura,peso,altura,fitted)

## -- Revisão Prof. Ana -- ##

# - Associação entre duas variáveis
# - Representação matemática

altura=c(1.74, 1.93, 2.41, 5.94, 7.33, 9.27)
diametro=c(10, 12, 14, 23, 40, 59)

# Pressupostos avaliados:

# 1. Correlação entre as variáveis

# 1.a. Normalidade
shapiro.test() # Avaliar normalidade
#--
shapiro.test(altura) # w= 0,87, P=0,23
shapiro.test(diametro) # w= 0,85, P=0,17
#--

# 1.b. Cálculos da associação: plot, r e significância
plot(altura, diametro, xlab="Altura", ylab="Diâmetro") # Plot
abline(lm(diametro~altura)) # Linha de regressão
cor.test(altura, diametro, method="pearson", alternative = "greater") 

## Resposta: Existe uma associação forte e positiva entre as variáveis al-
## tura e diâmetro (t= 6.69, df=4, P < 0,01). 

# 2. Representar matematicamente esta associação.
# 2.a Criando o modelo
m1=lm(diametro~altura)

# 2.b Coeficientes do modelo
m1 # Intercept = -1,49, altura= 5,83
   # Intercepto é o coeficiente linear
   # A altura é o coeficiente angular
   # diametro=b0*altura-1.49

diametro=5.832*altura-1.49

# 2.c Significância do modelo 
summary(m1)

# --
# Estimate Std. Error t value Pr(>|t|)   
# --            Estimative  erro     t      P
# (Intercept)  -1.4876      4.8704  -0.305  0.77527   
# altura        5.8325      0.8712   6.694  0.00259 **
# --
# r² = 92%

## !! obs: Decisões:
##         Se o P valor do coeficiente algular (neste caso, altura) for > 
##         que 0,05, não usar o modelo.
##         Se o P valor do coeficiente linear > 0,05 pode usar o modelo
##         mas b=0, mas o ideal é que ele seja < 0,05 para considerar o 
##         valor do coeficiente b. 

# 3. Validação do modelo
# 3.a Valores de preditos e resíduos
predito=fitted(m1)
residuo=resid(m1)

# 3.b Normalidade dos resíduos
shapiro.test(residuo) # w= 0,88, P=0,25

# 4. Plot dos dados
plot(diametro~altura)
abline(m1)
segments(altura, predito, altura, diametro)

# 5. Considerando o coeficiente de determinação do summary
summary(m1)
## Multiple R-squared = quando eu aceito ambos os coeficiente como 
## significativos (B0 e B1)
## Adjusted R-squared = quando eu aceito somente o coeficiente angular 
## Sendo o resultado de y(diâmetro) apresentado como diâmetro 
## = 5.83*altura+0

## Resposta (lembrando que o coeficiente linear = 0):
## O coeficiente de determinação (r² ajustado = 89%) não deve ser zero, 
## logo é admitido que existe regressão de altura sobre o diâmetro 
## (P < 0,05). Pode-se então concluir que o diametro depende da altura da 
## seguinte forma: Para cada acréscimo de 1m de altura, espera-se que o 
## diametro aumente 5.83cm (diamétro = 5.83*x).

## Outra resposta:
## A função da reta de regressão (y=5.83*x) explica 89% da variação dos 
## dados observados. 



