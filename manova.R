x <- read.table("turfa.txt", header = T)   ## lendo um conjunto de dados em txt ##
x

attach(x)
names(x)

par(mfrow = c(1,1))
boxplot(n ~ trat, names = c("Testemunha", "Fermentada", "Natural"), ylab = "Teor de N")
boxplot(p ~ trat, names = c("Testemunha", "Fermentada", "Natural"), ylab = "Teor de P")

## ANOVA ##

trat <- as.factor(trat)
anvn <- aov(n  ~ trat) # anova só para n ~ trat
anvp <- aov(p ~ trat) # anova só para p ~ trat
summary(anvn)  # P < 0.05
summary(anvp)  # P < 0.05

## MANOVA ##
## test = c("Pillai" (defualt), "Wilks", "Hotelling-Lawley", "Roy") ##

var <- as.matrix(x[,2:3]) # pegou as colunas 2 e 3 da matrix "x" - ou seja, apenas as var. resp.
var
fit<- manova(var ~ trat) # faz a manova
trat

summary.aov(fit) # deu o resultado para cada var resposta

summary.manova(fit) # deu resultado do tratam considerando n e p
summary.manova(fit, test = "Wilks") # com teste lamba wilks
