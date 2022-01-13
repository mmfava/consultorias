x <- read.table("manova_expfatoria.txt", header = T)   ## lendo um conjunto de dados em txt ##
x


attach(x) 
names(x)

var <- as.matrix(x[,3:5]) # separa somente as var. resposta
var
cor(var) # matriz varcovar


## MANOVA ##
## test = c("Pillai" (defualt), "Wilks", "Hotelling-Lawley", "Roy") ##

fit <- manova(var ~ razao + adi + razao:adi)
fit

summary.aov(fit)
summary.manova(fit)
summary.manova(fit, test = "Wilks")
shapiro.test(resid(fit))

# pelo menos um dos níveis de cada um dos fatores exerceu influência sobre as var. respostas

