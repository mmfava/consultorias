## instalar pacotes mvnormtest, mvsf e nortest, a partir da vers?o 2.13

require(mvnormtest)
require(mvsf)

## exemplo trabalhado em multivariada com voc?s ##
dados <- read.table("ex_solo.txt", header = T)   ## lendo um conjunto de dados em txt ##
dados


attach(dados)

par(mfrow = c(3,2))
hist(ca)
hist(mg)
hist(sb)

boxplot(ca)
boxplot(mg)
boxplot(sb)

## TESTE DE SHAPIRO-WILKS UNIVARIADO ##

shapiro.test(ca)
shapiro.test(mg)
shapiro.test(sb)

## GR?FICO QQ-PLOT CASO UNIVARIADO ##

qqnorm(ca, main = "QQ-plot para Ca")
qqline(ca, col = "blue")           ## qq-plot

qqnorm(mg, main = "QQ-plot para Mg")
qqline(mg, col = "blue")           ## qq-plot

qqnorm(sb, main = "QQ-plot para SB")
qqline(sb, col = "blue")           ## qq-plot

## TESTE DE ANDERSON DARLING E KOLMOGOROV SMIRNOV ##

require(mvsf)

ad.test(ca)       ## teste de Anderson-Darling ##
ad.test(mg)
ad.test(sb)

lillie.test(ca)   ## teste de Kolmogorov-Smirnov ##
lillie.test(mg)
lillie.test(sb)


## TESTE DE SHAPIRO WILKS MULTIVARIDO ##
## cada elemento amostral deve ser uma coluna ##

c <- as.matrix(dados)

mshapiro.test(t(c))

## TESTE DE NORMALIDADE MULTIVARIADO DE SHAPIRO-FRANCIA ##

mvsf(t(c))                    ## teste de Shapiro-Francia

## GR?FICO QQ-PLOT MULTIVARIADO ##

x <- as.matrix(dados) # Para rodar dados multivariados no R é necessário que estes
x                     # estejam em formato de matriz

S=var(x) # Faz matriz de variância e covariância
S

m=apply(x,2,mean)   ## indica que ? p/ fazer os c?lculos para cada coluna ##
m

invS=solve(S)
invS

d1_2 <- t(x[5,]-m) %*% invS %*% (x[5,]-m)
d1_2

(dim(x)[1]-1+0.5)/dim(x)[1]
1-((1-0.5)/dim(x)[1])

d = q = NULL

n= nrow(x)    ## n? de indiv?duos ##
p= ncol(x)    ## n? de vari?veis ##

for (i in 1:n){
  d = c(d, t(x[i,]-m) %*% invS %*% (x[i,]-m))
  prob = (i-0.5)/n
  q = c(q, qchisq(prob, df=p))
}

d=sort(d)

d
q

plot(d,q, xlab = "Dist?ncias Ordenadas - di^2", ylab = "Quantil Qui-Quadrado")



## Exercicio aula - Teste T² de Hotteling

#	Supondo que X = [X1 , X2]t tem distribuição normal bivariada, calcular a 
# estatística T2 para testar H0:  = [7,11]t versus H1:  ≠  [7,11]t,  usando os dado
# s. Especificar a distribuição de T2 para o caso em estudo e testar a hipótese 
# citada ao nível de significância  = 0,05

exemplo=matrix(c(2,12,8,9,6,9,8,10),  byrow=2, nrow=4) 
colnames(exemplo)=c("X","Y")
exemplo

norm=mshapiro.test(t(exemplo)) # normalidade dos dados
norm

varcovar=var(exemplo) # Matriz de variância covariância
varcovar

medias=colMeans(exemplo)
medias

s=cov(exemplo,exemplo)
s

mu0=c(7,11)

n=4

T2=n*t(medias-mu0)%*%solve(s)%*%(medias-mu0) # %*% = produto de matriz (se for * é produto de escalar)
T2 # O resultado do T² de rotteling é comparado na tabela F

cal=(((n-1)*2)/(n-2))
cal

tab=19.247 # encontramos na tabela F

cal*tab # 57.741

## 13,64 < 57.741 Aceita H0!

