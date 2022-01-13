library(circular)
x <- rvonmises(n=100, mu=circular(0), kappa=1)
angular.deviation(x)

x <- c(rvonmises(50, circular(0), 1), rvonmises(100, circular(pi/3), 10))
group <- c(rep(0, 50), rep(1, 100))
aov.circular(x, group)
aov.circular(x, group, method="LRT")

plot(rvonmises(10, circular(0), kappa=1))
arrows.circular(rvonmises(10, circular(0), kappa=1))
arrows.circular(rvonmises(10, circular(0), kappa=1), y=runif(10), col=2)
arrows.circular(rvonmises(10, circular(0), kappa=1), y=runif(10),
                x0=runif(10, -1, 1), y0=runif(10, -1, 1), col=3)

data=read.csv2(file.choose(),header = TRUE,dec=",")

attach(data)
table(botão[espécie=="Cordia trichotoma (Vell.) Arráb. ex Steud."])[mês=="ago/14"]
