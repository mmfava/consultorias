erly=read.csv2(file.choose(), h=T, sep=";", dec=",")
data$Horas.embebição2 <- data$Horas.embebição^2
data <- transform(data, Horas.embebição2=Horas.embebição^2, 
                  Horas.embebição3=Horas.embebição^3, Horas.embebiçãor=sqrt(Horas.embebição), 
                  Horas.embebiçãol=log(Horas.embebição), Horas.embebiçãoi=1/Horas.embebição, 
                  Horas.embebiçãoi2=1/Horas.embebição^2,
                  Proporção.do.extrato2=Proporção.do.extrato^2, 
                  Proporção.do.extrato3=Proporção.do.extrato^3, Proporção.do.extrator=sqrt(Proporção.do.extrato), 
                  Proporção.do.extratol=log(Proporção.do.extrato), Proporção.do.extratoi=1/Proporção.do.extrato, 
                  Proporção.do.extratoi2=1/Proporção.do.extrato^2)
attach(erly)

names(data)

m0=lm(POD.embriao~Horas.embebição+Proporção.do.extrato)
summary(m0)
plot(POD.embriao~Horas.embebição) # xlab=, ylab=
lines(fitted(m0)~Horas.embebição, col="red")
abline(m0, col=3, lty=2)

# análise de resíduos
par(mfrow=c(2,2))
plot(m0)
dev.off()
# ajuste do modelo quadrático
m1 <- lm(POD.embriao~Horas.embebição+Horas.embebição2+Proporção.do.extrato+Proporção.do.extrato2)
summary(m1)
plot(POD.embriao~Horas.embebição) # xlab=, ylab=
lines(fitted(m1)~Horas.embebição2)
plot(m1)

anova(m0,m1)


#
#------------------------------------------------------------------------------------------
# modelo cúbico
m2 <- lm(POD.embriao~Horas.embebição+Horas.embebição2+Horas.embebição3+Proporção.do.extrato+
           Proporção.do.extrato2+Proporção.do.extrato3)
summary(m2)
plot(POD.embriao~Horas.embebição)
lines(fitted(m2)~Horas.embebição)
plot(m2)

anova(m0,m2)
anova(m0,m1)

AIC(m0,m1,m2)

library(plotly)
packageVersion('plotly')

p <- plot_ly(erly, x=~Horas.embebição,y=~Proporção.do.extrato, z=~POD.embriao) %>% add_surface()
chart_link = plotly_POST(p, filename="surface/1")
chart_link

data=data.frame(economics)
#



#------------------------------------------------------------------------------------------
# modelo recíproco
m3 <- lm(h~d+di, data=dapcc)
summary(m3)
plot(h~d, dapcc); lines(fitted(m3)~d, dapcc); plot(m3)
#
#------------------------------------------------------------------------------------------
# modelo quadrado do recíproco
m4 <- lm(h~d+di2, data=dapcc)
summary(m4)
plot(h~d, dapcc); lines(fitted(m4)~d, dapcc); plot(m4)


## Not run: 

# plot_ly() tries to create a sensible plot based on the information you 
# give it. If you don't provide a trace type, plot_ly() will infer one.
plot_ly(economics, x = ~pop)
plot_ly(economics, x = ~date, y = ~pop)
# plot_ly() doesn't require data frame(s), which allows one to take 
# advantage of trace type(s) designed specifically for numeric matrices
plot_ly(z = ~volcano)
plot_ly(z = ~volcano, type = "surface")

data(volcano)

# plotly has a functional interface: every plotly function takes a plotly
# object as it's first input argument and returns a modified plotly object
add_lines(plot_ly(economics, x = ~date, y = ~unemploy/pop))

# To make code more readable, plotly imports the pipe operator from magrittr
economics %>% plot_ly(x = ~date, y = ~unemploy/pop) %>% add_lines()

# Attributes defined via plot_ly() set 'global' attributes that 
# are carried onto subsequent traces, but those may be over-written
plot_ly(economics, x = ~date, color = I("black")) %>%
  add_lines(y = ~uempmed) %>%
  add_lines(y = ~psavert, color = I("red"))

# Attributes are documented in the figure reference -> https://plot.ly/r/reference
# You might notice plot_ly() has named arguments that aren't in this figure
# reference. These arguments make it easier to map abstract data values to
# visual attributes.
p <- plot_ly(iris, x = ~Sepal.Width, y = ~Sepal.Length) 
add_markers(p, color = ~Petal.Length, size = ~Petal.Length)
add_markers(p, color = ~Species)
add_markers(p, color = ~Species, colors = "Set1")
add_markers(p, symbol = ~Species)
add_paths(p, linetype = ~Species)


## End(Not run)

