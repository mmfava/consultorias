###  Test for Two Within-Subject Repeated Measures
### One tricky part of this code came from Ben Bauer at the Univ. of Trent, Canada. 
### He writes code that is more R-like than I do. In fact, he knows more about R than 
### I do.
# Two within subject variables, 1:8
# Data from Bouton & Schwartzentruber (1985) -- Group 2
# Methods8, p. 486

data <- read.table("Tab14-11long.dat.txt", header = T)
n <- 8    #Subjects
cells <- 8    #cells = 2*4
nobs <- length(data$dv)
attach(data)
cat("Cell Means \n")
print(tapply(dv, list(Cycle,Phase), mean))    #cell means
cat("\n")
Subj <- factor(Subj)
Phase <- factor(Phase)
Cycle <- factor(Cycle)
#Standard Anova
options(contrasts = c("contr.sum","contr.poly"))
model1 <- aov(dv ~ (Cycle*Phase) + Error(Subj/(Cycle*Phase)), contrasts = contr.sum)
summary(model1)   # Standard repeated measures anova   
## I have used this example elsewhere  except with a randomization approach. 
