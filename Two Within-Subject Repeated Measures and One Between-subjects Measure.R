# These data are from the same Bouten and Swartzentrub study used above. 
# The difference is that I have used all three groups.

rm(list = ls())
data <- read.table("Tab14-11.dat.txt", header = T)
attach(data)
Phase <- factor(rep(1:2, each = 24, times = 4))
Cycle <- factor(rep(1:4, each = 48))
Group = factor(rep(1:3, each = 8,times = 8))
dv <- c(C1P1, C1P2, C2P1, C2P2, C3P1, C3P2, C4P1, C4P2)
Subj <- factor(rep(1:24, times = 8))
n <- 24    #Subjects
withincells <- 8    #withincells = 2*4
nobs <- length(dv)

cat("Cell Means Across Subjects \n")
print(tapply(dv, list(Cycle,Phase), mean))    #cell means
cat("\n")
#Standard Anova
options(contrasts = c("contr.sum","contr.poly"))
model1 <- aov(dv ~ (Group*Cycle*Phase) + Error(Subj/(Cycle*Phase)), 
              contrasts = contr.sum)
print(summary(model1))   # Standard repeated measures anova   
