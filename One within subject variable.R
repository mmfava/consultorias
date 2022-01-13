## R Code for One Within Subject Variable
#  R for repeated measures
#  One within subject measure and no between subject measures
#  Be sure to run this from the beginning because 
#  otherwise vectors become longer and longer.
library(car)
rm(list = ls())
##  You want to clear out old variables --with "rm(list = ls())" --
##  before building new ones.

data <- read.table("NolenHoeksema.dat.txt", header = TRUE)
datLong <- reshape(data = data, varying = 2:6, v.names = "outcome", timevar
                   = "time", idvar = "Subject", ids = 1:9, direction = "long")

datLong$time <- factor(datLong$time)
datLong$Subject <- factor(datLong$Subject)
orderedTime <- datLong[order(datLong$time),]
options(contrasts=c("contr.sum","contr.poly"))

# Using "data = dataLong" I can use the simple names for the variables
modelAOV <- aov(outcome~factor(time)+Error(factor(Subject)), data = datLong)
print(summary(modelAOV))
obtF <- summary(modelAOV)$"Error: Within"[[1]][[4]][1]

plot(datLong$time, datLong$outcome, pch = c(2,4,6), col = c(3,4,6), ylim=c(0,25))
legend(1, 25, c("same", "different", "control"), col = c(4,6,3),
       text.col = "green4",  pch = c(4, 6, 2),
       bg = 'gray90')

