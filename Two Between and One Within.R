# Two between subject variables, one within
# Data from St. Lawrence et al.(1995)
# Methods8, p. 479

#  Note: In data file each subject is called a Person, and I convert that to a factor.
#  In reshape, it creates a variable called "subject," but that is not what I want to use.
#  In aov the model is based on Person, not subject.
rm(list = ls())
data <- read.table("Tab14-7.dat.txt", header = T)
head(data)
# Create factors
data$Condition <- factor(data$Condition)
data$Sex <- factor(data$Sex)
data$Person <- factor(data$Person)
#Reshape the data
dataLong <- reshape(data = data, varying = 4:7, v.names = "outcome", timevar
                    = "Time", idvar = "subject", ids = 1:40, direction = "long")
dataLong$Time <- factor(dataLong$Time)
attach(dataLong)
tapply(dataLong$outcome, dataLong$Sex, mean)
tapply(dataLong$outcome, dataLong$Condition, mean)
tapply(dataLong$outcome, dataLong$Time, mean)
options(contrasts = c("contr.sum","contr.poly"))
model1 <- aov(outcome ~ (Condition*Sex*factor(Time)) + Error(Person/(Time)))
summary(model1)

interaction.plot(dataLong$Time, factor(dataLong$Condition),
                 dataLong$outcome, fun = mean, type="b", pch = c(2,4,6),
                 legend = "F", 
                 col = c(3,4,6), ylab = "Mean of Outcome")
interaction.plot(dataLong$Time, factor(dataLong$Sex),
                 dataLong$outcome, fun = mean, type="b", pch = c(2,4,6),
                 legend = "F", 
                 col = c(3,4,6), ylab = "Mean of Outcome")
