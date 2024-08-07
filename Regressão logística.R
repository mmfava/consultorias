mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)
summary(mydata)
sapply(mydata, sd)
xtabs(~admit + rank, data = mydata)
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)
confint(mylogit)
confint.default(mylogit)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)
exp(coef(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit)))
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))     
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))})

head(newdata3)

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                                                    ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
                                                                                                                      size = 1)

## S3 method for class 'default':
confusionMatrix(())

## S3 method for class 'table':
confusionMatrix((data, positive = NULL, prevalence = NULL, ...))