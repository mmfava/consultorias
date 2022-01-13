
olfato <-
  matrix(c(0,10,0,0,0,0,0,0,
           0,0,3,0,0,0,0,0,
           0,17,0,0,0,0,0,0,
           66,367,0,0,0,0,0,0,
           0,0,0,643,0,0,0,0),
         nrow = 5,
         byrow = TRUE,
         dimnames = list(1 : 5,
                         c("Controle", "T1", "T2", "T3","T4", "T5", "T6", "T7")))

install.packages("PMCMR")
library(PMCMR)
friedman.test(olfato)
posthoc.friedman.nemenyi.test(olfato)
boxplot(olfato, outline=F)
