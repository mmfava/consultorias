library(ggfortify)
df <- iris[c(1, 2, 3, 4)]
iris
autoplot(prcomp(df))
autoplot(prcomp(df), data = iris, colour = 'Species')
autoplot(prcomp(df), data = iris, colour = 'Species', label = TRUE, label.size = 3)
autoplot(prcomp(df), data = iris, colour = 'Species', shape = FALSE, label.size = 3)
autoplot(prcomp(df), data = iris, colour = 'Species', loadings = TRUE)
autoplot(prcomp(df), data = iris, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
library(cluster)
autoplot(fanny(iris[-5], 3), frame = TRUE)
autoplot(pam(iris[-5], 3), frame = TRUE, frame.type = 'norm')

