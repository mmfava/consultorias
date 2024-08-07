####################################################################################################
##                                    Distance Matrix Computation                                 ##
##                                         R documentation                                        ##
####################################################################################################

## Description
## This function computes and returns the distance matrix computed by using the specified distance 
## measure to compute the distances between the rows of a data matrix.

## Usage
## dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

## as.dist(m, diag = FALSE, upper = FALSE)

## Default S3 method:
as.dist(m, diag = FALSE, upper = FALSE)

## S3 method for class 'dist'
print(x, diag = NULL, upper = NULL,
      digits = getOption("digits"), justify = "none",
      right = TRUE, ...)

## S3 method for class 'dist'
as.matrix(x, ...)

## Arguments
## x: a numeric matrix, data frame or "dist" object.

## method: the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan",
## "canberra", "binary" or "minkowski". Any unambiguous substring can be given.

## diag: logical value indicating whether the diagonal of the distance matrix should be printed by 
## print.dist.

## upper: logical value indicating whether the upper triangle of the distance matrix should be 
## printed by print.dist.

## p: The power of the Minkowski distance.

## m: An object with distance information to be converted to a "dist" object. For the default method,
## a "dist" object, or a matrix (of distances) or an object which can be coerced to such a matrix
## using as.matrix(). (Only the lower triangle of the matrix is used, the rest is ignored).

## digits, justify: passed to format inside of print().

# right, ...:further arguments, passed to other methods.


## Details
# Available distance measures are (written for two vectors x and y):
  
## euclidean: Usual distance between the two vectors (2 norm aka L_2), sqrt(sum((x_i - y_i)^2)).

## maximum:Maximum distance between two components of x and y (supremum norm)

## manhattan: Absolute distance between the two vectors (1 norm aka L_1).

## canberra: sum(|x_i - y_i| / |x_i + y_i|). Terms with zero numerator and denominator are omitted 
## from the sum and treated as if the values were missing. This is intended for non-negative values 
## (e.g., counts): taking the absolute value of the denominator is a 1998 R modification to avoid
## negative distances.

## binary: (aka asymmetric binary): The vectors are regarded as binary bits, so non-zero elements 
## are 'on' and zero elements are 'off'. The distance is the proportion of bits in which only one 
## is on amongst those in which at least one is on.

## minkowski: The p norm, the pth root of the sum of the pth powers of the differences of the 
## components.

## Missing values are allowed, and are excluded from all computations involving the rows within 
## which they occur. Further, when Inf values are involved, all pairs of values are excluded when 
## their contribution to the distance gave NaN or NA. If some columns are excluded in calculating a 
## Euclidean, Manhattan, Canberra or Minkowski distance, the sum is scaled up proportionally to the 
## number of columns used. If all pairs are excluded when calculating a particular distance, the 
## value is NA.

## The "dist" method of as.matrix() and as.dist() can be used for conversion between objects of class
## "dist" and conventional distance matrices.

## as.dist() is a generic function. Its default method handles objects inheriting from class "dist",
## or coercible to matrices using as.matrix(). Support for classes representing distances (also known
## as dissimilarities) can be added by providing an as.matrix() or, more directly, an as.dist method
## for such a class.

## Value
## dist returns an object of class "dist".

## The lower triangle of the distance matrix stored by columns in a vector, say do. If n is the 
## number of observations, i.e., n <- attr(do, "Size"), then for i < j ??? n, the dissimilarity between
## (row) i and j is do[n*(i-1) - i*(i-1)/2 + j-i]. The length of the vector is n*(n-1)/2, i.e., 
## of order n^2.

## The object has the following attributes (besides "class" equal to "dist"):
  
## Size: integer, the number of observations in the dataset.

## Labels: optionally, contains the labels, if any, of the observations of the dataset.

## Diag, Upper: logicals corresponding to the arguments diag and upper above, specifying how the 
## object should be printed.

## call: optionally, the call used to create the object.

## method: optionally, the distance method used; resulting from dist(),the (match.arg()ed) method 
## argument.


## References
## Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.

## Mardia, K. V., Kent, J. T. and Bibby, J. M. (1979) Multivariate Analysis. Academic Press.

## Borg, I. and Groenen, P. (1997) Modern Multidimensional Scaling. Theory and Applications. Springer.



## Examples

require(graphics)

x <- matrix(rnorm(100), nrow = 5)
dist(x)
dist(x, diag = TRUE)
dist(x, upper = TRUE)
m <- as.matrix(dist(x))
d <- as.dist(m)
stopifnot(d == dist(x))

## Use correlations between variables "as distance"
dd <- as.dist((1 - cor(USJudgeRatings))/2)
round(1000 * dd) # (prints more nicely)
plot(hclust(dd)) # to see a dendrogram of clustered variables

## example of binary and canberra distances.
x <- c(0, 0, 1, 1, 1, 1)
y <- c(1, 0, 1, 1, 0, 1)
dist(rbind(x, y), method = "binary")
## answer 0.4 = 2/5
dist(rbind(x, y), method = "canberra")
## answer 2 * (6/5)

## To find the names
labels(eurodist)

## Examples involving "Inf" :
## 1)
x[6] <- Inf
(m2 <- rbind(x, y))
dist(m2, method = "binary")   # warning, answer 0.5 = 2/4
## These all give "Inf":
stopifnot(Inf == dist(m2, method =  "euclidean"),
          Inf == dist(m2, method =  "maximum"),
          Inf == dist(m2, method =  "manhattan"))
##  "Inf" is same as very large number:
x1 <- x; x1[6] <- 1e100
stopifnot(dist(cbind(x, y), method = "canberra") ==
            print(dist(cbind(x1, y), method = "canberra")))

## 2)
y[6] <- Inf #-> 6-th pair is excluded
dist(rbind(x, y), method = "binary"  )   # warning; 0.5
dist(rbind(x, y), method = "canberra"  ) # 3
dist(rbind(x, y), method = "maximum")    # 1
dist(rbind(x, y), method = "manhattan")  # 2.4
