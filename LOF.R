

library(Rlof)
 outlier.scores <- lof(iris2, k=5)
 # try with different number of neighbors (k = 5,6,7,8,9 and 10)
 outlier.scores <- lof(iris2, k=c(5:10))
 
 
 hist( outlier.scores, breaks=10)
 