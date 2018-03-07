dirchletMLE<- function (x, weights = NULL, eps = 10^(-5), convcrit = 1e-05, 
          maxit = 1000, oldfac = 0.3, progress = FALSE) 
{
  N <- nrow(x)
  K <- ncol(x)
  x <- (x + eps)/(1 + 2 * eps)
  x <- x/rowSums(x)
  N <- nrow(x)
  if (is.null(weights)) {
    weights <- rep(1, N)
  }
  weights <- N * weights/sum(weights)
  log.pbar <- colMeans(weights * log(x))
  alphaprob <- colMeans(x * weights)
  p2 <- mean(x[, 1]^2 * weights)
  xsi <- (alphaprob[1] - p2)/(p2 - (alphaprob[1])^2)
  alpha <- xsi * alphaprob
  K1 <- matrix(1, K, K)
  conv <- 1
  iter <- 1
  while ((conv > convcrit) & (iter < maxit)) {
    alpha0 <- alpha
    g <- N * base::digamma(sum(alpha)) - N * base::digamma(alpha) + 
      N * log.pbar
    z <- N * digamma1(sum(alpha))
    H <- diag(-N * digamma1(alpha)) + z
    alpha <- alpha0 - solve(H, g)
    alpha[alpha < 0] <- 10^(-10)
    alpha <- alpha0 + oldfac * (alpha - alpha0)
    conv <- max(abs(alpha0 - alpha))
    if (progress) {
      print(paste(iter, sum(alpha), conv))
    }
    iter <- iter + 1
    utils::flush.console()
  }
  alpha0 <- sum(alpha)
  xsi <- alpha/alpha0
  res <- list(alpha = alpha, alpha0 = alpha0, xsi = xsi)
  return(res)
}