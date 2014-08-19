normality.plot <- function(r.dist, n, ...) {
  r <- r.dist(n, ...)
  hist(scale(r), prob=T)
  lines(density(scale(r)))
  #curve(dnorm(x,0,1), col='blue', add=T)
  curve(dnorm, col='blue', add=T)
  shapiro.test(r)
}

op <- par(mfrow=c(1,2))
normality.plot(rchisq, n=50, df=1)
normality.plot(rchisq, n=50, df=10)
par(op)