
m <- 50; mu <- 0; sigma <- 1; n <- 15

z.int <- function(mu, sigma, n, alpha = 0.05) {
  sample <- rnorm(n, mean=mu, sd=sigma)
  sem <- sigma/sqrt(n)
  E <- qnorm(1-alpha/2)*sem
  xbar <- mean(sample)
  xbar + c(-E, E)
}

d <- matrix(nrow=2, ncol=m)
for(i in 1:m) {
  d[,i] <- z.int(mu, sigma, n)
}

matplot(d, rbind(1:m, 1:m), type='l', lty=1)
abline(v=mu)