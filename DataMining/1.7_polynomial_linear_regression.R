
# 참고자료
# http://www.r-bloggers.com/polynomial-regression-techniques/

x <- c(-0.99768, -0.69574, -0.40373, -0.10236, 0.22024, 0.47742, 0.82229)
y <- c(2.0885, 1.1646, 0.3287, 0.46013, 0.44808, 0.10013, -0.32952)
plot(x,y)
model <- lm(y~x)
plot(y~x)
abline(model, col="blue")

x_2 <- x^2
x_3 <- x^3
x_4 <- x^4
x_5 <- x^5

X <- cbind(1, x, x_2, x_3, x_4, x_5)
colnames(X) <- 0:5
model <- lm(y~X)
theta <- model$coef
theta <- theta[-2]

x_axis <- seq(-1, 1, by=0.05)
y_axis <- 1*theta[1] + 
  x_axis^1*theta[2] + 
  x_axis^2*theta[3] + 
  x_axis^3*theta[4] + 
  x_axis^4*theta[5] + 
  x_axis^5*theta[6]
plot(x,y)
points(x=x_axis, y=y_axis, type="l")

# 또는 
fit1 <- lm(y ~ x)
fit2 <- lm(y ~ x + I(x^2))
fit5 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
plot(x, y)
points(x_axis, predict(fit1, data.frame(x=x_axis)), type="l", col="red")
points(x_axis, predict(fit2, data.frame(x=x_axis)), type="l", col="green")
points(x_axis, predict(fit5, data.frame(x=x_axis)), type="l", col="blue")

# 위에 해준 작업을 간단하게 표현하면
fit5b <- lm(y~poly(x, 5, raw=T))
fit5b$coef
model$coef
summary(fit5b)
plot(x, y)
points(x_axis, predict(fit5b, data.frame(x=x_axis)), type="l", col="blue")






