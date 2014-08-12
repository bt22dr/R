# 참고자료 
# http://openclassroom.stanford.edu/MainFolder/DocumentPage.php?course=MachineLearning&doc=exercises/ex5/ex5.html
# http://www.r-bloggers.com/machine-learning-ex5-2-regularized-logistic-regression/


mydata = read.csv("http://spreadsheets.google.com/pub?key=0AnypY27pPCJydHZPN2pFbkZGd1RKeU81OFY3ZHJldWc&output=csv", header = TRUE)
plot(mydata$u[mydata$y == 0], mydata$v[mydata$y == 0],, xlab="u", ylab="v")
points(mydata$u[mydata$y == 1], mydata$v[mydata$y == 1], col="blue", pch=3)
legend("topright", c("y=0","y=1"), pch=c(1, 3), col=c("black", "blue"), bty="n")

# sigmoid function
g = function (z) {
  return (1 / (1 + exp(-z) ))
} 
plot(g(-10:10), type="l")

# build hight order feature vector
# for 2 features, for a given degree
hi.features = function (f1,f2,deg) {
  n = ncol(f1)
  ma = matrix(rep(1,length(f1)))
  for (i in 1:deg) {
    for (j in 0:i)    
      ma = cbind(ma, f1^(i-j) * f2^j)
  }
  return(ma)
} 
hi.features(c(1,2), c(3,4),2)
hi.features(c(1), c(2),2)
# creates: 1 u v u^2 uv v^2 ...

# hypothesis
h = function (x,th) {
  return(g(x %*% th))
} # h(x,th)

# derivative of J 
grad = function (x,y,th,m,la) {
  G = (la/m * th)
  G[1,] = 0
  return((1/m * t(x) %*% (h(x,th) - y)) +  G)
} # grad(x,y,th,m,la)

H = function (x,y,th,m,la) {
  n = length(th)
  L = la/m * diag(n)
  L[1,] = 0
  return((1/m * t(x) %*% x * diag(h(x,th)) * diag(1 - h(x,th))) + L)
} # H(x,y,th,m,la)

# cost function
J = function (x,y,th,m,la) {
  pt = th
  pt[1] = 0
  A = (la/(2*m))* t(pt) %*% pt
  return((1/m * sum(-y * log(h(x,th)) - (1 - y) * log(1 - h(x,th)))) + A)
} # J(x,y,th,m,la)

# setup variables
m = length(mydata$u) # samples
x = hi.features(mydata$u, mydata$v,6)
n = ncol(x) # features
y = matrix(mydata$y, ncol=1)

# lambda = 1
# use the cost function to check is works
th1 = matrix(0,n)
la = 1
jiter = array(0,c(15,1))
for (i in 1:15) {
  jiter[i] = J(x,y,th1,m,la)
  th1 = th1 - solve(H(x,y,th1,m,la)) %*% grad(x,y,th1,m,la) 
}

# check that is converging correctly
plot(jiter, xlab="iterations", ylab="cost J")

# lambda = 0
th0 = matrix(0,n)
la = 0
for (i in 1:15) {
  th0 = th0 - solve(H(x,y,th0,m,la)) %*% grad(x,y,th0,m,la) 
}

# lambda = 10
th10 = matrix(0,n)
la = 10
for (i in 1:15) {
  th10 = th10 - solve(H(x,y,th10,m,la)) %*% grad(x,y,th10,m,la) 
}

# calculate the decision boundary line
# create many points
u = seq(-1, 1.2, len=200);
v = seq(-1, 1.2, len=200);
z0 = matrix(0, length(u), length(v))
z1 = matrix(0, length(u), length(v))
z10 = matrix(0, length(u), length(v))
for (i in 1:length(u)) {
  for (j in 1:length(v)) {
    z0[j,i] =  hi.features(u[i],v[j],6) %*% th0
    z1[j,i] =  hi.features(u[i],v[j],6) %*% th1
    z10[j,i] =  hi.features(u[i],v[j],6) %*% th10
  }
}

# plots
contour(u,v,z0,nlev = 0, xlab="u", ylab="v", nlevels=0, col="black",lty=2)
contour(u,v,z1,nlev = 0, xlab="u", ylab="v", nlevels=0, col="red",lty=2, add=TRUE)
contour(u,v,z10,nlev = 0, xlab="u", ylab="v", nlevels=0, col="green3",lty=2, add=TRUE)
points(mydata$u[mydata$y == 0], mydata$v[mydata$y == 0])
points(mydata$u[mydata$y == 1], mydata$v[mydata$y == 1], col="blue", pch=3)
legend("topright",  c(expression(lambda==0), expression(lambda==1),expression(lambda==10)), lty=1, col=c("black", "red","green3"),bty="n" )



# 위 내용을 glmnet 패키지를 이용해 간단하게 수행해보면 아래와 같다.
######################################################################

# install.packages("glmnet")
library(glmnet)

x = hi.features(mydata$u, mydata$v, 6)
x = x[,-1]
n = ncol(x) # features
y = matrix(mydata$y, ncol=1)
fit <- glmnet(x, y, family = "binomial", alpha=0, lambda=c(0,0.1,1,10), nlambda=4, standardize=T)
table(predict(fit, newx = x, type = "class", s = 0.01), y)
theta_glmnet <- coef(fit,s=c(0,0.1,1,10))

# calculate the decision boundary line
# create many points
u = seq(-1, 1.2, len=200);
v = seq(-1, 1.2, len=200);
z0 = matrix(0, length(u), length(v))
z01 = matrix(0, length(u), length(v))
z1 = matrix(0, length(u), length(v))
z10 = matrix(0, length(u), length(v))
for (i in 1:length(u)) {
  for (j in 1:length(v)) {
    z0[j,i]  =  hi.features(u[i],v[j],6) %*% as.matrix(theta_glmnet[,1, drop=F])
    z01[j,i] =  hi.features(u[i],v[j],6) %*% as.matrix(theta_glmnet[,2, drop=F])
    z1[j,i]  =  hi.features(u[i],v[j],6) %*% as.matrix(theta_glmnet[,3, drop=F])
    z10[j,i] =  hi.features(u[i],v[j],6) %*% as.matrix(theta_glmnet[,4, drop=F])
  }
}

contour(u,v,z0,nlev = 0, xlab="u", ylab="v", nlevels=0, col="black",lty=2)
contour(u,v,z01,nlev = 0, xlab="u", ylab="v", nlevels=0, col="cyan",lty=2, add=TRUE)
contour(u,v,z1,nlev = 0, xlab="u", ylab="v", nlevels=0, col="red",lty=2, add=TRUE)
contour(u,v,z10,nlev = 0, xlab="u", ylab="v", nlevels=0, col="green3",lty=2, add=TRUE)
points(mydata$u[mydata$y == 0], mydata$v[mydata$y == 0])
points(mydata$u[mydata$y == 1], mydata$v[mydata$y == 1], col="blue", pch=3)
legend("topright",  c(expression(lambda==0), expression(lambda==0.1), expression(lambda==1),expression(lambda==10)), lty=1, col=c("black", "cyan", "red","green3"),bty="n" )





# k-fold Cross-validation으로 최적의 lambda 값을 얻기
######################################################################

cv.glmmod <- cv.glmnet(x, y, family = "binomial", alpha=0, lambda=exp(seq(-10,2, by=0.1)))

# In each ﬁgure the left vertical line corresponds to the minimum error, while the right
# vertical line the largest value of lambda such that the error is within one standard-error of
# the minimum—the so called “one-standard-error” rule. The top of each plot is annotated with
# the size of the models.
plot(cv.glmmod)
log(cv.glmmod$lambda.min) # 왼쪽선
log(cv.glmmod$lambda.1se) # 오른쪽선

histogram(cv.glmmod$lambda)

coef(cv.glmmod$glmnet.fit, s=cv.glmmod$lambda.1se)
best_lambda <- cv.glmmod$lambda.min
best_lambda 

predict(cv.glmmod,newx=x[54:63,])
coef(cv.glmmod) # == coef(cv.glmmod, s="lambda.1se")
coef(cv.glmmod, s="lambda.min")
predict(cv.glmmod,newx=x[54:63,],s=c(cv.glmmod$lambda.min, cv.glmmod$lambda.1se))

p <- predict(cv.glmmod,newx=x, s=c(cv.glmmod$lambda.min, cv.glmmod$lambda.1se))
sum(y == (p[,1]>0))
sum(y == (p[,2]>0))


