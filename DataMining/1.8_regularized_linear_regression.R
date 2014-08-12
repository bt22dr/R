# 참고자료
# http://openclassroom.stanford.edu/MainFolder/DocumentPage.php?course=MachineLearning&doc=exercises/ex5/ex5.html
# http://www.r-bloggers.com/machine-learning-ex5-1-regularized-linear-regression/


# setup variables
mydata = read.csv("http://spreadsheets.google.com/pub?hl=en_GB&hl=en_GB&key=0AnypY27pPCJydGhtbUlZekVUQTc0dm5QaXp1YWpSY3c&output=csv", header = TRUE)
#     x        y
# -0.99768	 2.0885
# -0.69574	 1.1646
# -0.40373	 0.3287
# -0.10236	 0.46013
#  0.22024	 0.44808
#  0.47742	 0.10013
#  0.82229	-0.32952


# view data
plot(mydata)

m = length(mydata$x) # samples
x = matrix(c(rep(1,m), mydata$x, mydata$x^2, mydata$x^3, mydata$x^4, mydata$x^5), ncol=6)
n = ncol(x) # features
y = matrix(mydata$y, ncol=1)
lambda = c(0,1,10)
d = diag(1,n,n)
d[1,1] = 0
th = array(0,c(n,length(lambda)))

# apply normal equations for each of the lambda's
for (i in 1:length(lambda)) {
  th[,i] = solve(t(x) %*% x + (lambda[i] * d)) %*% (t(x) %*% y)
}

# plot
plot(mydata)

# lets create many points
nwx = seq(-1, 1, len=50);
xx = matrix(c(rep(1,length(nwx)), nwx, nwx^2, nwx^3, nwx^4, nwx^5), ncol=6)
lines(nwx, xx %*% th[,1], col="blue", lty=2)
lines(nwx, xx %*% th[,2], col="red", lty=2)
lines(nwx, xx %*% th[,3], col="green3", lty=2)
legend("topright", c(expression(lambda==0), expression(lambda==1),expression(lambda==10)), lty=2,col=c("blue", "red", "green3"), bty="n")



# 위 내용을 glmnet 패키지를 이용해 간단하게 수행해보면 아래와 같다.
######################################################################

# install.packages("glmnet")
library(glmnet)

plot(mydata)
x = matrix(c(mydata$x, mydata$x^2, mydata$x^3, mydata$x^4, mydata$x^5), ncol=5)
fit=glmnet(x, y, alpha=0, lambda=c(0,1,10))
theta_glmnet <- coef(fit,s=c(0,1,10))

nwx = seq(-1, 1, len=50);
xx = matrix(c(rep(1,length(nwx)), nwx, nwx^2, nwx^3, nwx^4, nwx^5), ncol=6)
lines(nwx, xx %*% theta_glmnet[,1], col="blue", lty=2)
lines(nwx, xx %*% theta_glmnet[,2], col="red", lty=2)
lines(nwx, xx %*% theta_glmnet[,3], col="green3", lty=2)
legend("topright", c(expression(lambda==0), expression(lambda==1),expression(lambda==10)), lty=2,col=c("blue", "red", "green3"), bty="n")



