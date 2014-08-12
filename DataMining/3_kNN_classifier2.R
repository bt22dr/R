# 참고자료 : http://www.inside-r.org/packages/cran/knnflex/docs/knn.predict 

rm('knn')
library('class')

# a quick classification example
n1 <- 20
n2 <- 20
n <- n1 + n2
x1 <- c(rnorm(n1,mean=1),rnorm(n2,mean=5))
x2 <- c(rnorm(n1,mean=5),rnorm(n2,mean=1))
x  <- cbind(x1,x2)
y <- c(rep(1,n1),rep(0,n2))
train <- sample(1:n, n*(3/4))
# plot the training cases
plot(x1[train],x2[train],col = c("green3", "blue")[y[train]+1])
# predict the other cases
test <- (1:n)[-train]

predicted.y <- knn(x[train,], x[test,], y[train], k=5)
table(y[test], predicted.y)


# logistic regression의 decision boundary 그리기 
logit.model <- glm(y[train] ~ x1+x2, data=as.data.frame(x[train,]))
predictions <- as.numeric(predict(logit.model, as.data.frame(x[test,]))>0.5)
c <- logit.model$coef
x <- 0:100
y <- (-c[1] - c[2] * x) / c[3]
par(new=T)
plot(x,y, type="l", axes=F)


# donut data의 경우는?

# 1. logistic regression
d <- read.csv('./data/donut.csv')
d <- d[, c('x','y','color')]
plot(d$x, d$y, col=d$color)

#indices <- sample(1:nrow(d), 3/4*nrow(d))
logit.model <- glm(color~x+y, data=d)
predictions <- as.numeric(predict(logit.model, d) > 0.5)
x <- 0:100
y <- ( c[1] + c[2] * x) / -c[3]
par(new=T)
plot(x,y, type="l", axes=F)

nrow(d) # 전체 개수
sum(predictions != d[,3]) # 오류 개수

# 2. kNN
predicted.y <- knn(d[,c('x','y')], d[,c('x','y')], d[, 'color'], k=5)

nrow(d) # 전체 개수
sum(predicted.y != d[,'color']) # 오류 개수

table(d[, 'color'], predicted.y)
