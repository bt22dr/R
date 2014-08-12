# 참고자료 : 
# http://data.princeton.edu/R/glms.html
# http://www.cleveralgorithms.com/machinelearning/regression/logistic_regression.html(링크삭제)
#####################################################################################

setwd("C:\\Users\\Coupang\\gdrive_bt22dr\\업무 노트\\R\\kku")

# creates a 2d classification problem
x <- c(rnorm(50, mean=0), rnorm(50, mean=2))
y <- c(rnorm(50, mean=2), rnorm(50, mean=0))
z <- c(rep("a", 50), rep("b", 50))
data <- data.frame(x, y, z)

# split data in to train and test (67%/33%)
training_set <- sample(100,67)
train <- data[training_set,]
test <- data[-training_set,]

# plot the training dataset
plot(train$x, train$y, col=train$z, xlab="x", ylab="y")

# create the discriminator using Logistic Regression
model <- glm(
  z~x+y, # model formula, z give x and y
  binomial(link="logit"), # binomial using a logit link function
  train) # the training dataset

# summarize the fitted model
summary(model)

# plot the model's decision boundary
slope <- coef(model)[2]/(-coef(model)[3])
intercept <- coef(model)[1]/(-coef(model)[3])
abline(intercept, slope, lty=5, col='blue')

# make predictions
probabilities <- predict.glm(model, test[,1:2], type="response")
# convert probabilities into class values
predictions <- cut(probabilities, c(-Inf,0.5,Inf), labels=c("a","b"))
# summarize the predictions as a confusion matrix
table(predictions, test$z)

# plot the test dataset
plot(test$x, test$y, col=test$z, xlab="x", ylab="y")
abline(intercept, slope, lty=5, col='blue')
# 이 그림에서 생성된 데이터에 따라 모델이 약간 안 맞는듯한 그림이 나올 수도 있는데 
# training data로 만든 모델로 test data를 예측했기 때문이다.
# 즉 training set에는 적합한 모델이었지만 test data에는 잘 안맞는 모델이라는 얘기






# exam data로 테스트해보기
#########################

d <- read.csv('./data/ex2data1.csv')
#d <- d[, c('ex1','ex2','admission')]
plot(d$ex1, d$ex2, col=d$admission+1,)
logit.model <- glm(admission~ex1+ex2, binomial(link="logit"), data=d)
predictions <- ifelse((predict(logit.model, d[,-3]) >= 0.5), 1, 0)
fitted(logit.model)

nrow(d) # 전체 개수
sum(predictions != d[,3]) # 오류 개수
table(predictions, d[,3])

slope <- coef(logit.model)[[2]]/(-coef(logit.model)[[3]])
intercept <- coef(logit.model)[[1]]/(-coef(logit.model)[[3]])
slope; intercept
abline(intercept, slope, lty=5)
# TODO: 테이블 결과와 그림이 잘 안맞는데 검증해볼것!


