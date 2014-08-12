# 참고자료 : Machine Learning for Hackers, Machine Learning in Action

distance.matrix <- function(df)
{
  distance <- matrix(rep(NA, nrow(df)^2), nrow=nrow(df))
  
  for(i in 1:nrow(df)) {
    for(j in 1:nrow(df)) {
      distance[j,i] <- dist(rbind(df[i, -ncol(df)], df[j, -ncol(df)]))
    }
  }
  
  return(distance)
}

k.nearest.neighbors <- function(i, distance, k=5) {
  return(order(distance[i,])[2:(k+1)])
}

knn <- function(df, k=5) {
  distance <- distance.matrix(df)
  
  predictions <- rep(NA, nrow(df))
  
  for(i in 1:nrow(df)) {
    indices <- k.nearest.neighbors(i, distance, k=k)
    predictions[i] <- ifelse(mean(df[indices, 'c']) > 0.5, 1, 0)
  }
  
  return(predictions)
}


# 트레이닝 데이터 생성 
n <- 20
loc1 = 2
loc2 = 4 # 5로 바뀌면 정확도 높아질 것
d22 <- sapply(1:n, function(i) { c(rnorm(1, mean=loc1, sd=1), rnorm(1, mean=loc1, sd=1)) } )
d44 <- sapply(1:n, function(i) { c(rnorm(1, mean=loc2, sd=1), rnorm(1, mean=loc2, sd=1)) } )

m <- t(cbind(d22, d44))
c <- c(rep(0,n), rep(1,n))
plot(m[,1], m[,2], col=c+1, xlim=c(0,10), ylim=c(0,10))
rm('df')
df <- data.frame(m, c)
df <- transform(df, kNNPredictions=knn(df)) # prediction 결과 필드 추가

nrow(df) # 전체 개수
sum(with(df, c != kNNPredictions)) # class 맞춘 개수
table(real_class=c, prediction=df[,'kNNPredictions'])

