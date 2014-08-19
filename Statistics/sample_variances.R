
population <- list(c(2,4,6,8,10))
samples <- expand.grid(rep(population, 2))
freq_table <- apply(samples, 1, function(x) round(sd(x)^2))
dist_var <- table(cut(freq_table, breaks=-0.5:32.5, labels=0:32))
barplot(dist_var)

# 표본분산들의 평균은 모분산과 같다
# 즉, 표본분산은 모분산의 비편향추정량이다.
mean(freq_table)
n <- length(population[[1]])
var(unlist(population))*(n-1)/n