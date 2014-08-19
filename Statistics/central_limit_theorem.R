rm(v1, v2, v3)
v1 <- vector('integer')
v2 <- vector('integer')
v3 <- vector('integer')

for(i in 1:20000) { v1[i] <- sum(sample(c(1,2,9), 25, replace=T)) }
for(i in 1:20000) { v2[i] <- sum(sample(c(1,2,9), 50, replace=T)) }
for(i in 1:20000) { v3[i] <- sum(sample(c(1,2,9), 100, replace=T)) }

op <- par(mfrow=c(3,1))
hist(v1, breaks = seq(min(v1), max(v1), by = 1))
hist(v2, breaks = seq(min(v2), max(v2), by = 1))
hist(v3, breaks = seq(min(v3), max(v3), by = 1))
par(op)