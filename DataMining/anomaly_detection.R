

# 테스트 데이터 생성
d <- data.frame(srcIP=c("s1","s1","s1","s1","s2","s2","s3","s3","s3"),
                dstIP=c("d1","d2","d3","d2","d1","d4","d1","d4","d5"),
                port=c(1,2,1,4,2,3,1,4,4),
                cnt=c(3,6,3,1,6,4,2,8,1))

# src 별 dst IP 카운트
tapply(d$cnt, d[c('srcIP','dstIP')], sum)
#또는 tapply(d$cnt, list(d$srcIP, d$dstIP), sum)

# src 별 dst port 종류 카운트
tapply(d$cnt, d[c('srcIP','port')], length)
# 또는 table(d[c(1,3)]) 사용하면 NA가 0으로 바뀜




############################

library(outliers)

sum_na_rm <- function(x) {
  sum(x, na.rm=T)
}

d <- read.table("../infosec/20130730.dat", header=TRUE, as.is=c("switch", "srcIP", "dstIP"))
head(d)

# 한 방화벽 스위치 살펴보기 (srcIP -> dstIP)
tapply(d$cnt, d$switch, sum)
d1 <- d[d$switch=="114.31.63.26",]
td1 <- tapply(d1$cnt, d1[c('srcIP', 'dstIP')], sum)
dst_sum <- apply(td1, 1, sum_na_rm)
hist(dst_sum)
summary(dst_sum)
boxplot.stats(dst_sum, coef=2)$out

# 아웃라이어 체크하는 함수
func <- function(sub_table) {
  src_dstIP_table <- tapply(sub_table$cnt, sub_table[c(1,2)], sum)
  conn_cnt_src_IP <- apply(src_dstIP_table, 1, sum_na_rm)
  src_dstPort_table <- tapply(sub_table$cnt, sub_table[c(1,3)], length)
  port_cnt_src_IP <- apply(src_dstPort_table, 1, sum_na_rm)
  old.par <- par(mfrow=c(2,2))
  hist(conn_cnt_src_IP, main=switch, labels=TRUE)
  boxplot(conn_cnt_src_IP)
  hist(port_cnt_src_IP, main=switch, labels=TRUE)
  boxplot(port_cnt_src_IP)
  par(old.par)
  #print(summary(conn_cnt_src_IP))
  #print(boxplot.stats(conn_cnt_src_IP, coef=2)$out)
  #print(outlier(conn_cnt_src_IP))
  print("IP")
  print(sort(conn_cnt_src_IP[scores(conn_cnt_src_IP, type="z")>5], decreasing=T)[1:5])
  print("port")
  print(sort(port_cnt_src_IP[scores(port_cnt_src_IP, type="z")>5], decreasing=T)[1:5])
}

n <- length(levels(as.factor(d$switch)))
a <- tapply(d$cnt, d$switch, sum)
t <- tapply(d$cnt, d$switch, sum) / table(d$switch)
sort(a, decreasing=TRUE)
idx <- order(a, decreasing=T)
t[idx]
switch_vec <- levels(as.factor(d$switch))

# 몇몇 방화벽에 대해서 체크
switch_vec <- c('61.100.2.37', '211.239.159.236', '123.111.233.67', '210.117.191.209')
# switch_vec <- names(a[idx])[1:n]
for (switch in switch_vec) {  
  cat("\n=== switch IP :", switch, "===\n")
  sub_table <- d[d$switch==switch, -1]
  func(sub_table)
}



# 시계열 분석
# http://stackoverflow.com/questions/13327373/univariate-outlier-detection
y<-c(0.59, 0.61, 0.59, 1.55, 1.33, 3.50, 1.00, 1.22, 2.50, 3.00, 3.79, 3.98, 4.33, 4.45, 4.59, 4.72, 4.82, 4.90, 4.96, 7.92, 5.01, 5.01, 4.94, 5.05, 5.04, 5.03, 5.06, 5.10, 5.04, 5.06, 7.77, 5.07, 5.08, 5.08, 5.12, 5.12, 5.08, 5.17, 5.18) 
library(TSA)
ar = TSA::arima(y, c(1,0,0))
detectAO(ar)
plot(y)
lines(y)
