# 참고자료 
# - http://r4pda.co.kr/r4pda_2013_08_13.pdf
# - R을 이용한 누구나하는 통계분석 



# 회귀분석에 사용할 데이터 
head(cars)

#############################################################################

# 첫 번째 시도 : 회귀모형 생성
# dist = (theta0) + (theta1) * speep + (epsilon) 
# 위 모델을 관계식으로 나타내면 dist ~ speed 가 된다. (종속변수 ~ 독립변수)
model <- lm(dist ~ speed, cars)
model

# 그래프로 나타내기
plot(dist ~ speed, data=cars, col="blue")
abline(model, col="red")

# 회귀분석 결과 상세 내용 
summary(model)
# 추정된 회귀식 : dist(i) = -17.579 + 3.45 * speed(i)
# 결정계수(coefficient of determination) R^2 값은 모델의 설명력을 평가해준다.
with(cars, cor(dist, speed))^2 # R^2 값은 피어슨 상관계수 제곱값과 동일하다.



#############################################################################


# 두 번째 시도 : y절편을 제거하여 회귀모형 생성 
# (speed가 0일 때 음수 거리가 나온다)
model1 <- lm(dist ~ speed-1, cars)
model1

# 그래프로 나타내기
plot(dist ~ speed-1, data=cars, col="blue", xlim=c(0,27), ylim=c(0,130))
abline(model1, col="red")

# 회귀분석 결과 상세 내용 
summary(model1)


#############################################################################


# 세 번째 시도 : sqrt 변환
# speed와 dist는 대략 2차식의 패턴을 보이고, 
# speed가 증가하면 dist의 퍼진 정도도 더 커진다. 
plot(sqrt(dist) ~ speed-1, data=cars, col="blue", xlim=c(0,27), ylim=c(0,11))
model2 <- lm(sqrt(dist) ~ speed-1, data=cars)
abline(model2, col="red")
summary(model2) # 처음 시도에 비해 R^2값, t-값 모두 증가!!
# 추정된 회귀식 : dist(i) = 0.39675^2 * speed(i)^2




#############################################################################



# 회귀 진단
# 잘 적합된 모형에서 나온 잔차는 정규분포를 따라야 하고
# 분산이 일정하고 특별한 추세를 보이지 않아야 한다. 
# 잔차는 중요한 정보를 가지지 않은 찌꺼기이므로 
# 잔차가 추세를 보인다면 모델에 포함되어야 할 정보가 빠졌다는 증거이다. 

# normal Q-Q plot
par(mfrow=c(1,2))
qqnorm(resid(model)); qqline(resid(model))
qqnorm(resid(model2)); qqline(resid(model2))
# par(mfrow=c(2,2)); plot(model2)

# 잔차들의 정규성 검정 
# Shapiro-Wilk test에서는 자료가 정규분포를 따르는 것이 귀무가설로 설정된다.
# p-value가 0.05 보다 클 때 귀무가설이 채택되어 정규성을 만족한다. 
shapiro.test(resid(model)) # 잔차가 정규분포를 따르지 않는다.
shapiro.test(resid(model2)) # 잔차가 정규분포를 따른다.


#############################################################################


# 선형회귀 결과 추출
B <- coef(model)
B

# 예측값 : -17.579095 + 3.932409 * speed로 추정한 값에 해당한다.
fitted(model)[1:4]

# 잔차 : 예측값과 실제 dist간의 차이
resid(model)[1:4]

# 실제 dist에서 예측값을 빼면 잔차
cars$dist[1:4] - resid(model)[1:4]
fitted(model)[1:4] + resid(model)[1:4] # 예측값에 잔차를 더하면 dist

# 잔차의 표준오차 : 잔차의 제곱합을 자유도로 나눈 값의 sqrt
summary(model)$sigma
sqrt(sum(resid(model)^2)/model$df.residual)

# 계수의 신뢰구간(95%)
confint(model)



#############################################################################



cars$speed
test_data <- data.frame(speed=3) # 예측을 원하는 테스트 데이터 

# 테스트 데이터를 이용한 예측
predict(model, test_data)

# 예측값의 신뢰구간
# fit : 예측값의 점 추정치, lwr와 upr : 각각 신뢰구간의 하한과 상한 값

# 특정 속도를 가진 평균적인 차량 제동거리의 예측값과 신뢰구간.
# 다음 식을 바탕으로 신뢰구간을 구함 :  dist = (theta0) + (theta1) × speed 
# 평균적인 차량에 대한 추정 이므로 오차항은 고려되지 않는다. 
# 오차는 평균이 0인 정규분포로 가정되기 때문이다.
predict(model, newdata=data.frame(speed=c(3)), interval="confidence")

# 특정 속도를 가진 한대의 차량에 대한 제동거리 예측값과 신뢰구간.
# 다음 식을 바탕으로 신뢰구간을 구함 :  dist = (theta0) + (theta1) × speed + (epsilon)
# 오차항을 고려해야한다.
predict(model, newdata=data.frame(speed=c(3)), interval="prediction")
# 오차항으로인해 신뢰구간의 크기가 커짐을 볼 수 있다.




#############################################################################




# Simulation

summary(model2)
# residual standard error가 1.167이므로 (epsilon)은 N(0, 1.167^2)로 시뮬레이션 가능함
# 최종적으로 추정된 모형에 오차 (epsilon)을 추가하면 아래와 같다. 
# sqrt(dist(i)) = 0.39675*speed(i) + (epsilon(i)), (epsilon(i)) ~ N(0, 1.167^2)
# sqrt(dist(i)) ~ N(0.39675*speed(i), 1.167^2)

# 그러므로, rnorm(n=표본수, mean=예측값, sd=잔차의 표준오차) 결과를 제곱하여 
# dist를 시뮬레이션할 수 있다.
dist <- rnorm(n=nrow(cars), mean=fitted(model2), sd=summary(model2)$sigma)^2
plot(dist~cars$speed)
# 시뮬레이션 결과를 산포도로 그려보면 원래 데이터와 상당히 비슷함을 알 수 있다. 

