# http://wolfpack.hnu.ac.kr/lecture/Regression/ch6_multicollinearity.pdf
# http://kernelist.egloos.com/48044
# http://r4pda.co.kr/r4pda_2013_05_12.pdf

library(car)
mtcars_num <- mtcars[,c(1,3:7)]

# 산점도 행렬과 상관계수만으로 다중공선성 1차 진단
######################################################
pairs(mtcars_num, panel=panel.smooth)
cor(mtcars_num[,-1])
#            disp         hp        drat         wt        qsec
# disp  1.0000000  0.7909486 -0.71021393  0.8879799 -0.43369788
# hp    0.7909486  1.0000000 -0.44875912  0.6587479 -0.70822339
# drat -0.7102139 -0.4487591  1.00000000 -0.7124406  0.09120476
# wt    0.8879799  0.6587479 -0.71244065  1.0000000 -0.17471588
# qsec -0.4336979 -0.7082234  0.09120476 -0.1747159  1.00000000
# disp와 wt가 0.887로 상관성이 높다.

# 모델 생성 후 값 추정
######################################################
base_model <- lm(mpg~., data=mtcars_num)
summary(base_model)
predict(base_model, data.frame(disp=170, hp=120, drat=4.0, wt=2.7, qsec=17.0))

# vif를 이용해 다중공선성진단
######################################################
library(car)
vif(base_model)
# disp       hp     drat       wt     qsec 
# 9.110869 5.201833 2.322343 7.012686 3.191939 
vif(base_model) > 5

# stepwise selection 방법으로 변수 선택
######################################################
out <- step(base_model, direction="backward")
# Step:  AIC=63.89
# mpg ~ drat + wt + qsec # 현재 포함된 변수
# 
#        Df Sum of Sq    RSS    AIC
# <none>              183.52 63.891 # 현재 포함된 변수를 그대로 뒀을 경우
# - drat  1    11.942 195.46 63.908
# + hp    1     9.418 174.10 64.205
# + disp  1     1.506 182.02 65.627 # + 설명 변수 추가했을 경우
# - qsec  1    85.720 269.24 74.156 # - 설명 변수 제거했을 경우
# - wt    1   275.686 459.21 91.241
summary(out)
formula(out)
# mpg ~ drat + wt + qsec # 세 변수가 선택 되었다.

# 선택 변수만으로 모델 재생성
######################################################
selected_model <- lm(formula(out), data=mtcars_num)
summary(selected_model) # summary(out)과 동일한 결과

# 비교를 위해 상관성이 높은 변수만으로 모델 재생성
######################################################
selected_model <- lm(mpg~wt+disp, data=mtcars_num)
summary(selected_model)
# Call:
#   lm(formula = mpg ~ wt + hp + disp, data = mtcars_num)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3.891 -1.640 -0.172  1.061  5.861 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.105505   2.110815  17.579  < 2e-16 ***
#   wt          -3.800891   1.066191  -3.565  0.00133 ** 
#   hp          -0.031157   0.011436  -2.724  0.01097 *  
#   disp        -0.000937   0.010350  -0.091  0.92851    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.639 on 28 degrees of freedom
# Multiple R-squared:  0.8268,  Adjusted R-squared:  0.8083 # R^2값이 작아져 설명력이 떨어진다.
# F-statistic: 44.57 on 3 and 28 DF,  p-value: 8.65e-11

par(mfrow=c(2,2))
plot(selected_model)
