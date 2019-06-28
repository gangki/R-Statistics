# 12. 회귀 분석

# 1. 부모의 키가 클수록 자식의 키도 상대적으로 크다고 하는데, 
# 아버지의 키와 아들의 키를 조사하였더니 아래와 같이 나왔다고 한다.
# 이 자료를 바탕으로 해서 회귀식을 구하고 아버지의 키가 165cm일 때 
# 아들의 키는 얼마인지 예측하시오.
# 아버지의 키(x):	150  160  170  180  190	
# 아들의 키(y):	176  179  182  181  185

data1 <- data.frame(f.height = c(150, 160, 170, 180, 190),
                   s.height = c(176, 179, 182, 181, 185))
str(data1)

# 산점도
plot(data1)

# 공분산
cov(data1$f.height, data1$s.height) # 50이라는 양수이므로 두 변수 간의 상관관계는 상승하는 경향

# 상관계수
cor(data1$f.height, data1$s.height, method = 'pearson') # 0.9407209로 강한 양적 선형관계를 이루고 있음

# 상관계수 검정
cor.test(data1$f.height, data1$s.height) # p-value = 0.01717 < 0.05, 영가설 기각
# 영가설 : 상관관계가 없다.
# 대안가설 : 상관관계가 있다

# 그래프
par(mfrow=c(1, 1), mar=c(4, 4, 1, 1))
plot(data1$s.height~data1$f.height, pch=16, data=data1, xlab="아버지의 키(cm)", ylab="아들의 키(cm)")
abline(lm(s.height~f.height, data=data1), col="red", lwd=2)

# 회귀식
data1_cor <- lm(data1$s.height~data1$f.height, data=data1)
summary(data1_cor)

y <- 146.6 + (0.2*165)

# 정답 : 아버지의 키가 165cm일 때 179.6cm이다.

# 2. 소득이 높을수록 신용카드 사용량이 많아진다고 하는데, 월 소득 대비 신용카드 사용량을 
# 조사하였더니 아래와 같이 나왔다고 한다. 이 자료를 바탕으로 해서 회귀식을 구하고, 
# 월 소득이 250만원일 때 신용카드 사용량을 예측하시오. (단위: 만원) 
# 월 소득(x):	100  200  300  400  500
# 카드 사용량(y):	 30    70    85  140  197

data2 <- data.frame(inc = c(100, 200, 300, 400, 500),
                    exp = c(30, 70, 85, 140, 197))
str(data2)

# 산점도
plot(data2)

# 공분산
cov(data2$inc, data2$exp) # 10100이라는 양수이므로 두 변수 간의 상관관계는 상승하는 경향

# 상관계수
cor(data2$inc, data2$exp, method = 'pearson') # 0.9816588로 강한 양적 선형관계를 이루고 있음

# 상관계수 검정
cor.test(data2$inc, data2$exp) # p-value = 0.002974 < 0.05, 영가설 기각
# 영가설 : 상관관계가 없다.
# 대안가설 : 상관관계가 있다

# 그래프
par(mfrow=c(1, 1), mar=c(4, 4, 1, 1))
plot(exp~inc, pch=16, data=data2, xlab="월소득(만원)", ylab="카드 사용량(만원)")
abline(lm(exp~inc, data=data2), col="red", lwd=2)

# 회귀식
data2_cor <- lm(exp ~ inc, data=data2)
summary(data2_cor)
y <- -16.800 + (0.404*250)
y
# 정답 : 월 소득이 250만원일 때 신용카드 사용량은 84.2만원

# 3. mtcars 데이터셋에서 배기량(disp)에 따른 마력(hp)의 회귀식을 구하시오.
str(mtcars)

# 산점도
plot(mtcars$hp ~ mtcars$disp)

# 공분산
cov(mtcars$disp, mtcars$hp) # 6721.159이라는 양수이므로 두 변수 간의 상관관계는 상승하는 경향

# 상관계수
cor(mtcars$disp, mtcars$hp, method = 'pearson') # 0.7909486로 강한 양적 선형관계를 이루고 있음

# 상관계수 검정
cor.test(mtcars$disp, mtcars$hp) # p-value = 7.143e-08 < 0.05, 영가설 기각
# 영가설 : 상관관계가 없다.
# 대안가설 : 상관관계가 있다

# 그래프
par(mfrow=c(1, 1), mar=c(4, 4, 1, 1))
plot(hp~disp, pch=16, data=mtcars, xlab="배기량(disp)", ylab="마력(hp)")
abline(lm(hp~disp, data=mtcars), col="red", lwd=2)

# 회귀식
data3_cor <- lm(hp ~ disp, data=mtcars)
data3_cor
summary(data3_cor)

# 4. MASS 패키지를 설치하고, 이 패키지 안에 있는 Boston 데이터셋을 이용하여
# Boston 인근의 집값을 결정하는 다중회귀 모델을 만드시오.
install.packages('MASS')
install.packages("leaps")
library(leaps)
library(dplyr)
library(MASS)

data <- Boston
str(data)
glimpse(data)
summary(data)

data_lm <- lm(medv ~ . , data = data)
summary(data_lm)

training <- regsubsets(medv ~ . , data = data, )
plot(training)
