# 2. 모수와 통계량
# 라니의 카페

ranicafe <- read.csv('cafedata.csv', stringsAsFactors = F)
str(ranicafe)
head(ranicafe)
summary(ranicafe)
dim(ranicafe)

ranicafe$Coffees <- as.numeric(ranicafe$Coffees)
sort(ranicafe$Coffees)
sort(ranicafe$Coffees)[1] # 최솟값
sort(ranicafe$Coffees,decreasing = T)[1] # 최댓값
min(ranicafe$Coffees, na.rm = T)
max(ranicafe$Coffees, na.rm = T)

stem(ranicafe$Coffees) # 최빈값
hist(ranicafe$Coffees)

rc <- ranicafe$Coffees
weight <- (1 / (length(rc)-1)) # Na 갯수를 빼주어야 함
sum(rc * weight, na.rm = T) # 평균
mean(ranicafe$Coffees, na.rm = T)

rc[rc == max(rc, na.rm = T)] <- 480
mean(rc, na.rm = T)

median.idx <- (1+length(rc)-1) / 2 #홀수 일때
sort(rc)[median.idx] # 중앙값
median(rc, na.rm = T) # 중앙값

# 표준편차 구하기
height <- c(164, 166, 168, 170, 172, 174, 176)
height.m <- mean(height)
height.dev <- height - height.m
sum(height.dev)

height.dev2 <- height.dev^2 
sum(height.dev2) 
variance <- mean(height.dev2) # 분산
standard_dev <- sqrt(variance) # 표준편차

mean(height)
var(height) # R은 표본집단으로 계산
sd(height) # R은 표본집단으로 계산

# 사분위수 구하기
quantile(rc, na.rm = T)
qs <- quantile(rc, na.rm = T)
qs[4] - qs[2] # 3분위수 - 1분위수 : IQR (InterQuantile Range)
IQR(rc, na.rm = T)
bp <- boxplot(rc, main = '커피 판매량에 대한 상자도표')

# 이상치 판별(outlier)
bp <- boxplot(cars$dist)
qs <- quantile(cars$dist)
iqr <- qs[4] - qs[2]
iqr
upperLimit <- qs[4] + 1.5 * iqr
lowerLimit <- qs[2] - 1.5 * iqr
cars$dist[cars$dist > upperLimit] # 이상치
cars$dist[cars$dist < lowerLimit] # 이상치
