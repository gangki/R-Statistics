### 모비율 추정 ###

# 문제 1

xbar <- 1/2
sigma <- 1/2
n <- 196 # 2 * 1.96 * (1/2)/sqrt(n) = 0.14

ll <- xbar - ((1.96 * sigma) / sqrt(n))
Ul <- xbar + ((1.96 * sigma) / sqrt(n))

# 정답 : 0.43 <= p <= 0.57


# 문제 2

xbar <- 4/5
sigma <- 4/5
n <- 100

ll <- xbar - ((1.96 * sigma) / sqrt(n))
Ul <- xbar + ((1.96 * sigma) / sqrt(n))

# 정답 : 0.6432 <= p <= 0.9568

# 문제 3

xbar <- 430/1000
sigma <- 430/1000
n <- 1000

ll <- xbar - ((1.64 * sigma) / sqrt(n))
Ul <- xbar + ((1.64 * sigma) / sqrt(n))
# 정답 : 0.241 <= p <= 0.759



### 1-Sample T 테스트 ###

# 문제 1

battery <- data.frame(life = c(980, 1008, 968, 1032, 1012, 1002, 996, 1021, 1002, 996, 1017))

barx <- mean(battery$life) # 표본평균
s <- sd(battery$life)      # 표본편차
n <- length(battery$life)  # 표본개수       
h0 <- 1000 #영가설 평균

t.t <- (barx - h0) / (s / sqrt(n)) # 검정통계량 (0.562)
alpha <- 0.025 # 유의수준 (양쪽검정)
c.u <- qt(1-alpha, df = n-1) # 임계값 (2.228)
p.value <- 1 - pt(t.t, df = n-1) # 유의확률 0.293
p.value

t.test(battery$life, mu = 1000, alternative = 'two.sided')

# 양쪽 검정으로 p-value값(0.293)이 유의수준 0.025보다 크므로 
# 영가설 채택. 즉, 샘플이 모집단과 같다고 할 수 있다.


# 문제 2

point <- data.frame(math = c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39))

barx <- mean(point$math)
s <- sd(point$math)
n <- length(point$math)              
h0 <- 55

t.t <- (barx - h0) / (s / sqrt(n)) # 검정통계량 (0.245)
alpha <- 0.05 # 유의수준
c.u <- qt(1-alpha, df = n-1) # 임계값 (1.746)
p.value <- 1 - pt(t.t, df = n-1) # 유의확률 (0.405)
p.value

t.test(point$math, mu = 55, alternative = 'greater')

# (오른쪽)한쪽 검정으로 p-value값(0.405)이 유의수준 0.05보다 크므로 
# 영가설 채택. 즉, 0교시 수업을 시행한 후, 학생들의 성적은 올랐다고 할 수 있다.

# 문제 3

alchol <- data.frame(gram = c(15.50, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0, 4.97))

barx <- mean(alchol$gram) # 표본평균
s <- sd(alchol$gram)      # 표본편차
n <- length(alchol$gram)  # 표본개수       
h0 <- 8.1 #영가설 평균

t.t <- (barx - h0) / (s / sqrt(n)) # 검정통계량 (0.653)
alpha <- 0.025 # 유의수준 (양쪽검정)
c.u <- qt(1-alpha, df = n-1) # 임계값 (2.262)
p.value <- 1 - pt(t.t, df = n-1) # 유의확률 0.265
p.value

t.test(alchol$gram, mu = 8.1, alternative = 'two.sided')

# 양쪽 검정으로 p-value값(0.265)이 유의수준 0.025보다 크므로 
# 영가설 채택. 즉, 평균 알코올 섭취량이 달라졌다고 할 수 있다.


