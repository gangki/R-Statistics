# 제 4장. 표본분포
# 4-1. 표본평균
# 예제 4-2 표본평균 xbar 의 분포

m10 <- rep(NA, 1000)
m40 <- rep(NA, 1000)

set.seed(9)
for(i in 1:1000) {
    m10[i] <- mean(rnorm(10)) 
    m40[i] <- mean(rnorm(40))
}

#표본평균의 평균과 표준편차
options(digits = 4)
c(mean(m10), sd(m10))
c(mean(m40), sd(m40))

par(mfrow = c(1,2))
hist(m10, xlim = c(-1.5, 1.5), main = 'n=10', xlab = 'x', ylab='',
     col = 'cyan', border = 'blue')
hist(m40, xlim = c(-1.5, 1.5), main = 'n=40', xlab = 'x', ylab='',
     col = 'cyan', border = 'blue')
par(mfrow = c(1,1))


