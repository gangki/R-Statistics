# 연습문제

#1. 카이제곱분포 (chi-squared distribution)
set.seed(11)
df <- 3 # 자유도
n <- 1000

c.4.mean <- rep(NA, n)
c.16.mean <- rep(NA, n)
c.64.mean <- rep(NA, n)
c.256.mean <- rep(NA, n)

for (i in 1:n) {
  c.4.mean[i] <- mean(rchisq(4, df))
  c.16.mean[i] <- mean(rchisq(16, df))
  c.64.mean[i] <- mean(rchisq(64, df))
  c.256.mean[i] <- mean(rchisq(256, df))
}

options(digits = 4)
c(mean(c.4.mean), sd(c.4.mean))
c(mean(c.16.mean), sd(c.16.mean))
c(mean(c.64.mean), sd(c.64.mean))
c(mean(c.256.mean), sd(c.256.mean))


par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
hist(c.4.mean, prob = T, xlim = c(0, 8), main = '카이제곱분포 - 표본 크기 : 4',
     xlab = '', ylab = '', col = 'cyan', border = 'blue')
x1 <- seq(min(c.4.mean), max(c.4.mean), length = 1000)
y1 <- dnorm(x = x1, mean = df, sd = sqrt(df*2)/sqrt(4))
lines(x1, y1, lty = 2, lwd = 2, col = 'red')

hist(c.16.mean, prob = T, xlim = c(1, 5), main = '카이제곱분포 - 표본 크기 : 16',
     xlab = '', ylab = '', col = 'cyan', border = 'blue')
x2 <- seq(min(c.16.mean), max(c.16.mean), length = 1000)
y2 <- dnorm(x = x2, mean = df, sd = sqrt(df*2)/sqrt(16))
lines(x2, y2, lty = 2, lwd = 2, col = 'red')

hist(c.64.mean, prob = T, xlim = c(2, 4), main = '카이제곱분포 - 표본 크기 : 64',
     xlab = '', ylab = '', col = 'cyan', border = 'blue')
x3 <- seq(min(c.64.mean), max(c.64.mean), length = 1000)
y3 <- dnorm(x = x3, mean = df, sd = sqrt(df*2)/sqrt(64))
lines(x3, y3, lty = 2, lwd = 2, col = 'red')

hist(c.256.mean, prob = T, xlim = c(2.5, 3.5), main = '카이제곱분포 - 표본 크기 : 256',
     xlab = '', ylab = '', col = 'cyan', border = 'blue')
x4 <- seq(min(c.4.mean), max(c.4.mean), length = 1000)
y4 <- dnorm(x = x1, mean = df, sd = sqrt(df*2)/sqrt(256))
lines(x4, y4, lty = 2, lwd = 2, col = 'red')

mtext("Chi Square 표본평균 분포(df=3인 경우)", outer = TRUE, cex = 1.2)

#2. t-분포 (t-distribution)
df <- 3 # 자유도
n <- 1000

t.4.mean <- rep(NA, n)
t.8.mean <- rep(NA, n)
t.16.mean <- rep(NA, n)
t.32.mean <- rep(NA, n)

for (i in 1:n) {
  t.4.mean[i] <- mean(rt(4, df))
  t.8.mean[i] <- mean(rt(8, df))
  t.16.mean[i] <- mean(rt(16, df))
  t.32.mean[i] <- mean(rt(32, df))
}

options(digits = 4)
c(mean(t.4.mean), sd(t.4.mean))
c(mean(t.8.mean), sd(t.8.mean))
c(mean(t.16.mean), sd(t.16.mean))
c(mean(t.32.mean), sd(t.32.mean))


par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
hist(t.4.mean, prob = T, xlim = c(-3, 3), main = 't-분포 - 표본 크기 : 4',
     xlab = '', ylab = '', col = 'cyan', border = 'blue')
x1 <- seq(min(t.4.mean), max(t.4.mean), length = 1000)
y1 <- dnorm(x = x1, mean = 0, sd = sqrt(df/(df-2))/sqrt(4))
lines(x1, y1, lty = 2, lwd = 2, col = 'red')

hist(t.8.mean, prob = T, xlim = c(-1.5, 1.5), main = 't-분포 - 표본 크기 : 8',
     xlab = '', ylab = '', col = 'cyan', border = 'blue')
x2 <- seq(min(t.16.mean), max(t.16.mean), length = 1000)
y2 <- dnorm(x = x2, mean = 0, sd = sqrt(df/(df-2))/sqrt(8))
lines(x2, y2, lty = 2, lwd = 2, col = 'red')

hist(t.16.mean, prob = T, xlim = c(-1.5, 1.5), main = 't-분포 - 표본 크기 : 16',
     xlab = '', ylab = '', col = 'cyan', border = 'blue')
x3 <- seq(min(t.16.mean), max(t.16.mean), length = 1000)
y3 <- dnorm(x = x3, mean = 0, sd = sqrt(df/(df-2))/sqrt(16))
lines(x3, y3, lty = 2, lwd = 2, col = 'red')

hist(t.32.mean, prob = T, xlim = c(-1, 1), main = 't-분포 - 표본 크기 : 32',
     xlab = '', ylab = '', col = 'cyan', border = 'blue')
x4 <- seq(min(t.32.mean), max(t.32.mean), length = 1000)
y4 <- dnorm(x = x4, mean = 0, sd = sqrt(df/(df-2))/sqrt(32))
lines(x4, y4, lty = 2, lwd = 2, col = 'red')

mtext("t-distribution 표본평균 분포(df=3인 경우)", outer = TRUE, cex = 1.2)

#3. f-분포 (F-distribution)
df1 <- 10 # 자유도 k1
df2 <- 20 # 자유도 k2
n <- 1000

f.4.mean <- rep(NA, n)
f.16.mean <- rep(NA, n)
f.64.mean <- rep(NA, n)
f.256.mean <- rep(NA, n)

for (i in 1:n) {
  f.4.mean[i] <- mean(rf(4, df1, df2))
  f.16.mean[i] <- mean(rf(16, df1, df2))
  f.64.mean[i] <- mean(rf(64, df1, df2))
  f.256.mean[i] <- mean(rf(256, df1, df2))
}

options(digits = 4)
c(mean(f.4.mean), sd(f.4.mean))
c(mean(f.16.mean), sd(f.16.mean))
c(mean(f.64.mean), sd(f.64.mean))
c(mean(f.256.mean), sd(f.256.mean))


par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
hist(f.4.mean, prob = T, xlim = c(0.5, 2), main = 'f-분포 - 표본 크기 : 4',
     xlab = '', ylab='', col = 'cyan', border = 'blue')
x1 <- seq(min(f.4.mean), max(f.4.mean), length = 1000)
y1 <- dnorm(x = x1, mean = df2/(df2-2), sd = (sqrt((2*(df2^2)*(df1+df2-2))/(df1*((df2-2)^2)*(df2-4))))/sqrt(4))
lines(x1, y1, lty = 2, lwd = 2, col = 'red')

hist(f.16.mean, prob = T, xlim = c(0.5, 2), main = 'f-분포 - 표본 크기 : 16',
     xlab = '', ylab='', col = 'cyan', border = 'blue')
x2 <- seq(min(f.16.mean), max(f.16.mean), length = 1000)
y2 <- dnorm(x = x2, mean = df2/(df2-2), sd = (sqrt((2*(df2^2)*(df1+df2-2))/(df1*((df2-2)^2)*(df2-4))))/sqrt(16))
lines(x2, y2, lty = 2, lwd = 2, col = 'red')

hist(f.64.mean, prob = T, xlim = c(0.8, 1.5), main = 'f-분포 - 표본 크기 : 64',
     xlab = '', ylab='', col = 'cyan', border = 'blue')
x3 <- seq(min(f.64.mean), max(f.64.mean), length = 1000)
y3 <- dnorm(x = x3, mean = df2/(df2-2), sd = (sqrt((2*(df2^2)*(df1+df2-2))/(df1*((df2-2)^2)*(df2-4))))/sqrt(64))
lines(x3, y3, lty = 2, lwd = 2, col = 'red')

hist(f.256.mean, prob = T, xlim = c(0.95, 1.25), main = 'f-분포 - 표본 크기 : 256',
     xlab = '', ylab='', col = 'cyan', border = 'blue')
x4 <- seq(min(f.4.mean), max(f.4.mean), length = 1000)
y4 <- dnorm(x = x4, mean = df2/(df2-2), sd = (sqrt((2*(df2^2)*(df1+df2-2))/(df1*((df2-2)^2)*(df2-4))))/sqrt(256))
lines(x4, y4, lty = 2, lwd = 2, col = 'red')

mtext("F-distribution 표본평균 분포(df1=10, df2=20인 경우)", outer = TRUE, cex = 1.2)