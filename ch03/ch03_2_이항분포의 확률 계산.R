# 3-2. 이항분포의 확률 계산

n <- 6
p <- 1/3
x <- 0:n

# d : 확률 / p : 누적확률 / q : 누적확률 -> x / r : 난수 생성

dbinom(2, size = n, prob = p)
dbinom(4, size = n, prob = p)
px <- dbinom(x, size = n, prob = p)
px

plot(x, px, type = 's', xlab = '성공 횟수(x)', ylab = '확률(P[X=x]',
     main = 'B(6, 1/3)')
plot(x, px, type = 'h', xlab = '성공 횟수(x)', ylab = '확률(P[X=x]',
     main = 'B(6, 1/3)', lwd = 10, col = 'red')


pbinom(2, size = n, prob = p)
sum(px[1:3])

pbinom(4, n, p)
sum(px[1:5])

pbinom(4, n, p) - pbinom(2, n, p)
dbinom(3, n, p) + dbinom(4, n, p)

qbinom(0.1, n, p)
qbinom(0.5, n, p)

rbinom(10, n, p)

# 예제 3-4. R의 분포 함수를이용한 기댓값과 분산
n <- 6
p <- 1/3
x <- 0:n
px <- dbinom(x, n, p)

ex = sum(x * px) # Expectation value
ex2 = sum(x^2 * px)
varx <- ex2 - ex^2 # Variance

n * p           # 이항분포의 기댓값
n * p * (1-p)   # 이항분포의 분산 : np(1-p)

# 예제 3-5 R을 이용한 정규분포 계산

options(digits = 3)
mu <- 170
sigma <- 6
ll <- mu - 3*sigma # lower limit
ul <- mu + 3*sigma # upper limit

x <- seq(ll, ul, by = 0.01)
nd <- dnorm(x, mean = mu, sd = sigma)
plot(x, nd, type = "l", xlab = 'x', ylab = 'P(X=x)', lwd = 2, col = 'red')

pnorm(mu, mu, sigma)
pnorm(158, mu, sigma)
pnorm(180, mu, sigma) - pnorm(160, mu, sigma)

qnorm(0.25, mu, sigma) # 1분위수랑 같다
qnorm(0.75, mu, sigma) # 3분위수랑 같다
qnorm(0.5, mu, sigma) 

options(digits = 5)
se.seed(5)
smp <- rnorm(400, mu, sigma)
c(mean(smp), sd(smp))
hist(smp, prob = T,
     main = 'N(170, 6^2으로부터 추출한 표본의 분포(n=400)',
     xlab = '', ylab = '', col = 'white', border = 'black')

lines(x, nd, lty=2, col = 'red')

# 예제 3-6 R을 이용해 정규분포의 특징 알아보기
options(digits = 4)
mu <- 0
sigma <- 1

p0.05 <- qnorm(0.05, mu, sigma)
p0.025 <- qnorm(0.025, mu, sigma)
p0.05; p0.025

pnorm(1.645, mu, sigma) -  pnorm(-1.645, mu, sigma) # 90%
pnorm(1.96, mu, sigma) -  pnorm(-1.96, mu, sigma) # 95%

# 그림 3.17
z <- seq(-3, 3, by = 0.01)
z.p <- dnorm(z)
plot(z, z.p, axes = F, type = 'l',
     main = '표준정규분포 (95%)', ylab = '', ylim = c(-0.04, 0.4))
axis(1)

lines(c(-3, 3), c(0, 0))
points(-1.96, -0.02, pch = 17, col = 'red')
text(-1.96, -0.035, '-1.96', col = 'red')
points(1.96, -0.02, pch = 17, col = 'red')
text(1.96, -0.035, '1.96', col = 'red')

s <- seq(-1.96, 1.96, by = 0.001)
s.z <- dnorm(s, mean = 0, sd = 1)
s <- c(-1.96, s, 1.96)
s.z <- c(0, s.z, 0)
polygon(s, s.z, col = 'red', density = 10, angle = 305)
