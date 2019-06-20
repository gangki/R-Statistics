# 3. 확률과 확률분포
# 3-1. 확률

install.packages('prob')
library(prob)

tosscoin(1)
tosscoin(2)
rolldie(1)
rolldie(2)
urnsamples(1:3, size = 2)
urnsamples(1:3, size = 2, replace = T)
urnsamples(c(rep('R', 3), rep('B', 2)), size = 2)
tosscoin(2, makespace = T)

# 예제 3-2. 확률변수의 평균과 기댓값

x <- c(0, 1, 2)
px <- c(1/4, 2/4, 1/4)
EX <- sum(x * px)
EX
x2 <- x^2
x2
EX2 <- sum(x2 * px) # X 제곱의 기댓값
VARX <- EX2 - EX^2 # E(X^2) - E(X)^2
VARX
