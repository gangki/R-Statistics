# 이항 분포

# 1. 다음의 문제가 베르누이 시행인지 판단하시오.
# 1) 영화관에서 줄을 기다리는 시간을 측정한다.
# 2) 전화가 왔을 때, 전화를 한 사람이 여자인지를 측정한다.   
# 3) 주사위를 한 번 던졌을 때, 나오는 숫자를 체크한다. 
# 4) 주사위를 한 번 던졌을 때, 숫자 2가 나오는지를 체크한다.

# 정답 4번. 성공(2가 나오는 경우)과 실패(2가 안나오는 경우)의 두 가지 결과가 나타나는 확률 실험
# 정답 2번. 성공(여자), 실패(남자)인 경우 

# 2. 한 축구 선수가 페널티킥을 차면 5번 중 4번을 성공한다고 한다.
# 이 선수가 10번의 페널티킥을 차서 7번 성공할 확률을 구하시오.
n <- 10
p <- 4/5
x <- 0:10
options(digits = 3)

answer2 <- dbinom(7, size = n, prob = p)
answer2

px <- dbinom(x, size = n, prob = p)
#차트1
plot(x, px, type = 's', xlab = '성공 횟수(x)', ylab = '확률(P[X=x])',
     main = 'B(10, 4/5)')
#차트2
plot(x, px, type = 'h', xlab = '성공 횟수(x)', ylab = '확률(P[X=x])',
     main = 'B(10, 4/5)', lwd = 10, col = 'blue')
#ggplot
library(ggplot2)

df_binom <- data.frame(x, px) #데이터프레임 변환
ggplot(df_binom, aes(x, px)) +
  geom_bar(stat = 'identity', fill = 'mediumpurple', color = 'black', width = 1)
  

# 3. A라는 회사는 스마트폰의 한 부품을 만드는 회사로, 이 A사의 불량률은 5%로 알려져 있다.
# 이 회사의 제품 20개를 조사했을 때, 불량이 2개 이하로 나올 확률을 구하시오.

n <- 20
p <- 5/100
x <- 0:n
px <- dbinom(x, size = n, prob = p)

answer3 <- pbinom(2, size = n, prob = p)
answer3
sum(px[1:3])

# 4. 어떤 희귀 바이러스에 감염되었을 때, 회복할 수 있는 치료율은 20%라고 한다.
# 이 바이러스에 감염된 환자 20명을 치료했을 때, 적어도 2명 이상은 회복될 확률을 구하시오.

# 전체 확률
options(digits = 2)
n <- 20
p <- 20/100
x <- 0:n

px <- dbinom(x, size = n, prob = p)

answer4_all <- pbinom(20, size = 20, prob = p) # 1
answer4_two <- pbinom(1, size = 20, prob = p) # 2명 미만 회복 확률

answer4 <- answer4_all - answer4_two
answer4

# 5. 주사위 두 개를 던졌을 때, 눈금의 합이 6이 될 확률을 구하시오.

# 1,5 / 5,1
# 2,4 / 4,2
# 3,3
  
# 합계 6이 나올 경우의 수 : 5
# 총 경우의 수 : 36

n <- 1 # 시행횟수
p <- 5/36 # 성공확률
x <- 1
px <- dbinom(x, n, p) 
ex <- sum(x * px)
ex

# 정규 분포

# 1. A라는 전구회사에서 생산하는 전구의 수명은 800일이고 표준편차는 40일인 정규분포를 따른다고 한다.
# 이때 전구의 수명이 750일 이하일 확률을 구하시오.

options(digits = 3)
mu <- 800
sigma <- 40
x <- 750

result1 <- pnorm(x, mean = mu, sd = sigma)
result1

# 2. 어느 한 회사에 다니는 종업원들의 근무기간을 조사하였더니, 
# 평균은 11년이고 분산이 16년인 정규분포를 따른다고 한다.
mu <- 11
sigma <- sqrt(16)
# 1) 20년 이상 근무한 종업원의 비율을 구하시오.
result2 <- 1 - pnorm(20, mean = mu, sd = sigma)
result2
# 2) 근무연수가 가장 오래된 10%의 종업원은 이 회사에서 몇 년 이상 근무했다고 볼 수 있는가?
# 최소 16.1년 이상 근무
p0.9 <- qnorm(0.9, mean = mu, sd = sigma)
p0.9

# 3. 어느 고등학교 3학년 학생들의 수학성적은 평균이 70이고 표준편차가 8인 정규분포를 따른다고 한다.
# 이때 점수가 80점 이상이고 90점 이하인 학생의 비율을 구하시오.
mu <- 70
sigma <- 8
result3 <- pnorm(90, mean = mu, sd = sigma) - pnorm(80, mean = mu, sd = sigma)
result3

# 4. 확률변수 X가 평균이 1.5, 표준편차가 2인 정규분포를 따를 때,
# 실수 전체의 집합에서 정의된 함수 H(t)는 H(t) = P(t ≤ X ≤ t+1) 이다. H(0) + H(2)의 값을 구하시오.
mu <- 1.5
sigma <- 2

a <- pnorm(3, mean = mu, sd = sigma) - pnorm(2, mean = mu, sd = sigma)
answer4 = a * 2
answer4
