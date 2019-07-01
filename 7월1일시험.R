1. 다음의 값을 R 내장 함수를 이용하여 구하시오

1) 시행횟수가 6이고 성공확률이 1/3인 이항분포에서 성공횟수가 3이 될 확률

(dbinom(3, 6, 1/3))
정답 : 0.2194787

2) 평균이 170이고 표준편차가 6인 정규분포에서 상위 20%되는 사람들의 키 범위

(qnorm(0.8, mean = 170, sd = 6))
정답 : 상위 20%에 해당하는 키는 175.0497 이상이다.


3) 자유도가 3인 카이제곱분포에서 누적확률이 95%일 때의 값
(pchisq(0.95, df = 3))
정답 :  0.1866521

4) 자유도가 2인 t-분포에서 누적확률이 0.975일 때의 값
(pt(0.975, df = 2))
정답 : 0.7838034

5) 표준정규분포에서 확률변수의 값이 1일 때의 누적확률
(pnorm(1, mean = 0, sd = 1))
정답 : 0.8413447

2. 다음의 문항이 베르누이 시행인지 판단하시오.

1) 영화관에서 줄을 기다리는 시간을 측정한다. -> No
2) 전화가 왔을 때, 전화를 한 사람이 여자인지를 측정한다. -> Yes
3) 주사위를 한 번 던졌을 때, 나오는 숫자를 체크한다. -> No
4) 동전의 앞면이 위로 향하고 있는지 체크한다. -> Yes
5) 사람의 눈이 녹색인지 체크한다. -> Yes

3. R 내장 데이터인 "iris"를 이용하여 다음을 구하시오.
- "setosa" 종 Sepal.Length의 모평균에 대한 95% 신뢰구간

str(iris)
iris_setosa <- subset(iris, Species == 'setosa', select = c(Sepal.Length))

(t.test(iris_setosa))
정답 : 신뢰구간 (4.905824, 5.106176)

4. 한 농구 선수가 자유투를 던지면 10번중에서 7번을 성공한다고 할 때
다음을 R을 이용하여 풀이하시오

1) 이 선수가 자유투를 10번 던져서 9번이상 성공할 확률을 구하시오.
(1 - pbinom(8, 10, prob = 7/10))
정답 : 0.1493083

2) 이 선수가 자유투를 10번 던질 때 5번 이상 8번 이하로 성공할 확률을 구하시오.
pbinom(8, 10, prob = 7/10) - pbinom(4, 10, prob = 7/10)
정답 : 0.8033427

5. 다음을 R을 이용하여 검정하시오
- 2006년 조사에 의하면 한국인의 1인 1일 평균 알코올 섭취량이 8.1g 이다.
  2008년 무작위로 뽑은 알코올 섭취량은 다음과 같다
  16.90, 13.21, 15.67, 9.87, 13.15, 9.98, 3.56, 14.50, 8.12, 6.97
  평균 알코올 섭취량이 달라졌다고 할 수 있는가?
    
H0 : mu = 8.1 v.s H1 : mu != 8.1
    
alchol <- c(16.90, 13.21, 15.67, 9.87, 13.15, 9.98, 3.56, 14.50, 8.12, 6.97)
(shapiro.test(alchol))
(t.test(alchol, mu = 8.1))

정답 : p-value = 0.04562 이므로 유의수준 0.05하에서 영가설을 기각한다.
즉, 평균 알콜 섭취량이 달라졌다고 할 수 있다.

6.  정규분포에서 from <= X <= to 의 확률을 구하는 함수
rangenorm(from, to, mean, sd)을 작성하고
rangenorm(-1.96, 1.96, 0, 1)의 값을 구하시오

rangenorm <- function(from, to, mean, sd) {
  result <- pnorm(to, mean = mean, sd = sd) - pnorm(from, mean = mean, sd = sd)
  return(result)
} 
(rangenorm(-1.96, 1.96, 0, 1))
정답 : 0.9500042

7. mpg 데이터셋에서 다음을 검정해 보시오
1) subcompact 자동차와 midsize 자동차의 도시 연비
library(ggplot2)
library(dplyr)
str(mpg)
mpg <- mpg
shapiro.test(mpg$hwy[mpg$class=='subcompact']) # p-value = 0.01036
shapiro.test(mpg$hwy[mpg$class=='midsize']) # p-value = 0.01311

par(mfrow = c(2,1))
qqnorm(mpg$hwy[mpg$class=="subcompact"]); qqline(mpg$hwy[mpg$class=="subcompact"])
qqnorm(mpg$hwy[mpg$class=="midsize"]); qqline(mpg$hwy[mpg$class=="midsize"])
par(mfrow = c(1,1))

mpg1 <- mpg %>%
  filter(class == c("subcompact", "midsize")) %>%
  select('hwy', 'class')

var.test(mpg1$hwy ~ mpg1$class) # p-value = 0.0004447
t.test(mpg1$hwy ~ mpg1$class, var.equal=F) # p-value = 0.7171
정답 : p-value가 0.7171이므로 영가설을 기각하지 못한다. 즉, 연비에 차이가 없다고 볼 수 있다.

2) 일반 휘발유(r)와 고급 휘발유(p)의 고속도로 연비

shapiro.test(mpg$hwy[mpg$fl=='r']) # p-value = 7.603e-07
shapiro.test(mpg$hwy[mpg$fl=='p']) # p-value = 8.341e-06

par(mfrow = c(2,1))
qqnorm(mpg$hwy[mpg$fl=='r']); qqline(mpg$hwy[mpg$fl=='r'])
qqnorm(mpg$hwy[mpg$fl=='p']); qqline(mpg$hwy[mpg$fl=='p'])
par(mfrow = c(1,1))

mpg2 <- mpg %>%
  filter(fl == c("r", "p")) %>%
  select('hwy', 'fl')

var.test(mpg2$hwy ~ mpg2$fl) # p-value = 0.1987
t.test(mpg2$hwy ~ mpg2$fl, var.equal=T) # p-value = 0.03238
정답 : p-value가 0.03238이므로 영가설을 기각한다. 즉, 연비에 차이가 있다고 볼 수 있다.

8. 다음을 R을 이용하여 적합도를 검정하시오.

obs <- c(322, 109, 99, 29)
null.probs <- c(9/16, 3/16, 3/16, 1/16)
chisq.test(obs, p = null.probs)

정답 : p-value = 0.6413이므로 영가설을 기각하지 못한다.
      즉 멘델의 유전법칙이 작용한다고 볼 수 있다.

9. R 내장 데이터인 "Women"을 이용하여 다음을 구하시오
 - 키(Heigth)와 몸무게(Weight)의 곡선회귀분석을 통한 회귀식(단, 2차식으로 구할 것)
str(women)

fit <- lm(weight ~ height, data = women)
summary(fit)

fit2=lm(weight ~ height + I(height^2), data = women)
summary(fit2)

정답 : weight = 261.878 − 7.348 × height + 0.0831 × height^2