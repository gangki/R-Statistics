### 8. 2-Sample T 테스트

# 1. mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가
# 통계적으로 유의한지 t-test를 통해 확인해 보시오.

library(ggplot2)
str(mtcars)

# 데이터 전처리
data_0 <- subset(mtcars, am == 0, select = c(mpg, am)) # 0은 자동
data_1 <- subset(mtcars, am == 1, select = c(mpg, am)) # 1은 수동

# 정규성 테스트
shapiro.test(data_0$mpg) # p-value(0.8987) > 0.05, 정규성 있음
shapiro.test(data_1$mpg) # p-value(0.5363) > 0.05, 정규성 있음

# 등분산성 테스트
var.test(mtcars$mpg ~ mtcars$am) # p-value(0.067) > 0.05,  등분산성 따름

# Two Sample t-test
t.test(mtcars$mpg ~ mtcars$am, 
       mu = 0, alternative = 'less', var.equal = T) # p-value(0.01425)

####정답####

# t.test결과 p-value값이 0.0001425로 유의수준 0.05보다 작으므로 영가설 기각.
# 즉, 자동의 mpg 평균은 수동의 mpg 평균보다 작다

# 영가설 : 자동과 수동의 mpg의 평균은 서로 같다.
# 대안가설 : 자동의 mpg 평균은 수동의 mpg의 평균보다 작다.

# 2. MASS 패키지에 내장된 Cars93 데이터프레임에 대해서 생산국(Origin)이
# USA vs. non-USA 2개의 group 에 대해서 차 가격(Price)의 평균이 차이가 있는지를 검정해보시오.
library(MASS)
str(Cars93)
View(Cars93)

# 데이터 전처리
USA <- subset(Cars93, Origin == 'USA', select = c(Price, Origin))
non_USA <- subset(Cars93, Origin == 'non-USA', select = c(Price, Origin))


# 정규성 테스트
shapiro.test(USA$Price) # p-value(0.0002006) < 0.05, 정규성 없음, / 단, 샘플수(48) >= 30, 정규성 띤다고 가정
shapiro.test(non_USA$Price) # p-value(0.0002036) < 0.05, 정규성 없음, 단 샘플수(45) >= 30, 정규성을 띤다고 가정

nrow(USA)
nrow(non_USA)

# 등분산성 테스트
var.test(Cars93$Price ~ Cars93$Origin) # p-value(0.01387) < 0.05,  등분산성을 따르지 못함

# Two Sample t-test
t.test(Cars93$Price ~ Cars93$Origin, 
       mu = 0, alternative = 'less', var.equal = F) # p-value(0.1714)

####정답####
# t.test결과 p-value값이 0.1714로 유의수준 0.05보다 크므로 영가설 채택.
# 즉, USA의 가격 평균이 non-USA의 가격 평균보다 작다.

# 영가설 : USA와 non-USA의 Price의 평균은 서로 같다.
# 대안가설 : USA의 가격 평균이 non-USA의 가격 평균보다 작다.

# 3. mpg 데이터셋에서 다음을 검정해 보시오. 
# 1) subcompact 자동차와 midsize 자동차의 고속도로 연비
library(dplyr)
str(mpg)

data <- mpg %>%
  filter(class %in% c('subcompact', 'midsize')) %>%
  select('hwy', 'class')

df_data <- as.data.frame(data)

df_data_mid <- df_data %>%
  filter(class == 'midsize')
df_data_sub <- df_data %>%
  filter(class == 'subcompact')

# 정규성 테스트
shapiro.test(df_data_mid$hwy) # p-value(0.01311) < 0.05, 정규성 없음, / 단, 샘플수(41) >= 30, 정규성 따른다고 가정
shapiro.test(df_data_sub$hwy) # p-value(0.01036) < 0.05, 정규성 없음, / 단, 샘플수(35) >= 30, 정규성을 따른다고 가정

# 등분산성 테스트
var.test(df_data$hwy ~ df_data$class) # p-value(8.825e-08) < 0.05,  등분산성을 따르지 않음

# Two Sample t-test
t.test(df_data$hwy ~ df_data$class, 
       mu = 0, alternative = 'less', var.equal = F) # p-value(0.1923)
####정답####
# t.test결과 p-value값이 0.1923로 유의수준 0.05보다 크므로 영가설 채택.
# 즉, midsize와 subcompact의 hwy의 평균은 서로 동일하다.

# 영가설 : midsize와 subcompact의 hwy의 평균은 서로 동일하다.
# 대안가설 : midsize의 hwy 평균이 subcompact의 hwy 평균보다 작다.


# 2) 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비 
str(mpg)
table(mpg$fl)

data2 <- mpg %>%
  filter(fl %in% c('p', 'r')) %>%
  select('hwy', 'fl')

df_data2 <- as.data.frame(data2)

df_data2_p <- df_data2 %>%
  filter(fl == 'p')
df_data2_r <- df_data2 %>%
  filter(fl == 'r')

# 정규성 테스트
shapiro.test(df_data2_p$hwy) # p-value(8.341e-06) < 0.05, 정규성 없음, / 단, 샘플수(52) >= 30, 정규성 따른다고 가정
shapiro.test(df_data2_r$hwy) # p-value(7.603e-07) < 0.05, 정규성 없음, / 단, 샘플수(168) >= 30, 정규성을 따른다고 가정

# 등분산성 테스트
var.test(df_data2$hwy ~ df_data2$fl) # p-value(0.006139) < 0.05,  등분산성을 따르지 않음

# Two Sample t-test
t.test(df_data2$hwy ~ df_data2$fl, 
       mu = 0, alternative = 'greater', var.equal = F) # p-value(0.0007882)
####정답####
# t.test결과 p-value값이 0.0007882로 유의수준 0.05보다 작으므로 영가설 기각.
# 즉, 고급 휘발유(p)의 hwy평균이 일반 휘발유(r)의 hwy의 평균보다 크다.

# 영가설 : 고급 휘발유(p)와 일반 휘발유(r)의 hwy의 평균은 서로 동일하다.
# 대안가설 : 고급 휘발유(p)의 hwy평균이 일반 휘발유(r)의 hwy의 평균보다 크다. 

# 3) subcompact 자동차의 전륜구동(f)이냐 후륜구동(r)이냐에 따른 도시 연비
str(mpg)
table(mpg$drv)

data3 <- mpg %>%
  filter(drv %in% c('f', 'r')) %>%
  select('hwy', 'drv')

df_data3 <- as.data.frame(data3)

df_data3_f <- df_data3 %>%
  filter(drv == 'f')
df_data3_r <- df_data3 %>%
  filter(drv == 'r')

# 정규성 테스트
shapiro.test(df_data3_f$hwy) # p-value(1.556e-06) < 0.05, 정규성 없음, / 단, 샘플수(106) >= 30, 정규성 따른다고 가정
shapiro.test(df_data3_r$hwy) # p-value(0.03317) < 0.05, 정규성 없음, / 단, 샘플수(25) >= 30, 정규성을 따른다고 가정???

# 등분산성 테스트
var.test(df_data3$hwy ~ df_data3$drv) # p-value(0.4418) > 0.05,  등분산성을 따름

# Two Sample t-test
t.test(df_data3$hwy ~ df_data3$drv, 
       mu = 0, alternative = 'greater', var.equal = F) # p-value(6.992e-11)
####정답####
# t.test결과 p-value값이 6.992e-11로 유의수준 0.05보다 작으므로 영가설 기각.
# 즉, 전륜구동(f)의 hwy평균이 후륜구동(r)의 hwy의 평균보다 크다.

# 영가설 : 전륜구동(f)와 후륜구동(r)의 hwy의 평균은 서로 동일하다.
# 대안가설 : 전륜구동(f)의 hwy평균이 후륜구동(r)의 hwy의 평균보다 크다.



### 9. Paired Sample T 테스트
# 1. 새로운 당뇨병 치료제를 개발한 제약사에서는 치료에 지대한 영향을 주는 외부요인을
# 통제하기 위해 10명의 당뇨병 환자를 선별하여 1달 동안 '위약(placebo)'을 투여한 기간의
# 혈당 수치(Xi)와 '신약(new medicine)'을 투여한 1달 기간 동안의 혈당 수치(Yi)를 측정하여 
# 짝을 이루어 혈당 차이를 유의수준 5%에서 비교하시오.

xi <- c(51.4, 52.0, 45.5, 54.5, 52.3, 50.9, 52.7, 50.3, 53.8, 53.1)
yi <- c(50.1, 51.5, 45.9, 53.1, 51.8, 50.3, 52.0, 49.9, 52.5, 53.0)
df_data <- data.frame(xi, yi)

t.test(df_data$xi, df_data$yi, paired = T, alternative = 'less')
####정답####
# t.test결과 p-value값이 0.9969로 유의수준 0.05보다 크므로 영가설 채택.
# 즉, 위약(placebo)와 신약(new medicine)의 혈당 차이는 없다. (효과 없음)

# 영가설 : 위약(placebo)와 신약(new medicine)의 혈당 차이는 없다. (효과 없음)
# 대안가설 : 위약(placebo)와 신약(new medicine)의 혈당 차이가 있다. (효과 있음)

# 2. 두 종류의 신발 밑창의 원재료가 닳는 정도가 차이가 있는지를 검정하기 위해서
# 10명의 소년에게 한쪽은 A라는 원재료로 만든 신발을 신기고, 다른 한쪽은 B라는 원재료로 만든
# 신발을 신긴 후에, 일정 기간이 지난후에 신발을 수거하여 10명의 각 소년의 
# 왼쪽 신발 밑창의 닳은 정도와 오른쪽 신발 밑창의 닳은 정도의 차이를 비교하여 
# 두 종류 원재료의 재질이 다른지를 검정하시오

material_A <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
material_B <- c(14.0, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)
df_data2 <- data.frame(material_A, material_B)

t.test(df_data2$material_A, df_data2$material_B, paired = T)
####정답####
# t.test결과 p-value값이 0.008539로 유의수준 0.05보다 작으므로 영가설 기각.
# 즉, 원재료A와 원재료B의 닳은 정도의 차이가 있다. (재질이 다름)

# 영가설 : 원재료A와 원재료B의 닳은 정도의 차이는 없다. (재질이 동일함)
# 대안가설 : 원재료A와 원재료B의 닳은 정도의 차이가 있다. (재질이 다름)

### 일원 분산분석(One way ANOVA)
# 1. 다음은 3개 호수의 산소량의 차이가 있는지 없는지 알아보기 위하여 각 호수에서 10곳을 선택하여 
# 수심 1m의 물로부터 산소량(ppm)을 측정한 자료이다. 3개 호수의 산소량이 같다고 할 수 있는가?

lake1 <- data.frame(name = rep('1', 10),
                    ppm = c(5, 7, 6, 8, 6, 7, 8, 8, 6, 10))
lake2 <- data.frame(name = rep('2', 10),
                    ppm = c(6, 8, 9, 11, 13, 12, 10, 8, 9, 10))
lake3 <- data.frame(name = rep('3', 10),
                    ppm = c(14, 25, 26, 18, 19, 22, 21, 16, 20, 30))
lake <- rbind(lake1, lake2, lake3)

str(lake)

ow <- lm(ppm ~ name, data = lake)
anova(ow)

####정답####
# t.test결과 p-value값이 5.603e-09로 유의수준 0.05보다 작으므로 영가설 기각.
# 즉, 3개 호수의 산소량이 같다고 할 수 없다.

# 영가설 : 3개 호수의 산소량이 같다
# 대안가설 : 3개 호수의 산소량이 다르다.

# 2. 다음은 3개 채소에 대한 도매시장 7곳의 가격이다.
# 3개 채소의 가격이 같다고 할 수 있는가?

vege1 <- data.frame(name = rep('A', 7),
                         price = c(15.5, 14.3, 16.3, 13.5, 15.7, 16.4, 14.7))
vege2 <- data.frame(name = rep('B', 7),
                    price = c(14.7, 163., 15.5, 15.2, 163., 13.5, 15.4))
vege3 <- data.frame(name = rep('C', 7),
                    price = c(15.5, 13.2, 16.5, 15.7, 15.3, 15.2, 14.8))
vege <- rbind(vege1, vege2, vege3)

ow <- lm(price ~ name, data = vege)
anova(ow)

####정답####
# t.test결과 p-value값이 0.1228로 유의수준 0.05보다 크므로 영가설 채택.
# 즉, 3개 채소의 가격은 같다라고 할 수 있다.

# 영가설 : 3개 채소의 가격이 같다
# 대안가설 : 3개 채소의 가격이 다르다.

### 11.적합도/독립성 검정
# 1. 어느 공정의 부적합품률은 15% 이다. 시료 80개를 추출하여 검사한 결과 불량이 16개이다.
# 유의수준 5%로 적합도 검정을 하시오

x <- c(64, 16)
chisq.test(x, p = c(68, 12)/80)

# 2. 다음은 음주량과 흡연량 데이터이다.
# 이 표로부터 음주량과 흡연량 사이에 연관이 있는지 확인하시오

ciga <- data.frame(bottle = rep('1', 107),
                   ciga = )

