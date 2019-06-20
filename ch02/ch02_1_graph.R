# 2장
setwd("D:/Workspace/R-Statistics/ch02")
data <- read.csv('2010년 인구사항.csv', header = F, na.strings = c("."))
str(data)
data$V1 <- factor(data$V1, levels = c(1, 2),
                  labels = c('남자', '여자'))
data$V3 <- factor(data$V3, levels = 1:14,
                  labels = c('가구주', '가구주의 배우자', '자녀',
                             '자녀의 배우자', '가구주의 부모',
                             '배우자의 부모', '손자녀, 그 배우자',
                             '증손자녀, 그 배우자', '조부모',
                             '형제자매, 그 배우자',
                             '형제자매의 자녀, 그 배우자',
                             '부모의 형제자매, 그 배우자', '기타 친인척',
                             '그외같이사는사람'))
data$V4 <- factor(data$V4, levels = 1:8,
                  labels = c("안 받았음", '초등학교', '중학교',
                             '고등학교', '대학-4년제 미만', '대학-4년제 이상',
                             '석사과정', '박사과정'))
str(data)
head(data)

# 1. 그래프
library(ggplot2)
library(dplyr)
str(cars)
head(cars)

# 예제 2-1 두 변수 간의 관계를 나타내는 산점도
par(mfrow = c(1, 2))
plot(cars$speed, cars$dist,
     main = '속도와 제동거리', xlab = '속도(mph)', ylab = '제동거리(ft)',
     pch = 1, col = 'red')
plot(jitter(cars$speed), jitter(cars$dist),
     main = '속도와 제동거리', xlab = '속도(mph)', ylab = '제동거리(ft)',
     pch = 1, col = 'red')
par(mfrow = c(1, 1))

# ggplot으로 그리기
library(extrafont)
windowsFonts()
font_import(pattern = 'malgun')
loadfonts()
fonttable()

ggplot(cars, aes(cars$speed, cars$dist)) +
  geom_point(size = 3, color = 'red') +
  ggtitle('속도와 제동거리') +
  labs(x = '속도(mph)', y = '제동거리(ft)') +
  theme(plot.title = element_text(family = "malgun", face="bold", size=20, vjust=1, hjust = 0.5, color="darkblue"))

# 시계열 그래프

Nile
str(Nile)

plot(Nile,
     main = 'Nile강의 연도별 유량 변화', xlab = '연도', ylab = '유량')
plot(Nile, type = 'p',
     main = 'Nile강의 연도별 유량 변화', xlab = '연도', ylab = '유량')
plot.ts(Nile) # 시계열 그래프 함수

# Time series를 Dataframe으로 변환
df_Nile1 <- data.frame(Nile)
df_Nile1 <- df_Nile1 %>%
  mutate(year = c(1871:1970))

df_Nile1 <- df_Nile1[c(2,1)] # 열 순서 변경
head(df_Nile1)

df_Nile2 <- data.frame(year = time(Nile),
                       flow = as.matrix(Nile))
head(df_Nile2)

# ggplot으로 그리기
ggplot(df_Nile1, aes(x = year, y = Nile)) +
  geom_line(size = 1, color = 'red') +
  ggtitle('Nile강의 연도별 유량 변화') +
  labs(x = '연도', y = '유량') +
  theme(plot.title = element_text(family = "malgun", face="bold", size=20, vjust=2, hjust = 0.5, color="darkblue")) +
  theme(axis.title = element_text(family = "malgun", face="bold", size=15))


# 예제 2-2 막대그래프와 히스토그램
tableV5 <- table(data$V5)
tableV5

# 막대 그래프
barplot(tableV5,
        main = '출생아(남자)별 빈도', xlab = '출생아수', ylab = '빈도')

# 막대 그래프 ggplot
df_tableV5 <- as.data.frame(tableV5)

str(df_tableV5)
ggplot(df_tableV5, aes(Var1, Freq, fill = Var1)) + 
  geom_col() +
  ggtitle('출생아(남자)별 빈도') +
  labs(x = '출생아수', y = '빈도') +
  theme(plot.title = element_text(family = "malgun", face="bold", size=20, vjust=2, hjust = 0.5, color="darkblue")) +
  theme(axis.title = element_text(family = "malgun", face="bold", size=15))

# 히스토그램
hist(data$V2, breaks = c(seq(0, 90, 10)), right = F,
     main = '연령별 분포', xlab = '연령', ylab = '빈도')

# 히스토그램 ggplot
df_dataV2 <- as.data.frame(data$V2)
str(df_dataV2)
head(df_dataV2)

library(gridExtra)
hist(data$V2, breaks = c(seq(0, 90, 10)), right = F,
     main = '연령별 분포', xlab = '연령', ylab = '빈도')

ggplot(df_dataV2_1, aes(data$V2)) +
  geom_histogram(fill = '#F8766D', color = 'black', binwidth = 10) +
  ggtitle('연령별 분포') +
  labs(x = '연령', y = '빈도') +
  scale_x_continuous(breaks = c(seq(0, 90, 20))) +
  theme(plot.title = element_text(family = "malgun", face="bold", size=20, vjust=2, hjust = 0.5, color="darkblue")) +
  theme(axis.title = element_text(family = "malgun", face="bold", size=15))

# 예제 2-3 원도표
library(RColorBrewer)

pie(table(data$V4), main = '학력수준별 비중', cex = 0.8)
table(data$V4)

df_dataV4 <- as.data.frame(table(data$V4))
df_dataV4$Var1 <- droplevels(df_dataV4$Var1)
str(df_dataV4)
head(df_dataV4)
summary(data)
options(digits = 1)

df_dataV4 <- df_dataV4 %>%
  mutate(pct = Freq / sum(Freq) * 100) %>%
  mutate(ylabel = paste(Var1, '\n', '(', sprintf("%4.1f", pct), '%', ')', sep = '' )) %>%
  mutate(ypos = cumsum(pct) - 0.5 * pct) %>%
  arrange((Freq))

df_dataV4$Var1 <- factor(df_dataV4$Var1, levels = c('고등학교', '대학-4년제 이상', '초등학교', '중학교', '안 받았음', '대학-4년제 미만', '석사과정', '박사과정'), ordered = T)

myPal <- brewer.pal(8, 'Dark2')
brewer.pal.info
display.brewer.all()
ggplot(df_dataV4, aes(x = '', y = pct, fill = Var1)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(y = ypos, label = ylabel), color = 'black') +
  coord_polar(theta = 'y') +
  scale_fill_brewer(palette = 'Set2')

