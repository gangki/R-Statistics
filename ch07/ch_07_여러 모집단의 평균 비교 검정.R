# ch07. 여러 모집단의 평균 비교 검정
# 7-1. 모집단이 두개인 경우

data <- read.table('D:/Workspace/R-Statistics/source/Chapter07/data/chapter7.txt', header = T)
data2 <- data.frame(gender = c(1,1), weight=c(3350, 3380, 3800, 3900))
data <- rbind(data, data2)
data$weight[7]<-3000

boy <- subset(data, gender == 1)
girl <- subset(data, gender == 2)

#정규성 테스트
shapiro.test(boy$weight) # p-value < 0.05, 정규성 없음
qqnorm(boy$weight)
qqline(boy$weight)

shapiro.test(girl$weight) # p-value > 0.05, 정규성 있음
qqnorm(girl$weight)
qqline(girl$weight)

iriss <- subset(iris, Species == 'setosa')
shapiro.test(iriss$Petal.Length) # p-value > 0.05, 정규성 있음
qqnorm(iriss$Sepal.Length)
qqline(iriss$Sepal.Length)
 
shapiro.test(iriss$Petal.Width) # p-value < 0.05, 정규성 없음
qqnorm(iriss$Petal.Width)
qqline(iriss$Petal.Width)

# 등분산성 테스트
var.test(data$weight ~ data$gender)

# Two Sample t-test
t.test(data$weight ~ data$gender,
       mu = 0, alternative = 'less', var.equal = T)

# 예제2 식욕부진증 치료요법의 효과 검정

install.packages('PaireData')
library(PairdData)
data <- read.csv('D:/Workspace/R-Statistics/source/Chapter07/data/01.anorexia.csv', header = T)
str(data)

install.packages('psych')
library(psych)
summary(data)
describe(data)

n <- length(data$Prior - data$Post)
m <- mean(data$Prior - data$Post)
s <- sd(data$Prior - data$Post)
t.t <- m/(s/sqrt(n))

alpha <- 0.05
qt(alpha, df=16)
pt(t.t, df=16)

t.test(data$Prior, data$Post, paired = T, alternative = 'less')
