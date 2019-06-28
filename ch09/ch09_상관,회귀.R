# SECTION 01 상관계수

hf <- read.table("http://www.randomservices.org/random/data/Galton.txt", header=T, stringsAsFactors = FALSE)
str(hf)
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]
str(hf.son)

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)
cov.num <- sum((hf.son$Father-f.mean) * (hf.son$Height - s.mean))
cov.xy <- cov.num / (nrow(hf.son) - 1)
# R 함수를 이용한 공분산(표본)
cov(hf.son$Father, hf.son$Height)

r.xy <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height))
# R 함수를 이용한 표본상관계수
cor(hf.son$Father, hf.son$Height)

hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

sxy <- sum((hf.son$Father - mean.x)*(hf.son$Height - mean.y))
sxx <- sum((hf.son$Father - mean.x)^2)

( b1 <- sxy / sxx )
( b0 <- mean.y - b1 * mean.x )

# lm() 함수 이용
out <- lm(Height ~ Father, data=hf.son)
summary(out)

hf <- read.csv("http://www.math.uah.edu/stat/data/Galton.csv", header=T, stringsAsFactors = FALSE)
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]

out <- lm(Height ~ Father, data=hf.son)
out2 <- lm(dist ~ speed, data=cars)
# 그림 9-6
no <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), mar=c(2, 2, 2, 3))
plot( hf.son$Father, residuals(out), xlab="residuals", ylab="")
abline(h=0, col="red", lty=2)
plot( cars$speed, residuals(out2), xlab="residuals", ylab="" )
abline(h=0, col="red", lty=2)
par( no )
# 그림 9-7
dev.off()
no <- par(no.readonly = TRUE)
par(mfrow=c(1, 2), mar=c(2, 2, 2, 3))
qqnorm(residuals(out), main="")
qqline(residuals(out), lty=2, col="red")
qqnorm(residuals(out2), main="")
qqline(residuals(out2), lty=2, col="red")
par( no )

# 정규성 검정
shapiro.test(residuals(out2))