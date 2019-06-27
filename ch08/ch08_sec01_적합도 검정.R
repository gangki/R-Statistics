# ch08 범주형 자료 분석
# 그림 8-1
x <- seq(0, 15, by=0.01)
dc <- dchisq(x, df=3)

alpha <- 0.05
tol <- qchisq(0.95, df=3)

par(mar=c(0,1,1,1))
plot(x, dc, type="l", axes=F, ylim=c(-0.03, 0.25), xlab="", ylab="")
abline(h=0)
tol.g <- round(tol, 2)
polygon(c(tol.g, x[x>tol.g], 15), c(0, dc[x>tol.g], 0), col="red")
text(0, -0.03, "0", cex=0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==2.14), cex=0.8)

# 예제 1. 멘델의 법칙 
x <- c(315, 101, 108, 32)
chisq.test(x, p=c(9, 3, 3, 1)/16)

# 검정통계량 그래프
x <- seq(0, 15, by=0.01)
dc <- dchisq(x, df=3)
alpha <- 0.05
tol <- qchisq(0.95, df=3)
tol1 <- qchisq(1-0.9254, df=3)

par(mar=c(0,1,1,1))
plot(x, dc, type="l", axes=F, ylim=c(-0.03, 0.25), xlab="", ylab="")
abline(h=0)
tol.g <- round(tol, 2)
polygon(c(tol.g, x[x>tol.g], 15), c(0, dc[x>tol.g], 0), col="red")
polygon(c(tol1, x[x>tol1], 15), c(0, dc[x>tol1], 0), col="blue", density = 20)
text(0, -0.01, "0", cex=0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==2.14), cex=0.8)
text(tol1, -0.03, expression(chi[0.93]^{2}==0.47), cex=0.8)




