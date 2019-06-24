# 6장 준비

data <- read.csv('D:/Workspace/R-Statistics/ch05/2016.6th.csv', header = T)
str(data)

tmp <- subset(data, data$나이 == 7)
height.p <- tmp$X104.키


set.seed(9)
height <- height.p[sample(length(height.p), 15)]
height

pt(0.727, 14)
