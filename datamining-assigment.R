setwd("C:/Users/lssang/Desktop/datamining")
library(splines)

par(mfrow=c(2,2))
K = c(30, 70)
u = seq(2,97,by=.1)
B = data.frame(pred=u)

train_2_9 <- read.csv("train_2_9.csv", header=T, dec=".",sep=",")
m.ln_2_9=lm(resp~pred, train_2_9)
#attach(ds_2_9)
#ln_2_9=lm(resp~pred)
#ln_2_9
plot(train_2_9$pred, train_2_9$resp)
abline(m.ln_2_9, lwd=1, col="orange")

m.sp1_2_9 = lm(resp~bs(pred, knots=c(K), degree=2), data=train_2_9)
Y1_2_9 = predict(m.sp1_2_9, newdata=B)
lines(u, Y1_2_9, lwd=1, col="blue")

m.sp2_2_9 = lm(resp~bs(pred, knots=c(K), degree=30), data=train_2_9)
Y2_2_9 = predict(m.sp2_2_9, newdata=B)
lines(u, Y2_2_9, lwd=1, col="green")

##################

train_2_10 <- read.csv("train_2_10.csv", header=T, dec=".",sep=",")
lm.2_10=lm(resp~pred, train_2_10)
#attach(ds_2_10)
#lm.2_10=lm(resp~pred)
#lm.2_10
plot(train_2_10)
abline(lm.2_10,col="orange")

m.sp1_2_10 = lm(resp~bs(pred, knots=c(K), degree=2), data=train_2_10)
Y1_2_10 = predict(m.sp1_2_10, newdata=B)
lines(u, Y1_2_10, lwd=1, col="blue")

m.sp2_2_10 = lm(resp~bs(pred, knots=c(K), degree=30), data=train_2_10)
Y2_2_10 = predict(m.sp2_2_10, newdata=B)
lines(u, Y2_2_10, lwd=1, col="green")

##################

train_2_11 <- read.csv("train_2_11.csv", header=T, dec=".",sep=",")
lm.2_11=lm(resp~pred, train_2_11)
#attach(ds_2_11)
#lm.2_11=lm(resp~pred)
#lm.2_11
plot(train_2_11)
abline(lm.2_11,col="orange")

m.sp1_2_11 = lm(resp~bs(pred, knots=c(K), degree=2), data=train_2_11)
Y1_2_11 = predict(m.sp1_2_11, newdata=B)
lines(u, Y1_2_11, lwd=1, col="blue")

m.sp2_2_11 = lm(resp~bs(pred, knots=c(K), degree=30), data=train_2_11)
Y2_2_11 = predict(m.sp2_2_11, newdata=B)
lines(u, Y2_2_11, lwd=1, col="green")







