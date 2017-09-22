#MSE 2-10

setwd("C:/Users/lssang/Desktop/datamining")
library(splines)

par(mfrow=c(1,2))
K = c(30, 70)
u = seq(2,97,by=.1)
B = data.frame(pred=u)

train_2_10 <- read.csv("train_2_10.csv", header=T, dec=".",sep=",")
test_2_10 <- read.csv("test_2_10.csv", header=T, dec=".",sep=",")

plot(train_2_10)

#plot(test2_10$pred, test2_10$resp, col="red")
m.2_10=lm(resp~pred, train_2_10)
abline(m.2_10, lwd=1, col="orange")

m.sp1_2_10 = lm(resp~bs(pred, knots=c(K), degree=2), data=train_2_10)
Y1_2_10 = predict(m.sp1_2_10, newdata=B)
lines(u, Y1_2_10, lwd=1, col="blue")

m.sp2_2_10 = lm(resp~bs(pred, knots=c(K), degree=30), data=train_2_10)
Y2_2_10 = predict(m.sp2_2_10, newdata=B)
lines(u, Y2_2_10, lwd=1, col="green")

train_mses = c()
test_mses = c()

for(i in 2:25){
  print(i)
  m = lm(resp~bs(pred, knots=c(K), degree=i), data=train_2_10)
  Y = predict(m, newdata=B)
  
  train_mse = mean(m$residuals^2)
  train_mses = append(train_mses, train_mse)
  
  test_mse = mean((test_2_10$resp - predict.lm(m, test_2_10))^2)
  test_mses = append(test_mses, test_mse)
}

mse = append(train_mses, test_mses)
plot(mse)
lines(train_mses, lwd=1, col="blue")
lines(test_mses, lwd=1, col="red")

