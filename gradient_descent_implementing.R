#refer : https://www.r-bloggers.com/implementing-the-gradient-descent-algorithm-in-r/

# 데이터세트 mtcars를 작업환경에 붙인다
# mtcars의 속성들을 변수처럼 사용할 수 있다.
attach(mtcars)

#3행 4열의 팔레트를 형성한다(비쥬얼)
par(mfrow = c(3, 4))

#경사하강법 함수(예측변수, 응답변수, 학습비율 , 변환 임계치 ,최대 반복횟수)
gradientDesc <- function(x, y, learn_rate, conv_threshold, n, max_iter) {
  #plot(x, y, col = "blue", pch = 20, main = "cnt = 30")
  
  #기울기 m과 y절편 c를 선언한다.
  #runif(생성할 난수의 갯수, min, max)
  m <- runif(1, 0, 1)
  c <- runif(1, 0, 1)
  
  #y의 예측값인 y^을 구한다. 본 함수에서 m과 c를 조절함으로써
  #최종적으로 구하고자하는 선형함수 모델의 식
  yhat <- m * x + c
  
  #Mean square error
  MSE <- sum((y - yhat) ^ 2) / n
  
  #반복문 제어자
  converged = F
  
  #반복문 카운터
  iterations = 0
  
  while(converged == F) {
    
    print(iterations)
    
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))
    
    #기울기 조절 결과 반영
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    
    
    
    #이전 이후mse의 차가 변환임계치보다 작거나 같으면, 즉 거의 변동이 없으면 탈출
    if(MSE - MSE_new <= conv_threshold) {
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    
    iterations = iterations + 1
    
    #LSSANG - 5만번 마다 plot 그리기
    if(iterations%%50000 == 0){
      plot(x, y, col = "blue", pch = 20)
      abline(c, m, col = "red") 
    }
    
    #lSSANG - 60만번 반복 후 탈출
    if(iterations == 600000){
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    
    #반복 횟수가 최대 반복횟수보다 크면 탈출
    if(iterations > max_iter) { 
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
   
  }
}


# Run the function 
gradientDesc(disp, mpg, 0.0000293, 0.001, 32, 2500000)
