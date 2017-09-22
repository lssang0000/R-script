#refer : https://www.r-bloggers.com/implementing-the-gradient-descent-algorithm-in-r/

# �����ͼ�Ʈ mtcars�� �۾�ȯ�濡 ���δ�
# mtcars�� �Ӽ����� ����ó�� ����� �� �ִ�.
attach(mtcars)

#3�� 4���� �ȷ�Ʈ�� �����Ѵ�(�����)
par(mfrow = c(3, 4))

#����ϰ��� �Լ�(��������, ���亯��, �н����� , ��ȯ �Ӱ�ġ ,�ִ� �ݺ�Ƚ��)
gradientDesc <- function(x, y, learn_rate, conv_threshold, n, max_iter) {
  #plot(x, y, col = "blue", pch = 20, main = "cnt = 30")
  
  #���� m�� y���� c�� �����Ѵ�.
  #runif(������ ������ ����, min, max)
  m <- runif(1, 0, 1)
  c <- runif(1, 0, 1)
  
  #y�� �������� y^�� ���Ѵ�. �� �Լ����� m�� c�� ���������ν�
  #���������� ���ϰ����ϴ� �����Լ� ���� ��
  yhat <- m * x + c
  
  #Mean square error
  MSE <- sum((y - yhat) ^ 2) / n
  
  #�ݺ��� ������
  converged = F
  
  #�ݺ��� ī����
  iterations = 0
  
  while(converged == F) {
    
    print(iterations)
    
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))
    
    #���� ���� ��� �ݿ�
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    
    
    
    #���� ����mse�� ���� ��ȯ�Ӱ�ġ���� �۰ų� ������, �� ���� ������ ������ Ż��
    if(MSE - MSE_new <= conv_threshold) {
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    
    iterations = iterations + 1
    
    #LSSANG - 5���� ���� plot �׸���
    if(iterations%%50000 == 0){
      plot(x, y, col = "blue", pch = 20)
      abline(c, m, col = "red") 
    }
    
    #lSSANG - 60���� �ݺ� �� Ż��
    if(iterations == 600000){
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    
    #�ݺ� Ƚ���� �ִ� �ݺ�Ƚ������ ũ�� Ż��
    if(iterations > max_iter) { 
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
   
  }
}


# Run the function 
gradientDesc(disp, mpg, 0.0000293, 0.001, 32, 2500000)