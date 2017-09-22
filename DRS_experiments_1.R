setwd("C:/Users/lssang/Desktop/Workspace/preprocessing_in_R")
library(C50)

########## setting 1 ####################

#cleveland.csv #Y = Num
rpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/imputation/cleveland.csv")
wpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/imputation/cleveland_new.csv")
train.end = 242
test.start = 243
test.end = 303
att.classnum = -14

#dermatology.csv #Y = Class
rpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/dermatology.csv")
wpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/dermatology_new.csv")
train.end = 292
test.start = 293
test.end = 366
att.classnum = -35

#wisconsin.csv #Y = Class
rpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/wisconsin.csv")
wpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/wisconsin_new.csv")
train.end = 559
test.start = 560
test.end = 699
att.classnum = -10

#mammographic.csv #Y = Severity
rpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/mammographic.csv")
wpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/mammographic_new.csv")
train.end = 768
test.start = 769
test.end = 961
att.classnum = -6

#marketing.csv #Y = Income
rpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/marketing.csv")
wpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/marketing_new.csv")
train.end = 7194
test.start = 7195
test.end = 8993
att.classnum = -14

#mushroom.csv #Y = Class
rpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/imputation/mushroom.csv")
wpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/imputation/mushroom_new.csv")
train.end = 6499
test.start = 6500
test.end = 8124
att.classnum = -23

#hepatitis.csv #Y = Class
rpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/hepatitis.csv")
wpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/hepatitis_new.csv")
train.end = 124
test.start = 125
test.end = 155
att.classnum = -20

#horsecolic.csv
rpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/horsecolic.csv")
wpp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/horsecolic_new.csv")
train.end = 294
test.start = 295
test.end = 368
att.classnum = -24


##########################################
View(rpp_data)
View(wpp_data)
View(r_train)
View(r_test)
View(w_train)
View(w_test)
##########################################

#preprocessing in R - replacement wiht mean value
for(i in 1:ncol(rpp_data)){
  rpp_data[is.na(rpp_data[,i]), i] <- mean(rpp_data[,i], na.rm = TRUE)
}

#preprocessing in R - deleate missing value
rpp_dataa <- na.omit(rpp_data)

R_TEST_MSE = 0;
W_TEST_MSE = 0;
N = 100;

for(i in 1 : N){

  
    #suffle
    rpp_data = rpp_data[sample(nrow(rpp_data)),]
    wpp_data = wpp_data[sample(nrow(wpp_data)),]
    
    #R- set train/test
    r_train <- rpp_data[1:train.end,]
    r_test <- rpp_data[test.start:test.end,]
    
    #DRS - set train/test
    w_train <- wpp_data[1:train.end,]
    w_test <- wpp_data[test.start:test.end,]
    
    #R - learn classification model 
    r_model = C5.0(r_train[att.classnum], factor(r_train$Surgical_lesion.))
    #summary(r_model)
    
    #DRS - learn classification model
    w_model = C5.0(w_train[att.classnum],factor(w_train$Surgical_lesion.))
    #summary(w_model)
    
    #R - predict with test MSE
    r_pred = predict(r_model, r_test)
    r.table = table(r_pred, r_test$Surgical_lesion.)
    
    #DRS - predict with test MSE
    w_pred = predict(w_model, w_test)
    w.table = table(w_pred, w_test$Surgical_lesion.)
    
    #R - caluate TEST MSE
    t = 0
    f = 0
    n = ncol(r.table)
    for(j in 1:n){
      for(k in 1:n){
        if(j == k ){
          t = t + r.table[j,k]
        }else{
          f = f + r.table[j,k]
        }
      } 
    }
    res = (f/(t+f))*100
    R_TEST_MSE = R_TEST_MSE + res
    
    
    #DRS - caluate TEST MSE
    t = 0
    f = 0
    n = ncol(w.table)
    for(j in 1:n){
      for(k in 1:n){
        if(j == k ){
          t = t + w.table[j,k]
        }else{
          f = f + w.table[j,k]
        }
      } 
    }
    res = (f/(t+f))*100
    W_TEST_MSE = W_TEST_MSE + res
  
    print(i)
}

print(R_TEST_MSE/N)
print(W_TEST_MSE/N)

