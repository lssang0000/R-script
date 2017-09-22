setwd("C:/Users/lssang/Desktop/Workspace/preprocessing_in_R")
library(C50)


#### Y = OVERALL_DIAGNOSIS
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/spectfheart.csv")
w.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/spectfheart_select.csv")
train.end = 213
test.start = 214
test.end = 267
r.classnum = -45
w.classnum = -11

#### Y = Class
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/wdbc.csv")
w.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/wdbc_select.csv")
train.end = 398
test.start = 399
test.end = 569
r.classnum = -31
w.classnum = -11

#### Y = Class
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/thyroid.csv")
w.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/thyroid_select.csv")
train.end = 5760
test.start = 5761
test.end = 7200
r.classnum = -22
w.classnum = -6

#### Y = Class
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/splice.csv")
w.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/splice_select.csv")
train.end = 2552
test.start = 2553
test.end = 3190
r.classnum = -61
w.classnum = -23

#### Y = Class
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/marketing_miss.csv")
w.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/marketing_miss_select.csv")
train.end = 7194
test.start = 7195
test.end = 8993
r.classnum = -14
w.classnum = -9

#### Y = Class
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/horsecolic_miss.csv")
w.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/horsecolic_miss_select.csv")
train.end = 294
test.start = 295
test.end = 368
r.classnum = -24
w.classnum = -16

#########################################
View(r.pp_data)

R.TEST_MSE = 0;
W.TEST_MSE = 0;
N = 100;
#########################################

for(i in 1 : N){
  
  #suffle
  r.pp_data = r.pp_data[sample(nrow(r.pp_data)),]
  w.pp_data = w.pp_data[sample(nrow(w.pp_data)),]
  
  #R- set train/test
  r.train <- r.pp_data[1:train.end,]
  r.test <- r.pp_data[test.start:test.end,]
  
  #DRS - set train/test
  w.train <- w.pp_data[1:train.end,]
  w.test <- w.pp_data[test.start:test.end,]
  
  #R - learn classification model 
  r.model = C5.0(r.train[r.classnum], factor(r.train$Surgical_lesion.))
  #summary(r_model)
  
  #DRS - learn classification model
  w.model = C5.0(w.train[w.classnum],factor(w.train$Surgical_lesion.))
  #summary(w_model)
  
  #R - predict with test MSE
  r.pred = predict(r.model, r.test)
  r.table = table(r.pred, r.test$Surgical_lesion.)
  
  #DRS - predict with test MSE
  w.pred = predict(w.model, w.test)
  w.table = table(w.pred, w.test$Surgical_lesion.)
  
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
  R.TEST_MSE = R.TEST_MSE + res
  
  
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
  W.TEST_MSE = W.TEST_MSE + res

  print(i)
}

print(R.TEST_MSE/N)
print(W.TEST_MSE/N)
