setwd("C:/Users/lssang/Desktop/Workspace/preprocessing_in_R")
library(C50)

#### Y = OVERALL_DIAGNOSIS
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/spectfheart.csv")

#### Y = Class
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/wdbc.csv")

#### Y = Class
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/thyroid.csv")

#### Y = Class
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/splice.csv")

#### Y = Class
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/marketing_miss.csv")

#### Y = Class
r.pp_data <- read.csv("C:/Users/lssang/Desktop/Workspace/data/featureselection/horsecolic_miss.csv")