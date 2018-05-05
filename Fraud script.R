a<-read.csv("sanjibout.csv")
install.packages("caTools")
library(caTools)
b = sample.split(a, SplitRatio=3/4)
library(dplyr)
b=sample_frac(a,.5)
      
write.csv(b,"deeplearning_cut.csv")

d<-read.csv("deeplearning_cut.csv")

library(caTools)

## 75% of the sample size

mut1 <- a$impact==0 
a[mut1, "target"] <-0 

a$target=as.factor(a$target)




smp_size <- floor(0.7 * nrow(a))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(a)), size = smp_size)

train <- a[train_ind, ]
test <- a[-train_ind, ]


#train_train<-train[train_ind, ]
#train_test<-train[-train_ind, ]

write.csv(train,"deeplearning_2_train.csv",sep=" ")
write.csv(test,"deeplearning_2_test.csv",sep=" ")



a<-read.csv("Sanjibout.csv")
summary(a)

a$target<-0

targetlogic <- a$impact>0

a[targetlogic, "target"] <- 1

a$target<-as.factor(a$target)

smp_size <- floor(0.7 * nrow(a))

set.seed(123)
train_ind <- sample(seq_len(nrow(a)), size = smp_size)

train <- a[train_ind, ]
test <- a[-train_ind, ]

write.csv(train,"deeplearning_2_train.csv")
write.csv(test,"deeplearning_2_test.csv")

