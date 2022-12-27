# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

library(kernlab)
set.seed(1234567890)

data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58])
tr <- spam[1:3000, ]
va <- spam[3001:3800, ]
trva <- spam[1:3800, ]
te <- spam[3801:4601, ]

by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}
# err_va
# which.min(err_va)
# seq(by,5,by)

filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0

# same training data set but this is used for predicting test data set
filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

# the training data set is changed into trva
filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

# the training date set is changed into spam
filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?
# The filter1 should be returned to the user. The main reason is the error is the lowest, which means it has high precision,
# for making prediction.

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?
# I think err1 should be returned to the user. This is because the generalization error refers to the error we get when we use the mode
# to make prediction according to new unseen data set. Only err1 is created in this way. Namely, we use traning data set to train the model
# and then use test data set to make prediction.

# 3. Implementation of SVM predictions.

# After calculation we can find through different methods, we find the same prediction.
# the blue dots in the graph are created with a linear combination. The red line is created
# with "predict" function

sv<-alphaindex(filter3)[[1]]
co<-coef(filter3)[[1]]
inte<- - b(filter3)

support_vectors <- spam[sv,-58]
num_vector <- nrow(support_vectors)
y_hat <- c()
for(i in 1:10){
  kernel <- c()
  for (j in 1:num_vector) {
    kernel_temp <- exp(sum((support_vectors[j,] - spam[i,-58])**2) * -0.05)
    kernel <- c(kernel,kernel_temp)
  }
  y_hat_temp <- sum(t(kernel) * co) + inte
  y_hat <- c(y_hat,y_hat_temp)
}

y_hat2 <- predict(filter3,spam[1:10,-58], type = "decision")
data <- data.frame(x = 1:10, y = y_hat)
data2 <- data.frame(x = 1:10, y = y_hat2)
ggplot(data = data, aes( x = x, y = y)) + geom_point(color = "blue", size = 3) + geom_line(
  data = data2, color = "red", linewidth = 1) + xlab("the index of the test data point") +
  ylab("the predicted value")


