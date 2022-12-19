library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)
library(caret)

my_data = read.csv("C:/Users/yj313/Desktop/732A99-MachineLearning/lab01/Assignment2/parkinsons.csv")
set.seed(12345)
n = nrow(my_data)
id = sample(1:n, floor(n*0.6))
train = my_data[id,]
test = my_data[-id,]


scaler = preProcess(train)
trainS = predict(scaler, train)
testS = predict(scaler, test)


# train MSE
trainS_1 = trainS %>% select(motor_UPDRS, 7:22)
ml = lm(motor_UPDRS~.,trainS_1)
summary(ml)
Preds = predict(ml)
MSE_training = mean((trainS_1$motor_UPDRS - Preds)^2)

# test MSE
testS_1 = testS %>% select(motor_UPDRS, 7:22)
Preds_2 = predict(ml,testS_1)


MSE_test = mean((testS_1$motor_UPDRS - Preds_2)^2)



# The third question.

#a
# n <- nrow(trainS)
# trainS_1 <- tibble(trainS_1)
# one <- tibble(one = rep(1,n))

# trainS_1_new <- trainS_1%>%
#   select(-motor_UPDRS)%>%
#   mutate(one, .)

trainS_1_new <- trainS_1 %>%
  select(-motor_UPDRS)


likelihood <- function(xita,sigma){
  n <- nrow(trainS)
  y_hat <- as.matrix(trainS_1_new) %*% as.matrix(xita)
  difference <- trainS_1$motor_UPDRS - y_hat
  square_difference <- difference^2
  sum <- sum(square_difference)
  return((-n/2) * log(2 * pi * sigma^2 ) - (1 / (2 * sigma^2)) * sum)

}

#b
Ridge <- function(lambda, xita, sigma){
  -likelihood(xita,sigma) + lambda * sum(xita^2)
}


#c
RidgeOpt <- function(lambda){

  to_optimize <- function(x){
    x1 = x[1:16]
    x2 = x[17]
    return(Ridge(lambda, x1, x2))
  }
  optim(rep(1, times = 17), fn = to_optimize, method = "BFGS")
}



#d
DF <- function(lambda){
  X <- as.matrix(trainS_1_new)
  I <- diag(ncol(trainS_1_new))
  element_I <- diag(X %*% solve((t(X) %*% X + lambda * I)) %*% t(X))
  df <- sum(element_I)
  return(df)
}
df_1 <- DF(1)
df_100 <- DF(100)
df_1000 <- DF(1000)

#e

#when lambda equals 1, theta
theta_sigma_1 <- RidgeOpt(1)
theta_1 <- theta_sigma_1$par[1:16]

##MSE for training data
y_hat_tr_1 <- as.matrix(trainS_1_new) %*% as.matrix(theta_1)
MSE_train_1 <-mean((y_hat_tr_1 - trainS_1$motor_UPDRS)^2)

##MSE for test data
# n_2 <- nrow(testS)
# testS_1 <- tibble(testS_1)
# one_2 <- tibble(one = rep(1,n_2))

# testS_1_new <- testS_1%>%
#   select(-motor_UPDRS)%>%
#   mutate(one_2, .)

testS_1_new <- testS_1%>%
   select(-motor_UPDRS)


y_hat_test_1 <- as.matrix(testS_1_new) %*% as.matrix(theta_1)
MSE_test_1 <- mean((y_hat_test_1 - testS_1$motor_UPDRS)^2)





#when lambda equals 100, theta
theta_sigma_2 <- RidgeOpt(100)
theta_2 <- theta_sigma_2$par[1:16]


## MSE for training data

y_hat_tr_2 <- as.matrix(trainS_1_new) %*% as.matrix(theta_2)
MSE_train_2 <-mean((y_hat_tr_2 - trainS_1$motor_UPDRS)^2)

## MSE for test data
y_hat_test_2 <- as.matrix(testS_1_new) %*% as.matrix(theta_2)
MSE_test_2 <- mean((y_hat_test_2 - testS_1$motor_UPDRS)^2)




#when lambda equals 1000, theta
theta_sigma_3 <- RidgeOpt(1000)
theta_3 <- theta_sigma_3$par[1:16]

## MSE for training data

y_hat_tr_3 <- as.matrix(trainS_1_new) %*% as.matrix(theta_3)
MSE_train_3 <-mean((y_hat_tr_3 - trainS_1$motor_UPDRS)^2)

## MSE for test data
y_hat_test_3 <- as.matrix(testS_1_new) %*% as.matrix(theta_3)
MSE_test_3 <- mean((y_hat_test_3 - testS_1$motor_UPDRS)^2)

