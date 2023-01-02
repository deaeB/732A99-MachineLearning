library(tidyverse)
data3 <- read.csv("C:/Users/yj313/Desktop/LAB/732A99-MachineLearning/lab02/Assignment 3/communities.csv",sep = ",")
set.seed(12345)

# 1 ####
data3_full <- data3

data3_full[-ncol(data3_full)] <-
    data3_full %>%
      select( !ViolentCrimesPerPop) %>%
      apply( 2, scale)

data3_X <- data3_full[-ncol(data3_full)]
# no ViolentCrimesPerPop


# n <- nrow(data3_X)
# S1 <-    t(data3_X) %*% as.matrix(data3_X) / (n-1)
# eigenS1 <- eigen( S  )


S <- cov(data3_X)
eigenS <- eigen( S)


sumvar <- sum(eigenS$values)
for (i in 1:length(eigenS$values)) {
  if (sum(eigenS$values[1:i]) >= 0.95 * sumvar) {
    break;
  }
}
# i:how many components are needed to obtain at least 95% of variance in the data

eigenS$values[c(1,2)] / sumvar
# What is the proportion of variation explained by each of the first two principal components
# 0.80974598 0.05029035

# 2 ####

res <- princomp(data3_X)
lambda <- res$sdev^2
sprintf("%2.3f",lambda/sum(lambda)*100)
screeplot(res)

sort(abs(res$loadings[,1]), decreasing = TRUE)[1:5]
# Report which 5 features contribute mostly (by the absolute value) to the first principle component
# medFamInc      medIncome    PctKids2Par     pctWInvInc PctPopUnderPov

# all highly related to economic level


df_plotPCScores <- data.frame(PC1 = res$scores[,1], PC2 = res$scores[,2], ViolentCrimesPerPop = data3$ViolentCrimesPerPop)

ggplot(df_plotPCScores, aes(x = PC1, y = PC2))+
  geom_point(aes(colour = ViolentCrimesPerPop))

# pop with a higher PC1 score tend to have a higher violent crimes rate


# 3 ####

n <- nrow(data3)
r_train <- sample(1:n , floor(0.5 * n) )
data3_train <- data3[r_train,]
data3_test <- data3[-r_train,]


data3_train_scale <- as.data.frame(scale(data3_train, scale = FALSE))
data3_test_scale <- as.data.frame(scale(data3_test, scale = FALSE))

lm_q3 <- glm(ViolentCrimesPerPop ~.-1 , data = data3_train_scale, family = gaussian)
# what does family do?

err_train <- mean(lm_q3$residuals^2)

fit_test <- predict.glm(lm_q3, newdata = data3_test_scale)
err_test <- mean((fit_test - data3_test_scale$ViolentCrimesPerPop)^2)

# it sucks!

# 4 ####
# dataIn: r101

err_train_liklihood_optimed <- c()
err_test_liklihood_optimed <- c()

fun_cost <- function(theta, dataIn){
  y_hat <- theta %*% t(as.matrix(dataIn[,1:100]))
  y <- dataIn[,101]
  cost <- sum((y_hat - y)^2) / n


  pred_train_liklihood <- theta %*% t(data3_train_scale[,1:100])
  pred_test_liklihood <- theta %*% t(data3_test_scale[,1:100])
  i <<- i+1
  err_train_liklihood_optimed[i] <<- mean((pred_train_liklihood - data3_train_scale$ViolentCrimesPerPop)^2)
  err_test_liklihood_optimed[i] <<- mean((pred_test_liklihood - data3_test_scale$ViolentCrimesPerPop)^2)
  # cost <- -cost
  # # minimize
  return(cost)
}

theta0 <- rep(0,100)

i <- 0
# for fun_cost
theta_hat_optim <- optim(theta0, fun_cost, dataIn = data3_train_scale, method = "BFGS")

ggplot(data = data.frame(err_train_liklihood_optimed, err_test_liklihood_optimed) )+
  geom_point(aes(x = 1:i, y = err_train_liklihood_optimed, shape = "train error"),
             size = 1, color = "red") +
  geom_point(aes(x = 1:i, y = err_test_liklihood_optimed, shape = "test error"),
             size = 1) +
  xlim(500, i) +
  ylim(0, 0.03) +
  labs(
    x = "iterations",
    y = "errors"
  ) +
  # scale_color_manual(
  #   name = "shape1",
  #   values = c('red' = 'red', "black" = 'black'),
  #   breaks = c("red", "black"),
  #   labels = c('train error', 'test error')
  # ) +
  scale_shape_manual(
    values = c("train error" = 16, "test error" = 17),
    labels = c('train error', 'test error'),
  )



# err_train_liklihood <- c()
# err_test_liklihood <- c()
# theta_hat_iteration <- theta0
# for (i in 1:250) {
#   theta_hat_iteration_optim <-
#     optim(
#       par = theta_hat_iteration,
#       fn = fun_cost,
#       dataIn = data3_train_scale,
#       method = "BFGS",
#       control = list(maxit = 1))
#   theta_hat_iteration <- theta_hat_iteration_optim$par
#   pred_train_liklihood <- theta_hat_iteration %*% t(data3_train_scale[,1:100])
#   err_train_liklihood[i] <- mean((pred_train_liklihood - data3_train_scale$ViolentCrimesPerPop)^2)
#   pred_test_liklihood <- theta_hat_iteration %*% t(data3_test_scale[,1:100])
#   err_test_liklihood[i] <- mean((pred_test_liklihood - data3_test_scale$ViolentCrimesPerPop)^2)
# }




