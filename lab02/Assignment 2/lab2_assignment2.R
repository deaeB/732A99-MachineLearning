# task1
mydata <- read.csv("bank-full.csv", sep = ";",stringsAsFactors = TRUE)
mydata <- mydata[,-12]

n <- dim(mydata)[1]
set.seed(12345)
id <- sample(1:n,floor(n * 0.4))
data_train <- mydata[id,]

id1 <- setdiff(1:n,id)
set.seed(12345)
id2 <- sample(id1, floor(0.3 * n))
data_validation <- mydata[id2,]

id3 <- setdiff(id1,id2)
data_test <- mydata[id3,]

# task2

# the first decision tree without any limitation
library(tree)
tree_first <- tree(y~.,data = data_train)
predict_train_1 <- predict(tree_first,type = "class")
mis_train_1 <- 1- sum(diag(table(data_train$y,predict_train_1))) /
  nrow(data_train)


predict_valid_1 <- predict(tree_first, newdata = data_validation,type = "class")
mis_valid_1 <- 1- sum(diag(table(data_validation$y,predict_valid_1))) /
  nrow(data_validation)
# plot(tree_first,mai = c(0.5, 0.1, 0.5, 0.1))


# the second decision tree with smallest allowed node size equal to 7000.
tree_second <- tree(y~.,data = data_train,minsize = 7000)
predict_train_2 <- predict(tree_second,type = "class")
mis_train_2 <- 1- sum(diag(table(data_train$y, predict_train_2))) /
  nrow(data_train)

predict_valid_2 <- predict(tree_second, newdata = data_validation,type = "class")
mis_valid_2 <- 1- sum(diag(table(data_validation$y, predict_valid_2))) /
  nrow(data_validation)
# plot(tree_second,mai = c(0.5, 0.1, 0.5, 0.1))



# the third decision tree(minimum deviance to 0.0005)
tree_third <- tree(y~.,data = data_train,mindev = 0.0005)
predict_train_3 <- predict(tree_third,type = "class")
mis_train_3 <- 1- sum(diag(table(data_train$y, predict_train_3))) /
  nrow(data_train)

predict_valid_3 <- predict(tree_third,newdata = data_validation, type = "class")
mis_valid_3 <- 1- sum(diag(table(data_validation$y, predict_valid_3))) /
  nrow(data_validation)
# plot(tree_third,mai = c(0.5, 0.1, 0.5, 0.1))

result_table <- data.frame(train_error = c(mis_train_1,mis_train_2,mis_train_3),
                           valid_error = c(mis_valid_1,mis_valid_2,mis_valid_3))
rownames(result_table) <- c("default", "nodesize_7000", "mindev_0.0005")
result_table



# task3
leave_numbers <- NULL
train_de <- NULL
valid_de <- NULL

i <- 1
while(i < 50){
  tree_pruned <- prune.tree(tree_third,best = i + 1)
  predict_train_new <- predict(tree_pruned, type = "tree",newdata = data_train)
  predict_valid_new <- predict(tree_pruned, type = "tree", newdata = data_validation)

  leave_numbers[i] <- i + 1
  train_de[i] <- deviance(predict_train_new)
  valid_de[i] <- deviance(predict_valid_new)

  i <- i + 1
}

data_deviance <- data.frame(leave_numbers, train_de, valid_de)
index <- order(valid_de)[1]
leave_number <- leave_numbers[index]


# plot
library(reshape2)
melt_map <- melt(data_deviance, id.vars = "leave_numbers")

library(ggplot2)
ggplot(data = melt_map, aes(x = leave_numbers, y = value, color = variable)) +
  geom_line() + geom_point() + xlab("leave numbers") + ylab("deviance")



optimal_tree <- prune.tree(tree_third,best = leave_number)
summary(optimal_tree)



# task4
predict_test <- predict(optimal_tree,newdata = data_test, type = "class")
confusion_matrix <- table(data_test$y,predict_test)

true_positive <- confusion_matrix[2,2]
false_negative <- confusion_matrix[2,1]
false_positive <- confusion_matrix[1,2]
recall <- true_positive / (true_positive + false_negative)
precision <- true_positive / (true_positive + false_positive)
F1 <- 2 * precision * recall / (precision + recall)

accuracy <- 1 - mean(data_test$y != predict_test)
print(confusion_matrix)
cat("F1 score:",F1,"\naccuracy:",accuracy)


# task5
library(rpart)
loss_matrix <- matrix(c(0,1,5,0), nrow = 2, ncol = 2, byrow = TRUE)

# another function to create a tree with loss_matrix
new_tree <- rpart(y~.,data_train,method = "class", parms = list(loss = loss_matrix) )

predict_test_2 <- predict(new_tree, newdata = data_test, type = "class" )

confusion_matrix2 <- table(data_test$y,predict_test_2)
confusion_matrix2

true_positive2 <- confusion_matrix2[2,2]
false_negative2 <- confusion_matrix2[2,1]
false_positive2 <- confusion_matrix2[1,2]
recall2 <- true_positive2 / (true_positive2 + false_negative2)
precision2 <- true_positive2 / (true_positive2 + false_positive2)
F1_2 <- 2 * precision2 * recall2 / (precision2 + recall2)


accuracy_2 <- 1 - mean(data_test$y != predict_test_2)
print(confusion_matrix2)
cat("F1 score:",F1_2,"\naccuracy:",accuracy_2)


# task6


lr <- glm(y~.,data = data_train,family = "binomial")
predict_test_3 <- predict(lr, newdata = data_test, type = "response")
predict_test_4_temp <- predict(optimal_tree, newdata = data_test,
                          type = "vector")
predict_test_4 <- as.data.frame(predict_test_4_temp)$yes

threshold <- seq(0.05,0.95,0.05)

# TREE part
TPR1 <- NULL
FPR1 <- NULL
P1 <- NULL

for (i in threshold) {

  predicted <- ifelse(predict_test_4 > rep(i,13564), "yes","no")
  TP1 <- length(which((predicted  == data_test$y) & predicted == "yes" ))
  TN1 <-length(which((predicted  == data_test$y) & predicted == "no" ))
  FP1 <- length(which((predicted != data_test$y) & predicted == "yes" ))
  FN1 <- length(which((predicted != data_test$y) & predicted == "no" ))

  index <- i * 20

  TPR1[index] <- TP1 / (TP1 + FN1)
  FPR1[index] <- FP1 / (TN1 + FP1)
  P1[index] <- TP1 / (TP1 + FP1)

}

# GLM part

TPR2 <- NULL
FPR2 <- NULL
P2 <- NULL

for (i in threshold) {

  predicted_GLM <- ifelse(predict_test_3 > rep(i,13564), "yes","no")
  TP2 <- length(which(predicted_GLM == data_test$y & predicted_GLM == "yes"))
  TN2 <- length(which(predicted_GLM == data_test$y & predicted_GLM == "no"))
  FP2<- length(which(predicted_GLM != data_test$y & predicted_GLM == "yes"))
  FN2 <- length(which(predicted_GLM != data_test$y & predicted_GLM == "no"))

  index <- i * 20

  TPR2[index] <- TP2/ (TP2 + FN2)
  FPR2[index] <- FP2 / (TN2 + FP2)
  P2[index] <- TP2 / (TP2 + FP2)
}



data_roc_tree <- data.frame(FPR1,TPR1)
data_roc_glm <- data.frame(FPR2,TPR2)
ggplot(data_roc_tree,aes(x = FPR1, y = TPR1)) +
  geom_line(color = "red")  +
  xlim(0,1) + ylim(0,1) + labs(title = "Two ROC Curves for Two Models",
                               x = "FPR", y = "TPR")+
  geom_line(data=data_roc_glm, aes(x = FPR2, y = TPR2), color = "blue")



plot(TPR1,P1, main = "precision recall curve for tree")
plot(TPR2,P2, main = "precision recall curve for GLM")

