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
map <- data.frame(leave_numbers = rep(0,49), train_de = rep(0,49),
                  valid_de = rep(0,49))

# test_tree_pruned <- prune.tree(tree_third,best = 2)
# test_predict_train_new <- predict(test_tree_pruned, type = "tree",newdata = data_train)
# test_deviance <- deviance(test_predict_train_new)


i <- 1
while(i < 50){
  tree_pruned <- prune.tree(tree_third,best = i + 1)
  predict_train_new <- predict(tree_pruned, type = "tree",newdata = data_train)
  predict_valid_new <- predict(tree_pruned, type = "tree", newdata = data_validation)

  map[i,"leave_numbers"] <- i + 1
  map[i,"train_de"] <- deviance(predict_train_new)
  map[i,"valid_de"] <- deviance(predict_valid_new)

  i <- i + 1
}


index <- order(map$valid_de)[1]
leave_number <- map$leave_numbers[index]


# plot
library(reshape2)
melt_map <- melt(map, id.vars = "leave_numbers")

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
predict_test_4 <- predict(optimal_tree, newdata = data_test,
                          type = "vector")[,"yes"]


threshold <- seq(0.05,0.95,0.05)

TPR_TREE <- c()
FPR_TREE <- c()

TPR_GLM <- c()
FPR_GLM <- c()

P_TREE <- c()
P_GLM <- c()

for (i in threshold) {
  positive_predicted <- predict_test_4 > i
  positive_true <- data_test$y == "yes"
  TP_TREE <- length(which((positive_predicted == positive_true) & positive_predicted == TRUE ))

  negative_predicted <- predict_test_4 < i
  negative_true <- data_test$y == "no"
  TN_TREE <- length(which((negative_predicted == negative_true) & negative_predicted == TRUE ))


  FP_TREE <- length(which((positive_predicted == negative_true) & positive_predicted == TRUE ))

  FN_TREE <- length(which((negative_predicted == positive_true) & negative_predicted == TRUE ))

  # the following is the GLM part
  positive_predicted_2 <- predict_test_3 > i

  TP_GLM <- length(which((positive_predicted_2 == positive_true) & positive_predicted_2 == TRUE ))

  negative_predicted_2 <- predict_test_3 < i

  TN_GLM <- length(which((negative_predicted_2 == negative_true) & negative_predicted_2 == TRUE ))


  FP_GLM <- length(which((positive_predicted_2 == negative_true) & positive_predicted_2 == TRUE ))

  FN_GLM <- length(which((negative_predicted_2 & positive_true) & negative_predicted_2 == TRUE ))

  index <- i * 20

  TPR_TREE[index] <- TP_TREE / (TP_TREE + FN_TREE)
  FPR_TREE[index] <- FP_TREE / (TN_TREE + FP_TREE)
  P_TREE[index] <- TP_TREE / (TP_TREE + FP_TREE)


  TPR_GLM[index] <- TP_GLM / (TP_GLM + FN_GLM)
  FPR_GLM[index] <- FP_GLM / (TN_GLM + FP_GLM)
  P_GLM[index] <- TP_GLM / (TP_GLM + FP_GLM)
}
data_roc_tree <- data.frame(FPR_TREE,TPR_TREE)
data_roc_glm <- data.frame(FPR_GLM,TPR_GLM)
ggplot(data_roc_tree,aes(x = FPR_TREE, y = TPR_TREE)) +
  geom_line(color = "red")  +
  xlim(0,1) + ylim(0,1) + labs(title = "Two ROC Curves for Two Models",
                               x = "FPR", y = "TPR")+
  geom_line(data=data_roc_glm, aes(x = FPR_GLM, y = TPR_GLM), color = "blue")


# plot(FPR_TREE,TPR_TREE, main = "ROC curve for tree")
# plot(FPR_GLM,TPR_GLM, main = "ROC curve for GLM")
plot(TPR_TREE,P_TREE, main = "precision recall curve for tree")
plot(TPR_GLM,P_GLM, main = "precision recall curve for GLM")

