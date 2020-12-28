set.seed(1, sample.kind = "Rounding")

library(purrr)
find_knn <- function(k, trainset, testset){
  fit <- knn3(DEATH_EVENT ~ ., data = trainset, k = k)
  
  y_hat <- predict(fit, testset, type = "class")
  return(confusionMatrix(y_hat, testset$DEATH_EVENT)$overall[["Accuracy"]])
}

ks <- seq(3,100,1)
acc_knn <- sapply(ks,find_knn,
                  trainset = train_hf,
                  testset = test_hf)

qplot(ks,acc_knn)
k <- ks[which.max(acc_knn)]

set.seed(1, sample.kind = "Rounding")

library(caret)
knn_fit <- knn3(DEATH_EVENT ~ ., data = train_hf, k=k)
y_hat_knn <- predict(knn_fit, test_hf, type = "class")

confusionMatrix(y_hat_knn, test_hf$DEATH_EVENT)$overall["Accuracy"]

