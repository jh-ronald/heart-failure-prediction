library(randomForest)
set.seed(1, sample.kind = "Rounding")

fit <- randomForest(DEATH_EVENT~ ., data=train_hf_c)
confusionMatrix(predict(fit, test_hf_c),
                test_hf_c$DEATH_EVENT)$overall[["Accuracy"]]



set.seed(1, sample.kind = "Rounding")
nodesize <- seq(1, 50, 1)
find_ns <- function(ns, trainset){
  train(DEATH_EVENT ~ ., method = "rf", data = trainset,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
}

acc <- sapply(nodesize, find_ns,
              trainset=train_hf_c)

qplot(nodesize, acc)

nodesize[which.max(acc)]

fit2 <- randomForest(DEATH_EVENT~ ., data=train_hf_c,
                    nodesize=nodesize[which.max(acc)])

confusionMatrix(predict(fit2, test_hf_c),
                test_hf_c$DEATH_EVENT)$overall[["Accuracy"]]