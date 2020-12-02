set.seed(1, sample.kind = "Rounding")

train_hf_c <- subset(train_hf, select = -c(time))
test_hf_c <- subset(test_hf, select = -c(time))


train_rpart <- train(DEATH_EVENT ~ ., method = "rpart",
        data = train_hf_c)

y_hat2 <- predict(train_rpart, test_hf_c)


confusionMatrix(y_hat2, test_hf_c$DEATH_EVENT, positive = "Survived")