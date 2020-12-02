set.seed(1, sample.kind = "Rounding")

hf$DEATH_EVENT <- ifelse(hf$DEATH_EVENT==0,"Survived","Dead")
validation$DEATH_EVENT <- ifelse(validation$DEATH_EVENT==0,"Survived","Dead")

hf$DEATH_EVENT <- as.factor(hf$DEATH_EVENT)
validation$DEATH_EVENT <- as.factor(validation$DEATH_EVENT)


glm_fit_val <- mutate(hf, y=as.numeric(DEATH_EVENT=="Survived")) %>%
  glm(y ~ age + ejection_fraction + serum_creatinine + serum_sodium, data= ., family = "binomial")

p_hat_val <- predict(glm_fit_val, newdata = validation, type="response")

y_hat_val <- ifelse(p_hat_val > 0.6, "Survived", "Dead") %>% factor()
confusionMatrix(y_hat_val, validation$DEATH_EVENT, positive = "Survived")$overall[["Accuracy"]]

##########################################################################################
set.seed(1, sample.kind = "Rounding")

hf_c <- subset(hf, select = -c(time))
val_c <- subset(validation, select = -c(time))


train_rpart_val <- train(DEATH_EVENT ~ ., method = "rpart",
                     data = hf_c)

y_hat_cval <- predict(train_rpart_val, val_c)
confusionMatrix(y_hat_cval, val_c$DEATH_EVENT, positive = "Survived")$overall[["Accuracy"]]

##############################################################################################
library(randomForest)
set.seed(1, sample.kind = "Rounding")

fit_val <- randomForest(DEATH_EVENT~ ., data=hf_c)
confusionMatrix(predict(fit_val, val_c),
                val_c$DEATH_EVENT)$overall[["Accuracy"]]



set.seed(1, sample.kind = "Rounding")
nodesize <- seq(1, 50, 1)
acc <- sapply(nodesize, find_ns,
              trainset=hf_c)

qplot(nodesize, acc)

nodesize[which.max(acc)]

fit_val2 <- randomForest(DEATH_EVENT~ ., data=hf_c,
                     nodesize=nodesize[which.max(acc)])

confusionMatrix(predict(fit_val2, validation),
                validation$DEATH_EVENT)$overall[["Accuracy"]]