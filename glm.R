train_hf$DEATH_EVENT <- ifelse(train_hf$DEATH_EVENT==0,"Survived","Dead")
test_hf$DEATH_EVENT <- ifelse(test_hf$DEATH_EVENT==0,"Survived","Dead")

train_hf$DEATH_EVENT <- as.factor(train_hf$DEATH_EVENT)
test_hf$DEATH_EVENT <- as.factor(test_hf$DEATH_EVENT)


glm_fit <- mutate(train_hf, y=as.numeric(DEATH_EVENT=="Survived")) %>%
  glm(y ~ age + ejection_fraction + serum_creatinine + serum_sodium, data= ., family = "binomial")

p_hat <- predict(glm_fit, newdata = test_hf, type="response")

find_phat <- function(phat,testset){
  y_hat <- ifelse(p_hat > phat, "Survived", "Dead") %>% factor()
  
  return(confusionMatrix(factor(y_hat, 
                                levels = c("Dead","Survived")),
                         factor(test_hf$DEATH_EVENT,
                                levels = c("Dead","Survived")))$overall[["Accuracy"]])
}

phat <- seq(0,1,0.01)
accuracy <- sapply(phat, find_phat,
                   testset=test_hf)
qplot(phat, accuracy)

p <- phat[which.max(accuracy)]

y_hat <- ifelse(p_hat > p, "Survived", "Dead") %>% factor()
confusionMatrix(y_hat, test_hf$DEATH_EVENT, positive = "Survived")$overall[["Accuracy"]]