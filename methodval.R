################################################################################
#Validation
################################################################################

#Here I use the two models that yielded the greatest accuracy in training 
#The two models are generalized linear model and random forest model  

################################################################################
#Logistic Regression (Generalized Linear Model)
################################################################################
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

#Build glm model
glm_v <- glm(DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + serum_sodium, 
             data= hf, family = "binomial")

#Generate predictions of numerical values
p_hat_val <- predict(glm_v, newdata = validation, type="response")

#Generate predictions using p from previous training
y_hat_glmv <- ifelse(p_hat_val > p, "Survived", "Dead") %>% factor()

final_validation <- tibble(Method="glm",
                           Accuracy=confusionMatrix(y_hat_glmv, validation$DEATH_EVENT, positive = "Survived")$overall[["Accuracy"]])

################################################################################
#Random Forest
################################################################################
library(randomForest)

set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

#Build random forest model
rf_v <- randomForest(DEATH_EVENT~ ., data=hf)

#Generate predictions
y_hat_rfv <- predict(rf_v, validation, type = "class")

final_validation <- bind_rows(final_validation,
                              tibble(Method="rf",
                                     Accuracy=confusionMatrix(y_hat_rfv,validation$DEATH_EVENT)$overall[["Accuracy"]]))
final_validation

#END