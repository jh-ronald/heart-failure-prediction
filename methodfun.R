################################################################################
#Models
################################################################################

################################################################################
#Logistic Regression (Generalized Linear Model)
################################################################################
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

#Build a glm model with four most reliable predictors (based on correlation coefficient)
glm_fit <- glm(DEATH_EVENT ~ age + ejection_fraction + serum_creatinine + serum_sodium, 
               data= train_hf, family = "binomial")

#Generate predictions of numerical values
p_hat_glm <- predict(glm_fit, newdata = test_hf, type="response")

#Define function to find best (most accurate) decision rule
find_phat <- function(phat,pred,testset){
  y_hat <- ifelse(pred > phat, "Survived", "Dead") %>% factor()
  
  return(confusionMatrix(factor(y_hat, 
                                levels = c("Dead","Survived")),
                         testset$DEATH_EVENT)$overall[["Accuracy"]])
}

#Apply function to find best (most accurate) decision rule
phat <- seq(0,1,0.01)
accuracy <- sapply(phat, find_phat,
                   pred = p_hat_glm,
                   testset=test_hf)

#Plot values of p and accuracy
qplot(phat, accuracy)

p <- phat[which.max(accuracy)] #0.6

#Generate predictions
y_hat_glm <- ifelse(p_hat_glm > p, "Survived", "Dead") %>% factor()

#Record accuracy
results <- tibble(Method = "glm",
                  Accuracy = confusionMatrix(y_hat_glm, test_hf$DEATH_EVENT, positive = "Survived")$overall[["Accuracy"]])
results #glm:0.821

################################################################################
#Classification Tree
################################################################################
#Install and load rpart and rpart.plot packages
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
library(rpart)
library(rpart.plot)

set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

#Build classification tree
rp_fit <- rpart(DEATH_EVENT ~ .,
                data = train_hf,
                method = "class",
                cp = 0.01)

#Generate predictions
y_hat_rp <- predict(rp_fit, test_hf, type = "class")

#Record accuracy
results <- bind_rows(results, tibble(Method="rpart",
                                     Accuracy=confusionMatrix(y_hat_rp, test_hf$DEATH_EVENT, positive = "Survived")))

results #rp:0.75

#Classification Tree
rpart.plot(rp_fit, type = 1)

################################################################################
#Random Forest
################################################################################
#Install and load randomForest package
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)

set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

#Build random forest model
fit_rf <- randomForest(DEATH_EVENT~ ., data=train_hf)

#Generate predictions
y_hat_rf <- predict(fit_rf, test_hf, type = "class")

#Record accuracy
results <- bind_rows(results,
                     tibble(Method="rf",
                            Accuracy=confusionMatrix(y_hat_rf, test_hf$DEATH_EVENT, positive = "Survived")$overall[["Accuracy"]]))
results #rf:0.821

################################################################################
#k-nearest neighbours (knn)
################################################################################
library(caret)

set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

#Define function to find best value of k (that gives the highest accuracy)
find_knn <- function(k, trainset, testset){
  fit <- knn3(DEATH_EVENT ~ ., data = trainset, k = k)
  
  y_hat <- predict(fit, testset, type = "class")
  return(confusionMatrix(y_hat, testset$DEATH_EVENT)$overall[["Accuracy"]])
}

#Apply function to find best value of k
ks <- seq(3,100,1)
acc_knn <- sapply(ks,find_knn,
                  trainset = train_hf,
                  testset = test_hf)

#Plot vlaues of k and accuracy
qplot(ks,acc_knn)
k <- ks[which.max(acc_knn)] #3

set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

#Build knn model
knn_fit <- knn3(DEATH_EVENT ~ ., data = train_hf, k=k)

#Generate predictions
y_hat_knn <- predict(knn_fit, test_hf, type = "class")

#Record accuracy
results <- bind_rows(results,
                     tibble(Method="knn",
                            Accuracy=confusionMatrix(y_hat_knn, test_hf$DEATH_EVENT)$overall[["Accuracy"]]))

results #knn:0.643

################################################################################
#Support Vector Machine (SVM)
################################################################################
#Install and load e1071 package
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
library(e1071)

set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

#Build svm model
svm_fit <- svm(DEATH_EVENT ~ .,
               data = train_hf,
               kernel = "linear")

#Generate predictions
y_hat_svm <- predict(svm_fit,test_hf, type = "class")

#Record accuracy
results <- bind_rows(results,
                     tibble(Method="svm",
                            Accuracy=confusionMatrix(y_hat_svm,test_hf$DEATH_EVENT)$overall[["Accuracy"]]))
results #svm:0.75