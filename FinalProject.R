################################################################################
#Data Download
################################################################################
#Original dataset is found on https://www.kaggle.com/andrewmvd/heart-failure-clinical-data
#To simplify the operation, I have pre-downloaded it and uploaded it into my own GitHub repo, titled "heart-failure-prediction".
#Download the heart failure data from my own GitHub repo
#This data is identical to the one found on https://www.kaggle.com/andrewmvd/heart-failure-clinical-data
dl <- tempfile()
download.file("https://raw.githubusercontent.com/jh-ronald/heart-failure-prediction/main/heart_failure_clinical_records_dataset.csv",
              destfile = dl)

################################################################################
#Data Wrangling
################################################################################
#Install and load tidyverse and data.table packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(data.table)

#Convert csv file into readable data table
temp <- read_csv(dl)
heartfailure <- data.table(temp)

#Convert categorical predictors with boolean values into comprehensible characters
heartfailure$anaemia <- ifelse(heartfailure$anaemia==0, "No", "Yes")
heartfailure$diabetes <- ifelse(heartfailure$diabetes==0, "No", "Yes")
heartfailure$high_blood_pressure <- ifelse(heartfailure$high_blood_pressure==0, "No", "Yes")
heartfailure$sex <- ifelse(heartfailure$sex==0, "Female", "Male")
heartfailure$smoking <- ifelse(heartfailure$smoking==0, "No", "Yes")
heartfailure$DEATH_EVENT <- ifelse(heartfailure$DEATH_EVENT==0,"Survived","Dead")

#Further convert categorical predictors into factors with two levels
heartfailure$anaemia <- as.factor(heartfailure$anaemia)
heartfailure$diabetes <- as.factor(heartfailure$diabetes)
heartfailure$high_blood_pressure <- as.factor(heartfailure$high_blood_pressure)
heartfailure$sex <- as.factor(heartfailure$sex)
heartfailure$smoking <- as.factor(heartfailure$smoking)
heartfailure$DEATH_EVENT <- as.factor(heartfailure$DEATH_EVENT)

################################################################################
#Data Partition
################################################################################
#Install and load caret package
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

#Randomly splitting dataset into hf (training set) and validation (validation set)
#This is done in a training:validation = 9:1 ratio
test_index <- createDataPartition(y = heartfailure$DEATH_EVENT, times = 1, p = 0.1, list = FALSE)
hf <- heartfailure[-test_index,]
validation <- heartfailure[test_index,]

#Deleting time column 
#Reason for this is:
#Time refers to amount of time spent in the ward after heart failure, up to time of complete recovery or death
#It is impossible to know this information beforehand and during treatment
#Conclusion:
#Time is not a viable predictor
hf <- subset(hf, select = -c(time))
validation <- subset(validation, select = -c(time))

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

#Randomly splitting dataset into train_hf (training set) and test_hf (validation set)
#This is done in a training:validation = 9:1 ratio
test_index_hf <- createDataPartition(y = hf$DEATH_EVENT, 
                                     times = 1, 
                                     p = 0.1, 
                                     list = FALSE)
train_hf <- hf[-test_index_hf,]
test_hf <- hf[test_index_hf]

################################################################################
#Data Analysis
################################################################################

#Note that in the analysis, the predictor 'time' is not included

################################################################################
#Overview
################################################################################
head(heartfailure) 

dim(heartfailure) #299 rows and 13 columns (including time)

################################################################################
#Age
################################################################################
#Smooth density plot of age of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=age, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

#Box and whisker plot of age of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=age, fill=DEATH_EVENT)) +
  geom_boxplot() 

#Correlation coefficient of age and DEATH_EVENT
heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(age, d)) %>% 
  pull(r) #-0.2537285

################################################################################
#Anaemia
################################################################################
#Box plot of patients with/without anaemia adn their status
df_anaemia <- data.frame(anaemia=rep(c("Without","With"),each=2),
                         status=rep(c("Survived","Dead"),2),
                         count=c(sum(heartfailure$DEATH_EVENT[heartfailure$anaemia=="No"]=="Survived"),
                                 sum(heartfailure$DEATH_EVENT[heartfailure$anaemia=="No"]=="Dead"),
                                 sum(heartfailure$DEATH_EVENT[heartfailure$anaemia=="Yes"]=="Survived"),
                                 sum(heartfailure$DEATH_EVENT[heartfailure$anaemia=="Yes"]=="Dead")))

df_anaemia %>%
  ggplot(aes(x=status,y=count,fill=anaemia)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)

#P-value of the relationship between anaemia and status
tab_anaemia <- matrix(c(46,50,83,120),2,2) #Numbers are read from the above plot
rownames(tab_anaemia)<-c("With anaemia","Without anaemia")
colnames(tab_anaemia)<-c("Died","Survived")

fisher.test(tab_anaemia)$p.value #0.2627743

#Correlation coefficient of anaemia and DEATH_EVENT
heartfailure %>% 
  mutate(ana=as.numeric(anaemia),
         d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(ana, d)) %>% 
  pull(r) #-0.0662701

################################################################################
#Creatinine Phosphokinase (CPK level)
################################################################################  
#Smooth density plot of creatinine phosphokinase levels of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=creatinine_phosphokinase, fill=DEATH_EVENT)) +
  geom_boxplot()

#Box and whisker plot of creatinine phosphokinase levels of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=creatinine_phosphokinase, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

#Correlation coefficient of creatinine phosphokinase levels and DEATH_EVENT
heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(creatinine_phosphokinase, d)) %>% 
  pull(r) #-0.06272816

################################################################################
#Diabetes
################################################################################      
#Box plot of patients with/without diabetes and their status
df_diabetes <- data.frame(diabetes=rep(c("Without","With"),each=2),
                          status=rep(c("Survived","Dead"),2),
                          count=c(sum(heartfailure$DEATH_EVENT[heartfailure$diabetes=="No"]=="Survived"),
                                  sum(heartfailure$DEATH_EVENT[heartfailure$diabetes=="No"]=="Dead"),
                                  sum(heartfailure$DEATH_EVENT[heartfailure$diabetes=="Yes"]=="Survived"),
                                  sum(heartfailure$DEATH_EVENT[heartfailure$diabetes=="Yes"]=="Dead")))
df_diabetes %>%
  ggplot(aes(x=status,y=count,fill=diabetes)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)

#P-value of the relationship between diabetes and status
tab_diabetes <- matrix(c(40,85,56,118),2,2)
rownames(tab_diabetes)<-c("With diabetes","Without diabetes")
colnames(tab_diabetes)<-c("Died","Survived")

fisher.test(tab_diabetes)$p.value #1

#Correlation coefficient of diabetes and DEATH_EVENT
heartfailure %>% 
  mutate(dia=as.numeric(diabetes),
         d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(dia, d)) %>% 
  pull(r) #0.001942883

################################################################################
#Ejection fraction
################################################################################
#Smooth density plot of ejection fraction levels of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=ejection_fraction, fill=DEATH_EVENT)) +
  geom_boxplot()

#Box and whisker plot of ejection fraction levels of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=ejection_fraction, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

#Correlation coefficient of ejection fraction levels and DEATH_EVENT
heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(ejection_fraction, d)) %>% 
  pull(r) #0.2686033

################################################################################
#High Blood Pressure
################################################################################
#Box plot of patients with/without high blood pressure and their status
df_hbp <- data.frame(hbp=rep(c("Without","With"),each=2),
                     status=rep(c("Survived","Dead"),2),
                     count=c(sum(heartfailure$DEATH_EVENT[heartfailure$high_blood_pressure=="No"]=="Survived"),
                             sum(heartfailure$DEATH_EVENT[heartfailure$high_blood_pressure=="No"]=="Dead"),
                             sum(heartfailure$DEATH_EVENT[heartfailure$high_blood_pressure=="Yes"]=="Survived"),
                             sum(heartfailure$DEATH_EVENT[heartfailure$high_blood_pressure=="Yes"]=="Dead")))
df_hbp %>%
  ggplot(aes(x=status,y=count,fill=hbp)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)

#P-value of the relationship between high blood pressure and status
tab_hbp <- matrix(c(39,57,66,137),2,2)
rownames(tab_hbp)<-c("With high blood pressure","Without high blood pressure")
colnames(tab_hbp)<-c("Died","Survived")

fisher.test(tab_hbp)$p.value #0.194805

#Correlation coefficient of diabetes and DEATH_EVENT
heartfailure %>% 
  mutate(hbp=as.numeric(high_blood_pressure),
         d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(hbp, d)) %>% 
  pull(r) #-0.07935106

################################################################################
#Platelets
################################################################################
#Smooth density plot of platelets levels of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=platelets, fill=DEATH_EVENT)) +
  geom_boxplot()

#Box and whisker plot of platelets levels of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=platelets, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

#Correlation coefficient of platelets levels and DEATH_EVENT
heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(platelets, d)) %>% 
  pull(r) #0.04913887

################################################################################
#Serum Creatinine
################################################################################
#Smooth density plot of serum creatinine levels of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=serum_creatinine, fill=DEATH_EVENT)) +
  geom_boxplot()

#Box and whisker plot of serum creatinine levels of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=serum_creatinine, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

#Correlation coefficient of serum creatinine levels and DEATH_EVENT
heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(serum_creatinine, d)) %>% 
  pull(r) #-0.2942776

################################################################################
#Serum Sodium
################################################################################
#Smooth density plot of serum sodium levels of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=serum_sodium, fill=DEATH_EVENT)) +
  geom_boxplot()

#Box and whisker plot of serum sodium levels of patients and their status (Survived or Dead)
heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=serum_sodium, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

#Correlation coefficient of serum sodium levels and DEATH_EVENT
heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(serum_sodium, d)) %>% 
  pull(r) #0.1952036

################################################################################
#Sex
################################################################################
#Box plot of sex of patients and their status
df_sex <- data.frame(sex=rep(c("Female","Male"),each=2),
                     status=rep(c("Survived","Dead"),2),
                     count=c(sum(heartfailure$DEATH_EVENT[heartfailure$sex=="Female"]=="Survived"),
                             sum(heartfailure$DEATH_EVENT[heartfailure$sex=="Female"]=="Dead"),
                             sum(heartfailure$DEATH_EVENT[heartfailure$sex=="Male"]=="Survived"),
                             sum(heartfailure$DEATH_EVENT[heartfailure$sex=="Male"]=="Dead")))
df_sex %>%
  ggplot(aes(x=status,y=count,fill=sex)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)

#P-value of the relationship between sex and status
tab_sex <- matrix(c(62,132,34,71),2,2)
rownames(tab_sex)<-c("Male","Female")
colnames(tab_sex)<-c("Died","Survived")

fisher.test(tab_sex)$p.value #1

#Correlation coefficient of sex and DEATH_EVENT
heartfailure %>% 
  mutate(sex=as.numeric(sex),
         d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(sex, d)) %>% 
  pull(r) #0.004316376

################################################################################
#Smoking
################################################################################
#Box plot of patients with/without habit of smoking and their status
df_smoking <- data.frame(smoking=rep(c("Without","With"),each=2),
                         status=rep(c("Survived","Dead"),2),
                         count=c(sum(heartfailure$DEATH_EVENT[heartfailure$smoking=="No"]=="Survived"),
                                 sum(heartfailure$DEATH_EVENT[heartfailure$smoking=="No"]=="Dead"),
                                 sum(heartfailure$DEATH_EVENT[heartfailure$smoking=="Yes"]=="Survived"),
                                 sum(heartfailure$DEATH_EVENT[heartfailure$smoking=="Yes"]=="Dead")))
df_smoking %>%
  ggplot(aes(x=status,y=count,fill=smoking)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)

#P-value of the relationship between smoking and status
tab_smoking <- matrix(c(30,66,66,137),2,2)
rownames(tab_smoking)<-c("Smoking","Without Smoking")
colnames(tab_smoking)<-c("Died","Survived")

fisher.test(tab_smoking)$p.value #0.8947374

#Correlation coefficient of smoking and DEATH_EVENT
heartfailure %>% 
  mutate(smok=as.numeric(smoking),
         d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(smok, d)) %>% 
  pull(r) #0.01262315

################################################################################
#Correlation Plot
################################################################################
#Install and load corrplot package
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
library(corrplot)

#Convert data table into numerical data frame
heartfailure_df <- as.data.frame(heartfailure)
i <- c(1:13)
heartfailure_df <- sapply(heartfailure_df[,i],as.numeric)

#Calculate correlation coefficients between predictors
#Here 'time' predictor is included, just to make it a whole correlation plot
corr<-cor(heartfailure_df)
colnames(corr) <- c("Age","Anaemia","CPK","Diabetes","EjFraction",
                    "HBP","Platelets","SerCre",
                    "SerSo", "Sex", "Smoking", "Time", "Death")
rownames(corr) <- c("Age","Anaemia","CPK","Diabetes","EjFraction",
                    "HBP","Platelets","SerCre",
                    "SerSo", "Sex", "Smoking", "Time", "Death")

#Correlation Plot
corrplot(corr, method="color",tl.col="black", tl.srt=45, 
         addCoef.col = "black", number.cex = 0.75)

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
                                     Accuracy=confusionMatrix(y_hat_rp, test_hf$DEATH_EVENT, positive = "Survived")$overall[["Accuracy"]]))

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

final_validation #glm:0.806

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

final_validation #rf:0.806

#END