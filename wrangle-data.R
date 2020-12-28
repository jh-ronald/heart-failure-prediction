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