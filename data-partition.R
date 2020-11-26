if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = heartfailure$DEATH_EVENT, times = 1, p = 0.1, list = FALSE)
hf <- heartfailure[-test_index,]
validation <- heartfailure[test_index,]

set.seed(1, sample.kind="Rounding")

#Randomly splitting dataset to obtain test index (train:test = 9:1)
test_index_hf <- createDataPartition(y = hf$DEATH_EVENT, 
                                      times = 1, 
                                      p = 0.1, 
                                      list = FALSE)
train_hf <- hf[-test_index_hf,]
test_hf <- hf[test_index_hf]

