if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
library(corrplot)

heartfailure_df <- as.data.frame(heartfailure)
i <- c(1:13)
heartfailure_df <- sapply(heartfailure_df[,i],as.numeric)

corr<-cor(heartfailure_df)

colnames(corr) <- c("Age","Anaemia","CPK","Diabetes","EjFraction",
                    "HBP","Platelets","SerCre",
                    "SerSo", "Sex", "Smoking", "Time", "Death")
rownames(corr) <- c("Age","Anaemia","CPK","Diabetes","EjFraction",
                    "HBP","Platelets","SerCre",
                    "SerSo", "Sex", "Smoking", "Time", "Death")

corrplot(corr, method="color",tl.col="black", tl.srt=45, 
         addCoef.col = "black", number.cex = 0.75)
