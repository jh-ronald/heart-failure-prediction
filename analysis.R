##########################################################
# Age
##########################################################
heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=age, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

heartfailure$DEATH_EVENT <- as.factor(heartfailure$DEATH_EVENT)
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=age, fill=DEATH_EVENT)) +
  geom_boxplot() 

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(age, d)) %>% 
  pull(r)

##########################################################
# Anaemia
##########################################################
df_anaemia <- data.frame(ana=rep(c("0","1"),each=2),
                         D=rep(c("0","1"),2),
                         count=c(sum(heartfailure$DEATH_EVENT[heartfailure$anaemia==0]==0),
                                 sum(heartfailure$DEATH_EVENT[heartfailure$anaemia==0]==1),
                                 sum(heartfailure$DEATH_EVENT[heartfailure$anaemia==1]==0),
                                 sum(heartfailure$DEATH_EVENT[heartfailure$anaemia==1]==1)))


df_anaemia %>%
  ggplot(aes(x=D,y=count,fill=ana)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)

tab_anaemia <- matrix(c(46,50,83,120),2,2)
rownames(tab_anaemia)<-c("With anaemia","Without anaemia")
colnames(tab_anaemia)<-c("Died","Survived")

fisher.test(tab_anaemia)$p.value

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(anaemia, d)) %>% 
  pull(r)

##########################################################
# Creatinine Phosphokinase (CPK level)
##########################################################  
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=creatinine_phosphokinase, fill=DEATH_EVENT)) +
  geom_boxplot()

heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=creatinine_phosphokinase, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(creatinine_phosphokinase, d)) %>% 
  pull(r)

##########################################################
# Diabetes
##########################################################      
df_diabetes <- data.frame(dia=rep(c("0","1"),each=2),
                          D=rep(c("0","1"),2),
                          count=c(sum(heartfailure$DEATH_EVENT[heartfailure$diabetes==0]==0),
                                  sum(heartfailure$DEATH_EVENT[heartfailure$diabetes==0]==1),
                                  sum(heartfailure$DEATH_EVENT[heartfailure$diabetes==1]==0),
                                  sum(heartfailure$DEATH_EVENT[heartfailure$diabetes==1]==1)))
df_diabetes %>%
  ggplot(aes(x=D,y=count,fill=dia)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)

tab_diabetes <- matrix(c(40,85,56,118),2,2)
rownames(tab_diabetes)<-c("With diabetes","Without diabetes")
colnames(tab_diabetes)<-c("Died","Survived")

fisher.test(tab_diabetes)$p.value

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(diabetes, d)) %>% 
  pull(r)

##########################################################
# Ejection fraction
##########################################################
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=ejection_fraction, fill=DEATH_EVENT)) +
  geom_boxplot()

heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=ejection_fraction, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(ejection_fraction, d)) %>% 
  pull(r)

##########################################################
# High Blood Pressure
##########################################################
df_hbp <- data.frame(hbp=rep(c("0","1"),each=2),
                          D=rep(c("0","1"),2),
                          count=c(sum(heartfailure$DEATH_EVENT[heartfailure$high_blood_pressure==0]==0),
                                  sum(heartfailure$DEATH_EVENT[heartfailure$high_blood_pressure==0]==1),
                                  sum(heartfailure$DEATH_EVENT[heartfailure$high_blood_pressure==1]==0),
                                  sum(heartfailure$DEATH_EVENT[heartfailure$high_blood_pressure==1]==1)))
df_hbp %>%
  ggplot(aes(x=D,y=count,fill=hbp)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)

tab_hbp <- matrix(c(39,57,66,137),2,2)
rownames(tab_hbp)<-c("With high blood pressure","Without high blood pressure")
colnames(tab_hbp)<-c("Died","Survived")

fisher.test(tab_hbp)$p.value

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(high_blood_pressure, d)) %>% 
  pull(r)

##########################################################
# Platelets
##########################################################
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=platelets, fill=DEATH_EVENT)) +
  geom_boxplot()

heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=platelets, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(platelets, d)) %>% 
  pull(r)

##########################################################
# Serum Creatinine
##########################################################
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=serum_creatinine, fill=DEATH_EVENT)) +
  geom_boxplot()

heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=serum_creatinine, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(serum_creatinine, d)) %>% 
  pull(r)

##########################################################
# Serum Sodium
##########################################################
heartfailure %>%
  group_by(DEATH_EVENT) %>%
  ggplot(aes(x=DEATH_EVENT, y=serum_sodium, fill=DEATH_EVENT)) +
  geom_boxplot()

heartfailure %>%
  group_by(DEATH_EVENT)%>%
  ggplot(aes(x=serum_sodium, fill=DEATH_EVENT)) +
  geom_density(alpha=0.5)

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(serum_sodium, d)) %>% 
  pull(r)

##########################################################
# Sex
##########################################################
df_sex <- data.frame(sex=rep(c("0","1"),each=2),
                     D=rep(c("0","1"),2),
                     count=c(sum(heartfailure$DEATH_EVENT[heartfailure$sex==0]==0),
                             sum(heartfailure$DEATH_EVENT[heartfailure$sex==0]==1),
                             sum(heartfailure$DEATH_EVENT[heartfailure$sex==1]==0),
                             sum(heartfailure$DEATH_EVENT[heartfailure$sex==1]==1)))
df_sex %>%
  ggplot(aes(x=D,y=count,fill=sex)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)

tab_sex <- matrix(c(62,132,34,71),2,2)
rownames(tab_sex)<-c("Male","Female")
colnames(tab_sex)<-c("Died","Survived")

fisher.test(tab_sex)$p.value

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(sex, d)) %>% 
  pull(r)

##########################################################
# Smoking
##########################################################
df_smoking <- data.frame(smoking=rep(c("0","1"),each=2),
                     D=rep(c("0","1"),2),
                     count=c(sum(heartfailure$DEATH_EVENT[heartfailure$smoking==0]==0),
                             sum(heartfailure$DEATH_EVENT[heartfailure$smoking==0]==1),
                             sum(heartfailure$DEATH_EVENT[heartfailure$smoking==1]==0),
                             sum(heartfailure$DEATH_EVENT[heartfailure$smoking==1]==1)))
df_smoking %>%
  ggplot(aes(x=D,y=count,fill=smoking)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)

tab_smoking <- matrix(c(30,66,66,137),2,2)
rownames(tab_smoking)<-c("Smoking","Without Smoking")
colnames(tab_smoking)<-c("Died","Survived")

fisher.test(tab_smoking)$p.value

heartfailure %>% 
  mutate(d=as.numeric(DEATH_EVENT)) %>%
  summarize(r = cor(smoking, d)) %>% 
  pull(r)