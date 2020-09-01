##### Inference Modeling #####

### Econometrics 2 ###

# Required Packages
library(tidyverse)
library(modelr)
library(broom)
library(caret)

## Packages for Methods
# Ordinal Probit/Logit
# install.packages("MASS")
# library(MASS)
# psuedo-R^2
# install.packages("pscl")
library(pscl)


# Initial Datasets ----------------------------------------------------------------

# Initial Data
series_infernece <- series_adjusted
series_infernece <- series_infernece %>%
  mutate(outcome_binary_tm_1 = as.factor(ifelse(W_tm_1 > W_tm_2, 1, 0))) %>% 
  mutate(outcome_binary_tm_2 = as.factor(ifelse(W_tm_2 > W_tm_1, 1, 0))) %>%
  select(-outcome) %>%
  mutate(Home_tm_2 = ifelse(Home_tm_1 == 1, 0, 1))

# mutate(outcome_binary_num = ifelse(W_tm_1 > W_tm_2, 1, 0))

# Separating series into separate teams for analysis on each team indivdually
df_tm1 <- series_infernece %>%
  select(contains("tm_1"))
df_tm1 <- cbind(series_infernece[,1:3],df_tm1)
df_tm1 <- as_tibble(df_tm1)

df_tm2 <- series_infernece %>%
  select(contains("tm_2"))
df_tm2 <- cbind(series_infernece[,1:3],df_tm2)
df_tm2 <- as_tibble(df_tm2)

colnames(df_tm1) <- sub("_tm_1","",colnames(df_tm1))
colnames(df_tm2) <- sub("_tm_2","",colnames(df_tm2))

series_inf_2 <- rbind(df_tm1,df_tm2)

# Numeric only dataset for modeling purposes
series_full <- series_inf_2 %>%
  # removing refernce info, redudant info
  select(-c(1:11,name_Reg, GFperG_Reg,GAperG_Reg,GDperG_Reg,EVGFperG_Reg,EVGAperG_Reg,EVGDperG_Reg,
            PtsPer_Reg,PIMperG_Reg,TGperG_Reg))
series_full <- series_full %>%
  # Removing multicollinearity issues
  select(-c(SoW_Reg,SoL_Reg,T_Reg,MVP_Reg,FirstTeamAllStars_Reg))

colnames(series_full) <- sub("_Reg","",colnames(series_full))

# logit models ---------------------------------------------------

# Initial model
logit_01 <- glm(outcome_binary ~ ., data = series_full, family = "binomial")
summary(logit_01)
pR2(logit_01)

# Removiing multicollinearity issues (MODEL 1)
series_full_02 <- series_full %>%
  select(-c(GA,EVGA,EVGF,PPG))

logit_02 <- glm(outcome_binary ~ ., data = series_full_02, family = "binomial")
summary(logit_02)
pR2(logit_02)

# Removing bad predictors (> 0.9 p value)
series_full_03 <- series_full_02 %>%
  select(-c(BenchMinor,SHA))

logit_03 <- glm(outcome_binary ~ ., data = series_full_03, family = "binomial")
summary(logit_03)
pR2(logit_03)

# Removing W,L,OTL (MODEL 2)
series_full_04 <- series_full_03 %>%
  select(-c(W,L,OTL))

logit_04 <- glm(outcome_binary ~ ., data = series_full_04, family = "binomial")
summary(logit_04)
pR2(logit_04)

# Removing bad predictors (intuiton, > 0.75 p value) (model 3)
series_full_05 <- series_full_04 %>%
  select(-c(GF,PIM,PPC,SHF))

logit_05 <- glm(outcome_binary ~ ., data = series_full_05, family = "binomial")
summary(logit_05)
pR2(logit_05)
pred_logit_05 <- predict(logit_05,series_full,type = "response")
pred_binary_logit_05 <-as.factor(ifelse(pred_logit_05 > 0.5, 1, 0))
confusionMatrix(pred_binary_logit_05,series_full$outcome_binary)

# Interaction between PKG (PK goals allowed) and SavePer (Model 4)
logit_06 <- glm(outcome_binary ~ . + SavePer * PKG, data = series_full_05, family = "binomial")
summary(logit_06)
pR2(logit_06)
pred_logit_06 <- predict(logit_06,series_full,type = "response")
pred_binary_logit_06 <-as.factor(ifelse(pred_logit_06 > 0.5, 1, 0))
confusionMatrix(pred_binary_logit_06,series_full$outcome_binary)

# removing SavePer for F-test
series_full_09 <- series_full_05 %>%
  select(-c(SavePer,PKG))

logit_10 <- glm(outcome_binary ~ . , data = series_full_09, family = "binomial")
summary(logit_10)
pR2(logit_10)
pred_logit_10 <- predict(logit_10,series_full,type = "response")
pred_binary_logit_10 <-as.factor(ifelse(pred_logit_10 > 0.5, 1, 0))
confusionMatrix(pred_binary_logit_10,series_full$outcome_binary)

# F-test (SavePer, PKG, interaction)
anova(logit_10,logit_06,test = "Chisq")
cor(series_full_07$PKG,series_full_07$SavePer)

# Adding GA for interaction analysis (interaction is worse)
series_full_06 <- series_full_05 %>%
  mutate(GA = series_full$GA)

logit_07 <- glm(outcome_binary ~ . + SavePer * GA, data = series_full_06, family = "binomial")
summary(logit_07)
pR2(logit_07)

# Removing (intuition)
series_full_07 <- series_full_05 %>%
  select(-c(ROW,Last_Month_Pts_Per,SecondTeamAllStars))

logit_08 <- glm(outcome_binary ~ . + SavePer * PKG, data = series_full_07, family = "binomial")
summary(logit_08)
pR2(logit_08)
pred_logit_08 <- predict(logit_08,series_full,type = "response")
pred_binary_logit_08 <-as.factor(ifelse(pred_logit_08 > 0.5, 1, 0))
confusionMatrix(pred_binary_logit_08,series_full$outcome_binary)


# removing SavePer for F-test
series_full_08 <- series_full_07 %>%
  select(-SavePer)

logit_09 <- glm(outcome_binary ~ . , data = series_full_08, family = "binomial")
summary(logit_09)
pR2(logit_09)

# F-test
anova(logit_08,logit_09,test = "Chisq")
cor(series_full_07$PKG,series_full_07$SavePer)

# Continuing from logit_06, F-test on late season points
cor(series_full$Last_Month_Pts_Per, series_full$Second_Half_Pts_Per)

series_full_10 <- series_full_05 %>%
  select(-c(Last_Month_Pts_Per,Second_Half_Pts_Per))

logit_11 <- glm(outcome_binary ~ . + SavePer * PKG, data = series_full_10, family = "binomial")
summary(logit_11)
pR2(logit_11)

# F-test
anova(logit_11,logit_06,test = "Chisq")

# contintuing from logit_11, F-test on all stars 
series_full_11 <- series_full_10 %>%
  select(-c(SecondTeamAllStars,AllStars))

logit_12 <- glm(outcome_binary ~ . + SavePer * PKG, data = series_full_11, family = "binomial")
summary(logit_12)
pR2(logit_12)
pred_logit_12 <- predict(logit_12,series_full,type = "response")
pred_binary_logit_12 <-as.factor(ifelse(pred_logit_12 > 0.5, 1, 0))
confusionMatrix(pred_binary_logit_12,series_full$outcome_binary)

# F-test
anova(logit_12,logit_06,test = "Chisq")

# continuing from logit_12 
series_full_13 <- series_full_11 %>%
  select(-ROW)

logit_14 <- glm(outcome_binary ~ . + SavePer * PKG, data = series_full_13, family = "binomial")
summary(logit_14)
pR2(logit_14)

# continuing from logit_08 (Model 5)
series_full_12 <- series_full_07 %>%
  select(-Second_Half_Pts_Per)

logit_13 <- glm(outcome_binary ~ . + SavePer * PKG, data = series_full_12, family = "binomial")
summary(logit_13)
pR2(logit_13)
pred_logit_13 <- predict(logit_13,series_full,type = "response")
pred_binary_logit_13 <-as.factor(ifelse(pred_logit_13 > 0.5, 1, 0))
confusionMatrix(pred_binary_logit_13,series_full$outcome_binary)

cor(series_full$AllStars,series_full$GD)

# contonuining from logit_13 
series_full_13 <- series_full_12 %>%
  select(-Pts)

logit_14 <- glm(outcome_binary ~ . + SavePer * PKG, data = series_full_13, family = "binomial")
summary(logit_14)
pR2(logit_14)
pred_logit_14 <- predict(logit_14,series_full,type = "response")
pred_binary_logit_14 <-as.factor(ifelse(pred_logit_14 > 0.5, 1, 0))
confusionMatrix(pred_binary_logit_14,series_full$outcome_binary)





