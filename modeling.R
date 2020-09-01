###### EDA and Modeling Process ######

set.seed(1991)

library(tidyverse)
library(modelr)
library(broom)
library(caret)

## LASSO
# install.packages("glmnet")
library(glmnet)

## Plotting correlations
#install.packages("ggcorrplot")
library(ggcorrplot)

# Partitioning ------------------------------------------------------------

# Partitioning dataset into train and test
# 20% for testing (~75 observations, 5 years worth of data), 80% training (20 years worth of data)
series <- resample_partition(series_adjusted, c(train = 0.8, test = 0.2))
series_train <- as_tibble(series$train)
series_test <- as_tibble(series$test)

# Creating a second series dataset with refernce information, redundant and unlikely variables removed
series_2 <- series_adjusted %>%
  select(-c(1:7,9:14,Pts_Reg_tm_1,SoW_Reg_tm_1,SoL_Reg_tm_1,GF_Reg_tm_1,GA_Reg_tm_1,GD_Reg_tm_1, 
            name_Reg_tm_1, PIM_Reg_tm_1,PPG_Reg_tm_1,SHA_Reg_tm_1,PKG_Reg_tm_1,SHF_Reg_tm_1,
            EVGF_Reg_tm_1,EVGA_Reg_tm_1, EVGD_Reg_tm_1,
            franchID_Reg_tm_2,confID_Reg_tm_2,divID_Reg_tm_2,rank_Reg_tm_2,playoff_Reg_tm_2,
            G_Reg_tm_2,Pts_Reg_tm_2,SoW_Reg_tm_2,SoL_Reg_tm_2,GF_Reg_tm_2,GA_Reg_tm_2,GD_Reg_tm_2,
            name_Reg_tm_2,PIM_Reg_tm_2,PPG_Reg_tm_2,SHA_Reg_tm_2,PKG_Reg_tm_2,SHF_Reg_tm_2,
            EVGF_Reg_tm_2,EVGA_Reg_tm_2,EVGD_Reg_tm_2))
# Partitioning dataset into train and test
series_2 <- resample_partition(series_2, c(train = 0.8, test = 0.2))
series_2_train <- as_tibble(series_2$train)
series_2_test <- as_tibble(series_2$test)
# Adding binary outcome variable to compare with predicitons
series_2_train_tmp <- series_2_train %>%
  mutate(outcome_binary = as.factor(ifelse(outcome >= "Win in 7",1 , 0)))
series_2_test_tmp <- series_2_test %>%
  mutate(outcome_binary = as.factor(ifelse(outcome >= "Win in 7",1 , 0)))



# EDA ---------------------------------------------------------------------

# Developing correlation dataframe with a numeric version of outcome_binary
cor_tmp_df <- series_2_train %>%
  mutate(outcome_binary = ifelse(outcome >= "Win in 7",1 , 0)) %>%
  dplyr::select(-contains("tm_2")) %>%
  dplyr::select(-outcome) 
cor_tmp_df <- cor_tmp_df %>%
  dplyr::select_if(cor(.,cor_tmp_df$outcome_binary) > 0.2 | cor(.,cor_tmp_df$outcome_binary) < -0.2)
# Plotting Correlation matrix of the strongest correlations with outcome_binary
cor_plot_outcome_binary <- ggcorrplot(cor(cor_tmp_df),hc.order = TRUE)

# Plotting All Stars versus PtsPer
boxplot_PtsPer_AllStars <- series_2_train %>%
  ggplot(aes(as.factor(AllStars_Reg_tm_1), PtsPer_Reg_tm_1, group = as.factor(AllStars_Reg_tm_1))) +
  geom_boxplot(aes(fill = as.factor(AllStars_Reg_tm_1))) +
  labs(title = "Points Percentage vs Number of All Stars", x = "All Stars", y = "Points %") + 
  theme(legend.position = "none")

# SavePer versus PtsPer
dotplot_PtsPer_SavePer <- series_2_train %>% 
  filter(SavePer_Reg_tm_1 > 0.85) %>%
  ggplot(aes(SavePer_Reg_tm_1,PtsPer_Reg_tm_1)) +
  geom_point(aes(alpha=0.1)) +
  geom_smooth() +
  theme(legend.position = "none") +
  labs(title = "Pts% vs Save%")

# Density Diff in 2nd half pts grouped by outcome binary
density_second_half <- series_2_train_tmp %>%
  ggplot(aes(Second_Half_Pts_Per_Reg_tm_1 - Second_Half_Pts_Per_Reg_tm_2, group = outcome_binary)) +
  geom_density(aes(fill = outcome_binary)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "Difference in 2nd Half Pts by series outcome", 
       x = "Differnce in 2nd Half Pts")

# correlation matrix of luck, PtsPer,GDperG
cor_tmp_2 <- series_2_train %>%
  dplyr::select(c(PtsPer_Reg_tm_1,GDperG_Reg_tm_1,luck_Reg_tm_1)) 
                  
cor_plot_luck <- ggcorrplot(cor(cor_tmp_2),hc.order = TRUE)

# Count of outcomes facet wrapped by home field advantage
# series_2_train_tmp %>%
#   ggplot(aes(x = outcome, fill = outcome_binary)) +
#   geom_bar() +
#   facet_wrap(~ Home_tm_1) 

boxplot_home_outcomes <- series_2_train_tmp %>%
  ggplot(aes(x = outcome, fill = as.factor(Home_tm_1))) +
  geom_bar(position = "dodge") +
  labs(fill = "Home Team") +
  labs(title = "Home vs Away Outcomes")
  

# MAE CV LASSO (k = 10) ---------------------------------------------------------------

# Running a single LASSO fit
# grouped option keeps coefficients values same for each category 
# glmnet only accepts matrices for the predictors
glmnet_fit <- glmnet(as.matrix(series_2_train[,2:ncol(series_2_train)]), series_2_train$outcome,
                     family = 'multinomial', type.multinomial = "grouped")
# Cross validaiton LASSO: using mae as the plot score to minimize 
# Warnings occur, but only to show that more optimizaiton isn't neccesary
glmnet_cv_fit_mae <- cv.glmnet(as.matrix(series_2_train[,2:ncol(series_2_train)]), series_2_train$outcome,
                           family = 'multinomial',type.multinomial = "grouped", type.measure = "mae")
glmnet_cv_lamba_vs_mae <- plot(glmnet_cv_fit_mae)


### lambda = glmnet_cv_fit$lambda.min 
pred_glmnet_cv_response_mae_2 <- predict(glmnet_cv_fit_mae,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                   s = glmnet_cv_fit_mae$lambda.min , type = "response")

glmnet_cv_coef_mae_2 <- coef(glmnet_cv_fit_mae, s = glmnet_cv_fit_mae$lambda.min)

pred_glmnet_cv_response_mae_2 <- as_tibble(pred_glmnet_cv_response_mae_2[,1:8,1])
pred_glmnet_cv_response_mae_2 <- pred_glmnet_cv_response_mae_2 %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_2$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_2$outcome_pred_binary)

glmnet_cv_train_acc_mae_2 <- confusionMatrix(pred_glmnet_cv_response_mae_2$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]


### lambda = glmnet_cv_fit_mae$lambda.1se (most regularized within 1 SE of min) 
pred_glmnet_cv_response_mae_3 <- predict(glmnet_cv_fit_mae,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = glmnet_cv_fit_mae$lambda.1se , type = "response")

glmnet_cv_coef_mae_3 <- coef(glmnet_cv_fit_mae, s = glmnet_cv_fit_mae$lambda.1se)

resp_tmp <- pred_glmnet_cv_response_mae_3[,1:8,1]
resp_tmp <- as_tibble(resp_tmp)
resp_tmp <- resp_tmp %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
resp_tmp$outcome_pred_binary <- as.factor(resp_tmp$outcome_pred_binary)

glmnet_cv_train_acc_mae_3 <- confusionMatrix(resp_tmp$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]

### Lambda = lambda_mae_last (including nearly every variable) 
lambda_mae_last = glmnet_cv_fit_mae$lambda[length(glmnet_cv_fit_mae$lambda)]

pred_glmnet_cv_response_mae_6 <- predict(glmnet_cv_fit_mae,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = lambda_mae_last , type = "response")

glmnet_cv_coef_mae_6 <- coef(glmnet_cv_fit_mae, s = lambda_mae_last)

pred_glmnet_cv_response_mae_6 <- as_tibble(pred_glmnet_cv_response_mae_6[,1:8,1])
pred_glmnet_cv_response_mae_6 <- pred_glmnet_cv_response_mae_6 %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_6$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_6$outcome_pred_binary)

glmnet_cv_train_acc_mae_6 <- confusionMatrix(pred_glmnet_cv_response_mae_6$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]

### Lambda = 0.05418944 (smallest standard deviation on graph)
pred_glmnet_cv_response_mae_7 <- predict(glmnet_cv_fit_mae,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = 0.05418944 , type = "response")

glmnet_cv_coef_mae_7 <- coef(glmnet_cv_fit_mae, s = 0.05418944)

pred_glmnet_cv_response_mae_7 <- as_tibble(pred_glmnet_cv_response_mae_7[,1:8,1])
pred_glmnet_cv_response_mae_7 <- pred_glmnet_cv_response_mae_7 %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_7$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_7$outcome_pred_binary)

glmnet_cv_train_acc_mae_7 <- confusionMatrix(pred_glmnet_cv_response_mae_7$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]


# MAE CV LASSO (k = 20) ---------------------------------------------------

# Setting number of folds k = 20
glmnet_cv_fit_mae_2 <- cv.glmnet(as.matrix(series_2_train[,2:ncol(series_2_train)]), series_2_train$outcome,
                               family = 'multinomial',type.multinomial = "grouped", type.measure = "mae",
                               nfolds = 20)
glmnet_cv_lamba_vs_mae_2 <- plot(glmnet_cv_fit_mae_2)


### Lambda = glmnet_cv_fit$lambda.min, k = 20  
pred_glmnet_cv_response_mae_4 <- predict(glmnet_cv_fit_mae_2,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = glmnet_cv_fit_mae_2$lambda.min , type = "response")

glmnet_cv_coef_mae_4 <- coef(glmnet_cv_fit_mae_2, s = glmnet_cv_fit_mae_2$lambda.min)

pred_glmnet_cv_response_mae_4 <- pred_glmnet_cv_response_mae_4[,1:8,1]
pred_glmnet_cv_response_mae_4 <- as_tibble(pred_glmnet_cv_response_mae_4)
pred_glmnet_cv_response_mae_4 <- pred_glmnet_cv_response_mae_4 %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_4$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_4$outcome_pred_binary)

glmnet_cv_train_acc_mae_4 <- confusionMatrix(pred_glmnet_cv_response_mae_4$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]

### lambda = glmnet_cv_fit$lambda.1se, k = 20 
pred_glmnet_cv_response_mae_8 <- predict(glmnet_cv_fit_mae_2,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
                                         s = glmnet_cv_fit_mae_2$lambda.1se , type = "response")

glmnet_cv_coef_mae_8 <- coef(glmnet_cv_fit_mae_2, s = glmnet_cv_fit_mae_2$lambda.1se)

pred_glmnet_cv_response_mae_8 <- pred_glmnet_cv_response_mae_8[,1:8,1]
pred_glmnet_cv_response_mae_8 <- as_tibble(pred_glmnet_cv_response_mae_8)
pred_glmnet_cv_response_mae_8 <- pred_glmnet_cv_response_mae_8 %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_8$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_8$outcome_pred_binary)

glmnet_cv_train_acc_mae_8 <- confusionMatrix(pred_glmnet_cv_response_mae_8$outcome_pred_binary,
                                             series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]



# MAE CV LASSO (k = 60) -----------------------------------------------------

### commented out to avoid unnecessary computation slowing.

# Setting number of folds k = 60
# glmnet_cv_fit_mae_3 <- cv.glmnet(as.matrix(series_2_train[,2:ncol(series_2_train)]), series_2_train$outcome,
#                                  family = 'multinomial',type.multinomial = "grouped", type.measure = "mae",
#                                  nfolds = 60)
# glmnet_cv_lamba_vs_mae_3 <- plot(glmnet_cv_fit_mae_3)
# 
# ### Lambda = glmnet_cv_fit$lambda.min, k = 60  (65.2%)
# pred_glmnet_cv_response_mae_5 <- predict(glmnet_cv_fit_mae_3,as.matrix(series_2_train[,2:ncol(series_2_train)]),
#                                          s = glmnet_cv_fit_mae_3$lambda.min , type = "response")
# 
# glmnet_cv_coef_mae_5 <- coef(glmnet_cv_fit_mae_3, s = glmnet_cv_fit_mae_3$lambda.min)
# 
# resp_tmp <- pred_glmnet_cv_response_mae_5[,1:8,1]
# resp_tmp <- as_tibble(resp_tmp)
# resp_tmp <- resp_tmp %>%
#   mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
#   mutate(lose_per = 1 - win_per) %>%
#   mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
# resp_tmp$outcome_pred_binary <- as.factor(resp_tmp$outcome_pred_binary)
# 
# glmnet_cv_train_acc_mae_5 <- confusionMatrix(resp_tmp$outcome_pred_binary,
#                                              series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]
# 

# MSE CV LASSO (k = 10) --------------------------------------------------------------

#### Commentted out for computation speed

# # Cross validaiton LASSO: using mse as the plot score to minimize 
# glmnet_cv_fit_mse <- cv.glmnet(as.matrix(series_2_train[,2:ncol(series_2_train)]), series_2_train$outcome,
#                                family = 'multinomial',type.multinomial = "grouped", type.measure = "mse")
# glmnet_cv_lamba_vs_mse <- plot(glmnet_cv_fit_mse)
# 
# 
# ### lambda = glmnet_cv_fit_mse$lambda.min (lambda value with minimum mse) 
# pred_glmnet_cv_response_mse_1 <- predict(glmnet_cv_fit_mse,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
#                                    s = glmnet_cv_fit_mse$lambda.min , type = "response")
# 
# glmnet_cv_coef_mse_1 <- coef(glmnet_cv_fit_mse, s = glmnet_cv_fit_mse$lambda.min)
# 
# resp_tmp_mse <- pred_glmnet_cv_response_mse_1[,1:8,1]
# resp_tmp_mse <- as_tibble(resp_tmp_mse)
# resp_tmp_mse <- resp_tmp_mse %>%
#   mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
#   mutate(lose_per = 1 - win_per) %>%
#   mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
# resp_tmp_mse$outcome_pred_binary <- as.factor(resp_tmp_mse$outcome_pred_binary)
# 
# glmnet_cv_train_acc_mse_1 <- confusionMatrix(resp_tmp_mse$outcome_pred_binary,
#                                              series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]
# 
# 
# ### lambda = lambda_mse_last (including most variables) 
# lambda_mse_last <- glmnet_cv_fit_mse$lambda[length(glmnet_cv_fit_mse$lambda)]
# 
# pred_glmnet_cv_response_mse_2 <- predict(glmnet_cv_fit_mse,as.matrix(series_2_train[,2:ncol(series_2_train)]), 
#                                          s = lambda_mse_last , type = "response")
# 
# glmnet_cv_coef_mse_2 <- coef(glmnet_cv_fit_mse, s = lambda_mse_last)
# 
# resp_tmp_mse <- pred_glmnet_cv_response_mse_2[,1:8,1]
# resp_tmp_mse <- as_tibble(resp_tmp_mse)
# resp_tmp_mse <- resp_tmp_mse %>%
#   mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
#   mutate(lose_per = 1 - win_per) %>%
#   mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
# resp_tmp_mse$outcome_pred_binary <- as.factor(resp_tmp_mse$outcome_pred_binary)
# 
# glmnet_cv_train_acc_mse_2 <- confusionMatrix(resp_tmp_mse$outcome_pred_binary,
#                                              series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]
# 


# trivial prediciton ------------------------------------------------------

# Computing trival predictions (picking team with more points during regular season to win)
pred_trivial_series_2_train <- as.factor(ifelse(
  series_2_train$PtsPer_Reg_tm_1 > series_2_train$PtsPer_Reg_tm_2, 1, 0))

pred_trivial_acc <- confusionMatrix(pred_trivial_series_2_train,series_2_train_tmp$outcome_binary)$overall[["Accuracy"]]
# Testing model and visualizations -----------------------------------------------------------------

# Accuracy Dataframe
acc_df <- tibble(
  model = c("Lambda Min", "Lambda 1SE", "Trivial"), 
  train_acc = c(glmnet_cv_train_acc_mae_4,glmnet_cv_train_acc_mae_8,pred_trivial_acc),
  test_acc = c(0,0,0)
  )

# Creating a Training Accuracy Bar Chart
bar_chart_train_acc <- acc_df %>%
  ggplot(aes(reorder(model,train_acc),train_acc)) +
  geom_col(aes(fill = model)) +
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1)) +
  labs(title = "Training Accuracy", y = "Training Accuracy", x = "Model") +
  theme(legend.position = "none")

### lambda = glmnet_cv_fit$lambda.min, k = 20  
pred_glmnet_cv_response_mae_4_test <- predict(glmnet_cv_fit_mae_2,as.matrix(series_2_test[,2:ncol(series_2_test)]), 
                                         s = glmnet_cv_fit_mae_2$lambda.min , type = "response")
pred_glmnet_cv_response_mae_4_test <- pred_glmnet_cv_response_mae_4_test[,1:8,1]
pred_glmnet_cv_response_mae_4_test <- as_tibble(pred_glmnet_cv_response_mae_4_test)
pred_glmnet_cv_response_mae_4_test <- pred_glmnet_cv_response_mae_4_test %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_4_test$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_4_test$outcome_pred_binary)

glmnet_cv_test_acc_mae_4 <- confusionMatrix(pred_glmnet_cv_response_mae_4_test$outcome_pred_binary,
                                             series_2_test_tmp$outcome_binary)$overall[["Accuracy"]]

### lambda = glmnet_cv_fit$lambda.1se, k = 20 
pred_glmnet_cv_response_mae_8_test <- predict(glmnet_cv_fit_mae_2,as.matrix(series_2_test[,2:ncol(series_2_test)]), 
                                         s = glmnet_cv_fit_mae_2$lambda.1se , type = "response")
pred_glmnet_cv_response_mae_8_test <- pred_glmnet_cv_response_mae_8_test[,1:8,1]
pred_glmnet_cv_response_mae_8_test <- as_tibble(pred_glmnet_cv_response_mae_8_test)
pred_glmnet_cv_response_mae_8_test <- pred_glmnet_cv_response_mae_8_test %>%
  mutate(win_per = `Win in 7` + `Win in 6` + `Win in 5` + `Win in 4`) %>%
  mutate(lose_per = 1 - win_per) %>%
  mutate(outcome_pred_binary = ifelse(win_per > lose_per, 1, 0))
pred_glmnet_cv_response_mae_8_test$outcome_pred_binary <- as.factor(pred_glmnet_cv_response_mae_8_test$outcome_pred_binary)

glmnet_cv_test_acc_mae_8 <- confusionMatrix(pred_glmnet_cv_response_mae_8_test$outcome_pred_binary,
                                             series_2_test_tmp$outcome_binary)$overall[["Accuracy"]]

### Trivial model accuracy 
pred_trivial_series_2_test <- as.factor(ifelse(
  series_2_test$PtsPer_Reg_tm_1 > series_2_test$PtsPer_Reg_tm_2, 1, 0))

pred_trivial_acc_test <- confusionMatrix(pred_trivial_series_2_test,
                                         series_2_test_tmp$outcome_binary)$overall[["Accuracy"]]

# Updating accuracy dataframe
acc_df$test_acc = c(glmnet_cv_test_acc_mae_4,glmnet_cv_test_acc_mae_8,pred_trivial_acc_test)
acc_df  

# Bar Chart Test Accuracy
bar_chart_test_acc <- acc_df %>%
  ggplot(aes(reorder(model,train_acc),test_acc)) +
  geom_col(aes(fill = model)) +
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,0.7)) +
  labs(title = "Test Accuracy", y = "Test Accuracy", x = "Model") +
  theme(legend.position = "none")

# coefficients
coef_lambda.1SE_2 <- tibble(
  coefficient = c(row.names(glmnet_cv_coef_mae_8$`Lose in 4`),
                  row.names(glmnet_cv_coef_mae_8$`Lose in 4`)),
  value = c(glmnet_cv_coef_mae_8$`Lose in 4`[1:length(glmnet_cv_coef_mae_8$`Lose in 4`)],
            glmnet_cv_coef_mae_8$`Win in 4`[1:length(glmnet_cv_coef_mae_8$`Win in 4`)]),
  class = rep(c("Losein4","Winin4"), each = length(glmnet_cv_coef_mae_8$`Lose in 4`))
)
coef_lambda.1SE_2 <- coef_lambda.1SE_2 %>%
  filter(value !=0)

dotplot_coef <- coef_lambda.1SE_2 %>%
  ggplot(aes(coefficient,value, color = class)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45))

