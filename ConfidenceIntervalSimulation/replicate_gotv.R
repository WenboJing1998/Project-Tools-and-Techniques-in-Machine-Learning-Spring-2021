```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
if (!require("devtools")) install.packages("devtools")
devtools::install_github("soerenkuenzel/causalToolbox")

library(causalToolbox)
library(forestry)
head(gotv)

set.seed(30019455)
train_ids <- sample.int(nrow(gotv), 1000)
test_ids <- sample((1:nrow(gotv))[-train_ids], 5)
feat <- gotv[train_ids , -(8:9)]
tr <- gotv$treatment[train_ids]
yobs <- gotv$voted[train_ids]

# Create a T_RF learner with given features, treatments, and observations
t_rf <- T_RF(feat = feat, tr = tr, yobs = yobs)

feat_test <- gotv[test_ids ,]

# Estimate CATE with a given learner and test data
cate_t_rf <- EstimateCate(t_rf, gotv[test_ids, -(8:9)])

# calculate bias
# Calculate the estimated confidence interval for the CATE
cateCI <- CateCI(t_rf, gotv[test_ids, -(8:9)], B = 2, verbose = FALSE)
cateCI


s_rf <- S_RF(feat = feat, tr = tr, yobs = yobs)
cate_s_rf <- EstimateCate(s_rf, gotv[test_ids, -(8:9)])
x_bart <- X_BART(feat = feat, tr = tr, yobs = yobs)
cate_x_bart <- EstimateCate(x_bart, gotv[test_ids, -(8:9)])
t_bart <- T_BART(feat = feat, tr = tr, yobs = yobs)
cate_t_bart <- EstimateCate(t_bart, gotv[test_ids, -(8:9)])
cbind(cate_t_rf, cate_s_rf, cate_x_bart, cate_t_bart)


# use T-RF to generate CATE estimate on all datasets - treat them as ground truth
total_dataset_id <- 1:nrow(gotv)
feat_total <- gotv[total_dataset_id, -(8:9)]
tr_total <- gotv$treatment[total_dataset_id]
yobs_total <- gotv$voted[total_dataset_id]
t_rf_total <- T_RF(feat = feat_total, tr = tr_total, yobs = yobs_total)
#saveRDS(t_rf_total, "t_rf_total.")
cate_t_rf_total <- EstimateCate(t_rf_total, gotv[total_dataset_id, -(8:9)])
#saveRDS(cate_t_rf_total, "cate_t_rf_total.rds")
cate_t_rf_total <- readRDS("cate_t_rf_total.rds")

total_dataset_pos <- total_dataset_id[gotv$treatment==1]
total_dataset_neg <- total_dataset_id[gotv$treatment==0]

#training_size <- c(10,20,30,40,50,60)*1000
training_size <- c(0.5, 1, 2, 3, 4, 5)*1000
training_size <- c(training_size,  c(10,20,30,40,50,60)*1000)
#training_size <- c(1000)
var_l <- data.frame(matrix(ncol = 3, nrow = 0))
bias_l <- data.frame(matrix(ncol = 3, nrow = 0))
rmse_l <- data.frame(matrix(ncol = 3, nrow = 0))

for(i in 1:length(training_size)){
  n <- training_size[i]
  #n <- 10000
  pos_tr_n <- floor(n*0.167)
  neg_tr_n <- floor(n*(1-0.167))
  
  train_ids <- c(sample(total_dataset_pos, pos_tr_n), sample(total_dataset_neg, neg_tr_n))
  
  test_ids <- total_dataset_id[-train_ids]
  
  ground_truth <- cate_t_rf_total[test_ids]
  
  feat_tr <- gotv[train_ids , -(8:9)]
  tr_tr <- gotv$treatment[train_ids]
  yobs_tr <- gotv$voted[train_ids]
  
#  t_rf <- T_RF(feat = feat_tr, tr = tr_tr, yobs = yobs_tr)
#  cate_t_rf <- EstimateCate(t_rf, gotv[test_ids, -(8:9)])
  
#  s_rf <- S_RF(feat = feat_tr, tr = tr_tr, yobs = yobs_tr)
#  cate_s_rf <- EstimateCate(s_rf, gotv[test_ids, -(8:9)])
  
#  x_rf <- X_RF(feat = feat_tr, tr = tr_tr, yobs = yobs_tr)
#  cate_x_rf <- EstimateCate(x_rf, gotv[test_ids, -(8:9)])
  
#  x_bart <- X_BART(feat = feat, tr = tr, yobs = yobs)
#  cate_x_bart <- EstimateCate(x_bart, gotv[test_ids, -(8:9)])
  
#  t_bart <- T_BART(feat = feat, tr = tr, yobs = yobs)
#  cate_t_bart <- EstimateCate(t_bart, gotv[test_ids, -(8:9)])
  
  result <- cbind(cate_t_rf, cate_s_rf, cate_x_rf)
  #colMeans(abs(result - ground_truth))
  #sapply(data.frame(result), var)
  #sapply(data.frame(result), 
  #       function(x) mean(abs(x - ground_truth) ** 2) ** 0.5)
  filename <- sprintf("result_%s.rds", n)
#  saveRDS(result, filename)
  result <- readRDS(filename)
  bias_l <- rbind(bias_l, colMeans(abs(result - ground_truth)))
  var_l <- rbind(var_l, sapply(data.frame(result), var))
  rmse_l <- rbind(rmse_l, sapply(data.frame(result), 
                                 function(x) mean(abs(x - ground_truth) ** 2) ** 0.5))
}

colnames(var_l) <- c("cate_t_rf", "cate_s_rf", "cate_x_rf")
colnames(bias_l) <- c("cate_t_rf", "cate_s_rf", "cate_x_rf")
colnames(rmse_l) <- c("cate_t_rf", "cate_s_rf", "cate_x_rf")

var_l <- var_l[2:12, ]
bias_l <- bias_l[2:12, ]
rmse_l <- rmse_l[2:12, ]
training_size <- training_size[2:12]
# varying training size to see the difference in variance, bias, and RMSE

library(ggplot2)
library("tidyverse")
library("reshape2")

variance_df <- data.frame(training_size, var_l)
variance_df <-melt(variance_df, id="training_size")

ggplot(data=variance_df,
       aes(x=training_size, y=value, colour=variable)) +
  geom_line(size=0.5, aes(linetype=variable))+
  labs(x="Training Size", y = "Average Variance")+ theme_bw()+
  geom_point(aes(shape=variable, color=variable))

ggsave("Average Variance.png", path =getwd(), device='png', dpi=320)

bias_df <- data.frame(training_size, bias_l)
bias_df <-melt(bias_df, id="training_size")
ggplot(data=bias_df,
       aes(x=training_size, y=value, colour=variable)) +
  geom_line(size=0.5, aes(linetype=variable))+
  labs(x="Training Size", y = "Average Bias")+ theme_bw() + 
  geom_point(aes(shape=variable, color=variable))
ggsave("Average Bias.png", path =getwd(), device='png', dpi=320)

rmse_df <- data.frame(training_size, rmse_l)
rmse_df <-melt(rmse_df, id="training_size")
ggplot(data=rmse_df,
       aes(x=training_size, y=value, colour=variable)) +
  geom_line(size=0.5, aes(linetype=variable, color=variable))+
  labs(x="Training Size", y = "RMSE")+ theme_bw()+
  geom_point(aes(shape=variable, color=variable))
ggsave("RMSE.png", path =getwd(), device='png', dpi=320)
