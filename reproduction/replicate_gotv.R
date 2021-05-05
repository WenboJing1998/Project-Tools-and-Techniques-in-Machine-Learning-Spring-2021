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
cate_s_rf <- EstimateCate(s_rf, gotv[test_ids, ])
x_bart <- X_BART(feat = feat, tr = tr, yobs = yobs)
cate_x_bart <- EstimateCate(x_bart, gotv[test_ids, ])
t_bart <- T_BART(feat = feat, tr = tr, yobs = yobs)
cate_t_bart <- EstimateCate(t_bart, gotv[test_ids, ])
cbind(cate_t_rf, cate_s_rf, cate_x_bart, cate_t_bart)


# use T-RF to generate CATE estimate on all datasets - treat them as ground truth
total_dataset_id <- sample.int(nrow(gotv), 10000)
feat_total <- gotv[total_dataset_id, -(8:9)]
tr_total <- gotv$treatment[total_dataset_id]
yobs_total <- gotv$voted[total_dataset_id]
t_rf_total <- T_RF(feat = feat_total, tr = tr_total, yobs = yobs_total)
cate_t_rf_total <- EstimateCate(t_rf_total, gotv[total_dataset_id, -(8:9)])

training_size <- c(1000, 2000, 3000, 4000, 5000, 6000, 7000)
#training_size <- c(1000)
var_l <- data.frame(matrix(ncol = 4, nrow = 0))
bias_l <- data.frame(matrix(ncol = 4, nrow = 0))
rmse_l <- data.frame(matrix(ncol = 4, nrow = 0))

for(i in 1:length(training_size)){
  n <- training_size[i]
  train_ids <- sample.int(total_dataset_id, n)
  test_ids <- sample(total_dataset_id[-train_ids], 500)
  ground_truth <- EstimateCate(t_rf_total, gotv[test_ids, -(8:9)])
  feat <- gotv[train_ids , -(8:9)]
  tr <- gotv$treatment[train_ids]
  yobs <- gotv$voted[train_ids]
  t_rf <- T_RF(feat = feat, tr = tr, yobs = yobs)
  cate_t_rf <- EstimateCate(t_rf, gotv[test_ids, -(8:9)])
  
  s_rf <- S_RF(feat = feat, tr = tr, yobs = yobs)
  cate_s_rf <- EstimateCate(s_rf, gotv[test_ids, -(8:9)])
  x_bart <- X_BART(feat = feat, tr = tr, yobs = yobs)
  cate_x_bart <- EstimateCate(x_bart, gotv[test_ids, -(8:9)])
  t_bart <- T_BART(feat = feat, tr = tr, yobs = yobs)
  cate_t_bart <- EstimateCate(t_bart, gotv[test_ids, -(8:9)])
  
  result <- cbind(cate_t_rf, cate_s_rf, cate_x_bart, cate_t_bart)
  
  bias_l <- rbind(bias_l, colMeans(result - ground_truth))
  var_l <- rbind(var_l, sapply(data.frame(result), var))
  rmse_l <- rbind(rmse_l, sapply(data.frame(result), function(x) mean((x - ground_truth) ** 2) ** 0.5))
}
colnames(var_l) <- c("cate_t_rf", "cate_s_rf", "cate_x_bart", "cate_t_bart")
colnames(bias_l) <- c("cate_t_rf", "cate_s_rf", "cate_x_bart", "cate_t_bart")
colnames(rmse_l) <- c("cate_t_rf", "cate_s_rf", "cate_x_bart", "cate_t_bart")

# varying training size to see the difference in variance, bias, and RMSE

library(ggplot2)
library("tidyverse")
library("reshape2")
variance_df <- data.frame(training_size, var_l)
variance_df <-melt(variance_df, id="training_size")

ggplot(data=variance_df,
       aes(x=training_size, y=value, colour=variable)) +
  geom_line(size=0.75)+
  labs(title="Plot of variance",x="training size", y = "variance")+ theme_bw()

bias_df <- data.frame(training_size, bias_l)
bias_df <-melt(bias_df, id="training_size")
ggplot(data=bias_df,
       aes(x=training_size, y=value, colour=variable)) +
  geom_line(size=1)+
  labs(title="Plot of bias",x="training size", y = "bias")+ theme_bw()

rmse_df <- data.frame(training_size, rmse_l)
rmse_df <-melt(rmse_df, id="training_size")
ggplot(data=rmse_df,
       aes(x=training_size, y=value, colour=variable)) +
  geom_line(size=1)+labs(title="Plot of RMSE",x="training size", y = "RMSE")+ theme_bw()
