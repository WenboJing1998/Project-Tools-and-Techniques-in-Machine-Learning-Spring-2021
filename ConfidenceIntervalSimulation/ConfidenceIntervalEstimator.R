library(causalToolbox)
packageVersion("causalToolbox")
source('./TTML/FeatureConverter.R')

# For Testing
# Estimate_Learner_CI_Coverage(200, 200, 10, 2, "", NULL)
# Estimate_Learner_CI_Coverage(200, 200, 10, 2, "Discretization", parameter_data=list(k_bin = 8))
# Estimate_Learner_CI_Coverage(200, 200, 10, 2, "Binary", parameter_data=list(depth_bt = 3, type_root="mean"))

# For Simulation
# Estimate_Learner_CI_Coverage(5000, 1000, 10, 1000, type_data="", NULL)
# Estimate_Learner_CI_Coverage(5000, 1000, 10, 1000, type_data="Discretization", parameter_data=list(k_bin = 8))
# Estimate_Learner_CI_Coverage(5000, 1000, 10, 1000, type_data="Binary", parameter_data=list(depth_bt = 3, type_root="mean"))

# Test Simulation
# df_test = Full_Simulation(200, 200, 10, 2)

# Full Simulation
# Result01
# df_full = Full_Simulation(10000, 500, 10, 100)
# Result02
# df_full = Full_Simulation(10000, 500, 10, 500)
# Result03
# df_full = Full_Simulation(25000, 1000, 10, 100)

# vec_train_size5 = seq(1000, 5000, by=1000)
# vec_test_size5 = seq(50, 250, by=50)
# vec_trials5 = seq(1000, 5000, by=1000)
# list_rtn_5 = Bias_Full_Simulation(5, vec_train_size5, vec_test_size5, 10)

# vec_train_size10 = seq(1000, 10000, by=1000)
# vec_test_size10 = seq(50, 500, by=50)
# vec_trials10 = seq(1000, 10000, by=1000)
# list_rtn_10 = Bias_Full_Simulation(10, vec_train_size10, vec_test_size10, 10)

# vec_train_size15 = seq(1000, 15000, by=1000)
# vec_test_size15 = seq(50, 750, by=50)
# vec_trials15 = seq(1000, 15000, by=1000)
# list_rtn_15 = Bias_Full_Simulation(15, vec_train_size15, vec_test_size15, 10)

# vec_train_size20 = seq(1000, 20000, by=1000)
# vec_test_size20 = seq(50, 1000, by=50)
# vec_trials20 = seq(1000, 20000, by=1000)
# list_rtn_20 = Bias_Full_Simulation(20, vec_train_size20, vec_test_size20, 10)

Bias_Full_Simulation<- function(num_trials, vec_train_size, vec_test_size, d){
  
  vec_rmse_sl_rf = c()
  vec_rmse_sl_bart = c()
  vec_rmse_tl_rf = c()
  vec_rmse_tl_bart = c()
  vec_rmse_xl_rf = c()
  vec_rmse_xl_bart = c()
  
  vec_bias_sl_rf = c()
  vec_bias_sl_bart = c()
  vec_bias_tl_rf = c()
  vec_bias_tl_bart = c()
  vec_bias_xl_rf = c()
  vec_bias_xl_bart = c()
  
  vec_rmse_sl_rf_B = c()
  vec_rmse_sl_bart_B = c()
  vec_rmse_tl_rf_B = c()
  vec_rmse_tl_bart_B = c()
  vec_rmse_xl_rf_B = c()
  vec_rmse_xl_bart_B = c()
  
  vec_bias_sl_rf_B = c()
  vec_bias_sl_bart_B = c()
  vec_bias_tl_rf_B = c()
  vec_bias_tl_bart_B = c()
  vec_bias_xl_rf_B = c()
  vec_bias_xl_bart_B = c()
  
  vec_rmse_sl_rf_K = c()
  vec_rmse_sl_bart_K = c()
  vec_rmse_tl_rf_K = c()
  vec_rmse_tl_bart_K = c()
  vec_rmse_xl_rf_K = c()
  vec_rmse_xl_bart_K = c()
  
  vec_bias_sl_rf_K = c()
  vec_bias_sl_bart_K = c()
  vec_bias_tl_rf_K = c()
  vec_bias_tl_bart_K = c()
  vec_bias_xl_rf_K = c()
  vec_bias_xl_bart_K = c()
  
  for(i in 1:num_trials){
    num_train = vec_train_size[i]
    num_test = vec_test_size[i]
    message(sprintf("==========Running %d of %d Trials, Train Size: %d, Test Size: %d==========", i, num_trials, num_train, num_test))

    list_result_i = Bias_One_Simulation(num_train, num_test, d)
    
    rmse_sl_rf = list_result_i$rmse_sl_rf
    rmse_sl_bart = list_result_i$rmse_sl_bart
    rmse_tl_rf = list_result_i$rmse_tl_rf
    rmse_tl_bart = list_result_i$rmse_tl_bart
    rmse_xl_rf = list_result_i$rmse_xl_rf
    rmse_xl_bart = list_result_i$rmse_xl_bart
    vec_rmse_sl_rf = append(vec_rmse_sl_rf, rmse_sl_rf)
    vec_rmse_sl_bart = append(vec_rmse_sl_bart, rmse_sl_bart)
    vec_rmse_tl_rf = append(vec_rmse_tl_rf, rmse_tl_rf)
    vec_rmse_tl_bart = append(vec_rmse_tl_bart, rmse_tl_bart)
    vec_rmse_xl_rf = append(vec_rmse_xl_rf, rmse_xl_rf)
    vec_rmse_xl_bart = append(vec_rmse_xl_bart, rmse_xl_bart)
    
    bias_sl_rf = list_result_i$bias_sl_rf
    bias_sl_bart = list_result_i$bias_sl_bart
    bias_tl_rf = list_result_i$bias_tl_rf
    bias_tl_bart = list_result_i$bias_tl_bart
    bias_xl_rf = list_result_i$bias_xl_rf
    bias_xl_bart = list_result_i$bias_xl_bart
    vec_bias_sl_rf = append(vec_bias_sl_rf, bias_sl_rf)
    vec_bias_sl_bart = append(vec_bias_sl_bart, bias_sl_bart)
    vec_bias_tl_rf = append(vec_bias_tl_rf, bias_tl_rf)
    vec_bias_tl_bart = append(vec_bias_tl_bart, bias_tl_bart)
    vec_bias_xl_rf = append(vec_bias_xl_rf, bias_xl_rf)
    vec_bias_xl_bart = append(vec_bias_xl_bart, bias_xl_bart)
    
    rmse_sl_rf_B = list_result_i$rmse_sl_rf_B
    rmse_sl_bart_B = list_result_i$rmse_sl_bart_B
    rmse_tl_rf_B = list_result_i$rmse_tl_rf_B
    rmse_tl_bart_B = list_result_i$rmse_tl_bart_B
    rmse_xl_rf_B = list_result_i$rmse_xl_rf_B
    rmse_xl_bart_B = list_result_i$rmse_xl_bart_B
    vec_rmse_sl_rf_B = append(vec_rmse_sl_rf_B, rmse_sl_rf_B)
    vec_rmse_sl_bart_B = append(vec_rmse_sl_bart_B, rmse_sl_bart_B)
    vec_rmse_tl_rf_B = append(vec_rmse_tl_rf_B, rmse_tl_rf_B)
    vec_rmse_tl_bart_B = append(vec_rmse_tl_bart_B, rmse_tl_bart_B)
    vec_rmse_xl_rf_B = append(vec_rmse_xl_rf_B, rmse_xl_rf_B)
    vec_rmse_xl_bart_B = append(vec_rmse_xl_bart_B, rmse_xl_bart_B)
    
    bias_sl_rf_B = list_result_i$bias_sl_rf_B
    bias_sl_bart_B = list_result_i$bias_sl_bart_B
    bias_tl_rf_B = list_result_i$bias_tl_rf_B
    bias_tl_bart_B = list_result_i$bias_tl_bart_B
    bias_xl_rf_B = list_result_i$bias_xl_rf_B
    bias_xl_bart_B = list_result_i$bias_xl_bart_B
    vec_bias_sl_rf_B = append(vec_bias_sl_rf_B, bias_sl_rf_B)
    vec_bias_sl_bart_B = append(vec_bias_sl_bart_B, bias_sl_bart_B)
    vec_bias_tl_rf_B = append(vec_bias_tl_rf_B, bias_tl_rf_B)
    vec_bias_tl_bart_B = append(vec_bias_tl_bart_B, bias_tl_bart_B)
    vec_bias_xl_rf_B = append(vec_bias_xl_rf_B, bias_xl_rf_B)
    vec_bias_xl_bart_B = append(vec_bias_xl_bart_B, bias_xl_bart_B)
    
    rmse_sl_rf_K = list_result_i$rmse_sl_rf_K
    rmse_sl_bart_K = list_result_i$rmse_sl_bart_K
    rmse_tl_rf_K = list_result_i$rmse_tl_rf_K
    rmse_tl_bart_K = list_result_i$rmse_tl_bart_K
    rmse_xl_rf_K = list_result_i$rmse_xl_rf_K
    rmse_xl_bart_K = list_result_i$rmse_xl_bart_K
    vec_rmse_sl_rf_K = append(vec_rmse_sl_rf_K, rmse_sl_rf_K)
    vec_rmse_sl_bart_K = append(vec_rmse_sl_bart_K, rmse_sl_bart_K)
    vec_rmse_tl_rf_K = append(vec_rmse_tl_rf_K, rmse_tl_rf_K)
    vec_rmse_tl_bart_K = append(vec_rmse_tl_bart_K, rmse_tl_bart_K)
    vec_rmse_xl_rf_K = append(vec_rmse_xl_rf_K, rmse_xl_rf_K)
    vec_rmse_xl_bart_K = append(vec_rmse_xl_bart_K, rmse_xl_bart_K)
    
    bias_sl_rf_K = list_result_i$bias_sl_rf_K
    bias_sl_bart_K = list_result_i$bias_sl_bart_K
    bias_tl_rf_K = list_result_i$bias_tl_rf_K
    bias_tl_bart_K = list_result_i$bias_tl_bart_K
    bias_xl_rf_K = list_result_i$bias_xl_rf_K
    bias_xl_bart_K = list_result_i$bias_xl_bart_K
    vec_bias_sl_rf_K = append(vec_bias_sl_rf_K, bias_sl_rf_K)
    vec_bias_sl_bart_K = append(vec_bias_sl_bart_K, bias_sl_bart_K)
    vec_bias_tl_rf_K = append(vec_bias_tl_rf_K, bias_tl_rf_K)
    vec_bias_tl_bart_K = append(vec_bias_tl_bart_K, bias_tl_bart_K)
    vec_bias_xl_rf_K = append(vec_bias_xl_rf_K, bias_xl_rf_K)
    vec_bias_xl_bart_K = append(vec_bias_xl_bart_K, bias_xl_bart_K)
  }
  
  DF_RMSE <- data.frame(
    sl_rf = vec_rmse_sl_rf, 
    sl_bart = vec_rmse_sl_bart, 
    tl_rf = vec_rmse_tl_rf,
    tl_bart = vec_rmse_tl_bart,
    xl_rf = vec_rmse_xl_rf,
    xl_bart = vec_rmse_xl_bart,
    
    sl_rf_B = vec_rmse_sl_rf_B, 
    sl_bart_B = vec_rmse_sl_bart_B, 
    tl_rf_B = vec_rmse_tl_rf_B,
    tl_bart_B = vec_rmse_tl_bart_B,
    xl_rf_B = vec_rmse_xl_rf_B,
    xl_bart_B = vec_rmse_xl_bart_B,
    
    sl_rf_K = vec_rmse_sl_rf_K, 
    sl_bart_K = vec_rmse_sl_bart_K, 
    tl_rf_K = vec_rmse_tl_rf_K,
    tl_bart_K = vec_rmse_tl_bart_K,
    xl_rf_K = vec_rmse_xl_rf_K,
    xl_bart_K = vec_rmse_xl_bart_K
  )
  
  DF_RMSE <- data.frame(
    sl_rf = vec_rmse_sl_rf, 
    sl_bart = vec_rmse_sl_bart, 
    tl_rf = vec_rmse_tl_rf,
    tl_bart = vec_rmse_tl_bart,
    xl_rf = vec_rmse_xl_rf,
    xl_bart = vec_rmse_xl_bart,
    
    sl_rf_B = vec_rmse_sl_rf_B, 
    sl_bart_B = vec_rmse_sl_bart_B, 
    tl_rf_B = vec_rmse_tl_rf_B,
    tl_bart_B = vec_rmse_tl_bart_B,
    xl_rf_B = vec_rmse_xl_rf_B,
    xl_bart_B = vec_rmse_xl_bart_B,
    
    sl_rf_K = vec_rmse_sl_rf_K, 
    sl_bart_K = vec_rmse_sl_bart_K, 
    tl_rf_K = vec_rmse_tl_rf_K,
    tl_bart_K = vec_rmse_tl_bart_K,
    xl_rf_K = vec_rmse_xl_rf_K,
    xl_bart_K = vec_rmse_xl_bart_K
  )
  
  DF_BIAS <- data.frame(
    sl_rf = vec_bias_sl_rf, 
    sl_bart = vec_bias_sl_bart, 
    tl_rf = vec_bias_tl_rf,
    tl_bart = vec_bias_tl_bart,
    xl_rf = vec_bias_xl_rf,
    xl_bart = vec_bias_xl_bart,
    
    sl_rf_B = vec_bias_sl_rf_B, 
    sl_bart_B = vec_bias_sl_bart_B, 
    tl_rf_B = vec_bias_tl_rf_B,
    tl_bart_B = vec_bias_tl_bart_B,
    xl_rf_B = vec_bias_xl_rf_B,
    xl_bart_B = vec_bias_xl_bart_B,
    
    sl_rf_K = vec_bias_sl_rf_K, 
    sl_bart_K = vec_bias_sl_bart_K, 
    tl_rf_K = vec_bias_tl_rf_K,
    tl_bart_K = vec_bias_tl_bart_K,
    xl_rf_K = vec_bias_xl_rf_K,
    xl_bart_K = vec_bias_xl_bart_K
  )
  
  list_rtn = list(rmse = DF_RMSE, bias = DF_BIAS)
  
  return(list_rtn)
}

Bias_One_Simulation<- function(num_train, num_test, d){
  simulated_experiment <- simulate_causal_experiment(
    ntrain = num_train,
    ntest = num_test,
    dim = d)
  
  feature_train_raw <- simulated_experiment$feat_tr
  feature_test_raw <- simulated_experiment$feat_te
  w_train <- simulated_experiment$W_tr
  yobs_train <- simulated_experiment$Yobs_tr
  cate_true <- simulated_experiment$tau_te
  
  feature_train = feature_train_raw
  feature_test = feature_test_raw
  
  list_rtn_B = Convert_Dataset_Binary(feature_train_raw, feature_test_raw, depth_bt = 8, type_root="mean")
  feature_train_B = list_rtn_B$train
  feature_test_B = list_rtn_B$test
  
  list_rtn_K = Convert_Dataset_KBin(feature_train_raw, feature_test_raw, k_bin=16)
  feature_train_K = list_rtn_K$train
  feature_test_K = list_rtn_K$test
  
  # print(head(feature_train))
  # print(head(feature_train_B))
  # print(head(feature_train_K))
  
  message(sprintf("Learning Over Raw Data..."))
  sl_rf <- S_RF(feat = feature_train, tr = w_train, yobs = yobs_train)
  sl_bart <- S_BART(feat = feature_train, tr = w_train, yobs = yobs_train)
  
  tl_rf <- T_RF(feat = feature_train, tr = w_train, yobs = yobs_train)
  tl_bart <- T_BART(feat = feature_train, tr = w_train, yobs = yobs_train)
  
  xl_rf <- X_RF(feat = feature_train, tr = w_train, yobs = yobs_train)
  xl_bart <- X_BART(feat = feature_train, tr = w_train, yobs = yobs_train)
  
  cate_esti_sl_rf <- EstimateCate(sl_rf, feature_test)
  cate_esti_sl_bart <- EstimateCate(sl_bart, feature_test)
  cate_esti_tl_rf <- EstimateCate(tl_rf, feature_test)
  cate_esti_tl_bart <- EstimateCate(tl_bart, feature_test)
  cate_esti_xl_rf <- EstimateCate(xl_rf, feature_test)
  cate_esti_xl_bart <- EstimateCate(xl_bart, feature_test)
  
  # cate_true <- cate_esti_tl_rf
  cate_true <- simulated_experiment$tau_te
  
  bias_sl_rf = abs(mean(cate_esti_sl_rf - cate_true))
  bias_sl_bart = abs(mean(cate_esti_sl_bart - cate_true))
  bias_tl_rf = abs(mean(cate_esti_tl_rf - cate_true))
  bias_tl_bart = abs(mean(cate_esti_tl_bart - cate_true))
  bias_xl_rf = abs(mean(cate_esti_xl_rf - cate_true))
  bias_xl_bart = abs(mean(cate_esti_xl_bart - cate_true))
  
  rmse_sl_rf = mean((cate_esti_sl_rf - cate_true) ^ 2)
  rmse_sl_bart = mean((cate_esti_sl_bart - cate_true) ^ 2)
  rmse_tl_rf = mean((cate_esti_tl_rf - cate_true) ^ 2)
  rmse_tl_bart = mean((cate_esti_tl_bart - cate_true) ^ 2)
  rmse_xl_rf = mean((cate_esti_xl_rf - cate_true) ^ 2)
  rmse_xl_bart = mean((cate_esti_xl_bart - cate_true) ^ 2)
  
  message(sprintf("Learning Over Binary Data..."))
  sl_rf_B <- S_RF(feat = feature_train_B, tr = w_train, yobs = yobs_train)
  sl_bart_B <- S_BART(feat = feature_train_B, tr = w_train, yobs = yobs_train)
  
  tl_rf_B <- T_RF(feat = feature_train_B, tr = w_train, yobs = yobs_train)
  tl_bart_B <- T_BART(feat = feature_train_B, tr = w_train, yobs = yobs_train)
  
  xl_rf_B <- X_RF(feat = feature_train_B, tr = w_train, yobs = yobs_train)
  xl_bart_B <- X_BART(feat = feature_train_B, tr = w_train, yobs = yobs_train)
  
  cate_esti_sl_rf_B <- EstimateCate(sl_rf_B, feature_test_B)
  cate_esti_sl_bart_B <- EstimateCate(sl_bart_B, feature_test_B)
  cate_esti_tl_rf_B <- EstimateCate(tl_rf_B, feature_test_B)
  cate_esti_tl_bart_B <- EstimateCate(tl_bart_B, feature_test_B)
  cate_esti_xl_rf_B <- EstimateCate(xl_rf_B, feature_test_B)
  cate_esti_xl_bart_B <- EstimateCate(xl_bart_B, feature_test_B)
  
  # cate_true_B <- cate_esti_tl_rf_B
  cate_true_B <- simulated_experiment$tau_te
  
  bias_sl_rf_B = abs(mean(cate_esti_sl_rf_B - cate_true_B))
  bias_sl_bart_B = abs(mean(cate_esti_sl_bart_B - cate_true_B))
  bias_tl_rf_B = abs(mean(cate_esti_tl_rf_B - cate_true_B))
  bias_tl_bart_B = abs(mean(cate_esti_tl_bart_B - cate_true_B))
  bias_xl_rf_B = abs(mean(cate_esti_xl_rf_B - cate_true_B))
  bias_xl_bart_B = abs(mean(cate_esti_xl_bart_B - cate_true_B))
  
  rmse_sl_rf_B = mean((cate_esti_sl_rf_B - cate_true_B) ^ 2)
  rmse_sl_bart_B = mean((cate_esti_sl_bart_B - cate_true_B) ^ 2)
  rmse_tl_rf_B = mean((cate_esti_tl_rf_B - cate_true_B) ^ 2)
  rmse_tl_bart_B = mean((cate_esti_tl_bart_B - cate_true_B) ^ 2)
  rmse_xl_rf_B = mean((cate_esti_xl_rf_B - cate_true_B) ^ 2)
  rmse_xl_bart_B = mean((cate_esti_xl_bart_B - cate_true_B) ^ 2)
  
  message(sprintf("Learning Over Discrete Data..."))
  sl_rf_K <- S_RF(feat = feature_train_K, tr = w_train, yobs = yobs_train)
  sl_bart_K <- S_BART(feat = feature_train_K, tr = w_train, yobs = yobs_train)
  
  tl_rf_K <- T_RF(feat = feature_train_K, tr = w_train, yobs = yobs_train)
  tl_bart_K <- T_BART(feat = feature_train_K, tr = w_train, yobs = yobs_train)
  
  xl_rf_K <- X_RF(feat = feature_train_K, tr = w_train, yobs = yobs_train)
  xl_bart_K <- X_BART(feat = feature_train_K, tr = w_train, yobs = yobs_train)
  
  
  
  cate_esti_sl_rf_K <- EstimateCate(sl_rf_K, feature_test_K)
  cate_esti_sl_bart_K <- EstimateCate(sl_bart_K, feature_test_K)
  cate_esti_tl_rf_K <- EstimateCate(tl_rf_K, feature_test_K)
  cate_esti_tl_bart_K <- EstimateCate(tl_bart_K, feature_test_K)
  cate_esti_xl_rf_K <- EstimateCate(xl_rf_K, feature_test_K)
  cate_esti_xl_bart_K <- EstimateCate(xl_bart_K, feature_test_K)
  
  # cate_true_K <- cate_esti_tl_rf_K
  cate_true_K <- simulated_experiment$tau_te
  
  bias_sl_rf_K = abs(mean(cate_esti_sl_rf_K - cate_true_K))
  bias_sl_bart_K = abs(mean(cate_esti_sl_bart_K - cate_true_K))
  bias_tl_rf_K = abs(mean(cate_esti_tl_rf_K - cate_true_K))
  bias_tl_bart_K = abs(mean(cate_esti_tl_bart_K - cate_true_K))
  bias_xl_rf_K = abs(mean(cate_esti_xl_rf_K - cate_true_K))
  bias_xl_bart_K = abs(mean(cate_esti_xl_bart_K - cate_true_K))
  
  rmse_sl_rf_K = mean((cate_esti_sl_rf_K - cate_true_K) ^ 2)
  rmse_sl_bart_K = mean((cate_esti_sl_bart_K - cate_true_K) ^ 2)
  rmse_tl_rf_K = mean((cate_esti_tl_rf_K - cate_true_K) ^ 2)
  rmse_tl_bart_K = mean((cate_esti_tl_bart_K - cate_true_K) ^ 2)
  rmse_xl_rf_K = mean((cate_esti_xl_rf_K - cate_true_K) ^ 2)
  rmse_xl_bart_K = mean((cate_esti_xl_bart_K - cate_true_K) ^ 2)
  
  
  list_rtn = list(rmse_sl_rf = rmse_sl_rf, rmse_sl_bart = rmse_sl_bart, 
                  rmse_tl_rf = rmse_tl_rf, rmse_tl_bart = rmse_tl_bart, 
                  rmse_xl_rf = rmse_xl_rf, rmse_xl_bart = rmse_xl_bart,
                  rmse_sl_rf_B = rmse_sl_rf_B, rmse_sl_bart_B = rmse_sl_bart_B,
                  rmse_tl_rf_B = rmse_tl_rf_B, rmse_tl_bart_B = rmse_tl_bart_B,
                  rmse_xl_rf_B = rmse_xl_rf_B, rmse_xl_bart_B = rmse_xl_bart_B,
                  rmse_sl_rf_K = rmse_sl_rf_K, rmse_sl_bart_K = rmse_sl_bart_K,
                  rmse_tl_rf_K = rmse_tl_rf_K, rmse_tl_bart_K = rmse_tl_bart_K,
                  rmse_xl_rf_K = rmse_xl_rf_K, rmse_xl_bart_K = rmse_xl_bart_K,
                  bias_sl_rf = bias_sl_rf, bias_sl_bart = bias_sl_bart, 
                  bias_tl_rf = bias_tl_rf, bias_tl_bart = bias_tl_bart, 
                  bias_xl_rf = bias_xl_rf, bias_xl_bart = bias_xl_bart,
                  bias_sl_rf_B = bias_sl_rf_B, bias_sl_bart_B = bias_sl_bart_B,
                  bias_tl_rf_B = bias_tl_rf_B, bias_tl_bart_B = bias_tl_bart_B,
                  bias_xl_rf_B = bias_xl_rf_B, bias_xl_bart_B = bias_xl_bart_B,
                  bias_sl_rf_K = bias_sl_rf_K, bias_sl_bart_K = bias_sl_bart_K,
                  bias_tl_rf_K = bias_tl_rf_K, bias_tl_bart_K = bias_tl_bart_K,
                  bias_xl_rf_K = bias_xl_rf_K, bias_xl_bart_K = bias_xl_bart_K)
  
  return(list_rtn)
}

CI_Full_Simulation<- function(num_train, num_test, d, num_bootstrap){
  
  list_rtn_raw = Estimate_Learner_CI_Coverage(num_train, num_test, d, num_bootstrap, type_data="", NULL)
  list_rtn_K = Estimate_Learner_CI_Coverage(num_train, num_test, d, num_bootstrap, type_data="Discretization", parameter_data=list(k_bin = 8))
  list_rtn_B = Estimate_Learner_CI_Coverage(num_train, num_test, d, num_bootstrap, type_data="Binary", parameter_data=list(depth_bt = 3, type_root="median"))
  
  vec_ci_cov_full = c(list_rtn_raw$vec_ci_cov, list_rtn_K$vec_ci_cov, list_rtn_B$vec_ci_cov)
  vec_ci_len_full = c(list_rtn_raw$vec_ci_len, list_rtn_K$vec_ci_len, list_rtn_B$vec_ci_len)
  vec_ci_name_full = c(list_rtn_raw$vec_ci_name, list_rtn_K$vec_ci_name, list_rtn_B$vec_ci_name)
  vec_ci_type_full = c("R", "R", "R", "R", "R", "R", "K", "K", "K", "K", "K", "K", "B", "B", "B", "B", "B", "B")
  
  DF_Result <- data.frame(
    Average_CI_Length = vec_ci_len_full, 
    Coverage = vec_ci_cov_full, 
    PointName = vec_ci_name_full,
    ColorGroup = vec_ci_type_full,
    ShapeGroup = vec_ci_type_full
  )
  
  return(DF_Result)
}

Estimate_Learner_CI_Coverage <- function(num_train, num_test, d, num_bootstrap, type_data, parameter_data) {
  message(sprintf("Generating Experiment Environment and Learning Learners ..."))
  exp_env = Generate_Simulation(num_train, num_test, d, type_data, parameter_data)
  truth = exp_env$truth
  feature_test = exp_env$feature_test
  
  mark_suffix = "R"
  if(type_data != ""){
    if(type_data == "Binary"){ mark_suffix = "B" }
    else if (type_data == "Discretization"){ mark_suffix = "K" }
  }
  
  mark_sl_rf = paste("S_RF", mark_suffix, sep="_")
  message(sprintf("----------%s Running Bootstrap...----------", mark_sl_rf))
  sl_rf = exp_env$sl_rf
  list_rtn_sl_rf = Estimate_One_Learner_CI_Coverage(truth, feature_test, sl_rf, num_bootstrap) 
  cov_sl_rf = list_rtn_sl_rf$coverage
  len_sl_rf = list_rtn_sl_rf$length_avg
  message(sprintf("%s CI Coverage = %f", mark_sl_rf, cov_sl_rf))
  message(sprintf("%s CI AvgLength = %f", mark_sl_rf, len_sl_rf))
  
  mark_sl_bart = paste("S_BART", mark_suffix, sep="_")
  message(sprintf("----------%s Running Bootstrap...----------", mark_sl_bart))
  sl_bart = exp_env$sl_bart
  list_rtn_sl_bart = Estimate_One_Learner_CI_Coverage(truth, feature_test, sl_bart, num_bootstrap) 
  cov_sl_bart = list_rtn_sl_bart$coverage
  len_sl_bart = list_rtn_sl_bart$length_avg
  message(sprintf("%s CI Coverage = %f", mark_sl_bart, cov_sl_bart))
  message(sprintf("%s CI AvgLength = %f", mark_sl_bart, len_sl_bart))
  
  mark_tl_rf = paste("T_RF", mark_suffix, sep="_")
  message(sprintf("----------%s Running Bootstrap...----------", mark_tl_rf))
  tl_rf = exp_env$tl_rf
  list_rtn_tl_rf = Estimate_One_Learner_CI_Coverage(truth, feature_test, tl_rf, num_bootstrap) 
  cov_tl_rf = list_rtn_tl_rf$coverage
  len_tl_rf = list_rtn_tl_rf$length_avg
  message(sprintf("%s CI Coverage = %f", mark_tl_rf, cov_tl_rf))
  message(sprintf("%s CI AvgLength = %f", mark_tl_rf, len_tl_rf))
  
  mark_tl_bart = paste("T_BART", mark_suffix, sep="_")
  message(sprintf("----------%s Running Bootstrap...----------", mark_tl_bart))
  tl_bart = exp_env$tl_bart
  list_rtn_tl_bart = Estimate_One_Learner_CI_Coverage(truth, feature_test, tl_bart, num_bootstrap) 
  cov_tl_bart= list_rtn_tl_bart$coverage
  len_tl_bart = list_rtn_tl_bart$length_avg
  message(sprintf("%s CI Coverage = %f", mark_tl_bart, cov_tl_bart))
  message(sprintf("%s CI AvgLength = %f", mark_tl_bart, len_tl_bart))
  
  mark_xl_rf = paste("X_RF", mark_suffix, sep="_")
  message(sprintf("----------%s Running Bootstrap...----------", mark_xl_rf))
  xl_rf = exp_env$xl_rf
  list_rtn_xl_rf = Estimate_One_Learner_CI_Coverage(truth, feature_test, xl_rf, num_bootstrap) 
  cov_xl_rf = list_rtn_xl_rf$coverage
  len_xl_rf = list_rtn_xl_rf$length_avg
  message(sprintf("%s CI Coverage = %f", mark_xl_rf, cov_xl_rf))
  message(sprintf("%s CI AvgLength = %f", mark_xl_rf, len_xl_rf))
  
  mark_xl_bart = paste("X_BART", mark_suffix, sep="_")
  message(sprintf("----------%s Running Bootstrap...----------", mark_xl_bart))
  xl_bart = exp_env$xl_bart
  list_rtn_xl_bart = Estimate_One_Learner_CI_Coverage(truth, feature_test, xl_bart, num_bootstrap) 
  cov_xl_bart = list_rtn_xl_bart$coverage
  len_xl_bart = list_rtn_xl_bart$length_avg
  message(sprintf("%s CI Coverage = %f", mark_xl_bart, cov_xl_bart))
  message(sprintf("%s CI AvgLength = %f", mark_xl_bart, len_xl_bart))
  
  vec_ci_cov = c(cov_sl_rf, cov_sl_bart, cov_tl_rf, cov_tl_bart, cov_xl_rf, cov_xl_bart)
  vec_ci_len = c(len_sl_rf, len_sl_bart, len_tl_rf, len_tl_bart, len_xl_rf, len_xl_bart)
  vec_ci_name = c(mark_sl_rf, mark_sl_bart, mark_tl_rf, mark_tl_bart, mark_xl_rf, mark_xl_bart)
  
  list_rtn = list(vec_ci_cov=vec_ci_cov, vec_ci_len=vec_ci_len, vec_ci_name=vec_ci_name)
  return(list_rtn)
}

Generate_Simulation <- function(num_train, num_test, d, type_data, parameter_data){
  simulated_experiment <- simulate_causal_experiment(
    ntrain = num_train,
    ntest = num_test,
    dim = d)
  
  feature_train_raw <- simulated_experiment$feat_tr
  feature_test_raw <- simulated_experiment$feat_te
  w_train <- simulated_experiment$W_tr
  yobs_train <- simulated_experiment$Yobs_tr
  cate_true <- simulated_experiment$tau_te
  
  feature_train = feature_train_raw
  feature_test = feature_test_raw
  
  if(type_data != ""){
    if(type_data == "Binary"){
      message(sprintf("Converting Real To Binary..."))
      list_rtn_B = Convert_Dataset_Binary(feature_train_raw, feature_test_raw, parameter_data$depth_bt, parameter_data$type_root)
      feature_train = list_rtn_B$train
      feature_test = list_rtn_B$test
      
    }
    else if (type_data == "Discretization"){
      message(sprintf("Converting Real To Discretization"))
      list_rtn_K = Convert_Dataset_KBin(feature_train_raw, feature_test_raw, parameter_data$k_bin)
      feature_train = list_rtn_K$train
      feature_test = list_rtn_K$test
    }
  }
  
  print(head(feature_train))
  
  sl_rf <- S_RF(feat = feature_train, tr = w_train, yobs = yobs_train)
  sl_bart <- S_BART(feat = feature_train, tr = w_train, yobs = yobs_train)

  tl_rf <- T_RF(feat = feature_train, tr = w_train, yobs = yobs_train)
  tl_bart <- T_BART(feat = feature_train, tr = w_train, yobs = yobs_train)

  xl_rf <- X_RF(feat = feature_train, tr = w_train, yobs = yobs_train)
  xl_bart <- X_BART(feat = feature_train, tr = w_train, yobs = yobs_train)
  
  cate_esti_tl_rf <- EstimateCate(tl_rf, feature_test)
  
  list_rtn = list(truth = cate_esti_tl_rf, cate_true = cate_true, feature_test = feature_test, sl_rf = sl_rf, sl_bart = sl_bart, tl_rf = tl_rf, tl_bart = tl_bart, xl_rf = xl_rf, xl_bart = xl_bart)
  
  return(list_rtn)
}

Estimate_One_Learner_CI_Coverage <- function(truth, feature_test, lr, num_bootstrap) {
  
  lr_ci <- CateCI(lr, feature_test, B = num_bootstrap, verbose=FALSE)
  
  list_rtn = Estimate_One_CI_Coverage(truth, lr_ci)
  
  return(list_rtn)
}

Estimate_One_CI_Coverage <- function(truth, lr_ci) {
  N = length(lr_ci$pred)
  counter = 0
  len_CI = 0
  
  for(i in 1:N){
    # message(sprintf("I: %d = %f", i, lr_ci$pred[i]))
    ci_low_i = lr_ci$X5.[i]
    ci_high_i = lr_ci$X95.[i]
    len_i = abs(ci_high_i - ci_low_i)
    len_CI = len_CI + len_i
    # message(sprintf("CI Low: %f", ci_low_i))
    # message(sprintf("CI High: %f", ci_high_i))
    
    if((truth >= ci_low_i) && (truth <= ci_high_i)){
      counter = counter + 1
      # message(sprintf("Counter: %d", counter))
    }
  }
  
  coverage = counter / N
  len_CI_avg = len_CI / N
  list_rtn = list(coverage = coverage, length_avg = len_CI_avg)
  # message(sprintf("N: %d, Counter: %d", N, counter))
  # message(sprintf("Coverage: %f", coverage))
  return(list_rtn)
}

