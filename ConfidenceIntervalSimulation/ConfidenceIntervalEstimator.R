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

Full_Simulation<- function(num_train, num_test, d, num_bootstrap){
  
  list_rtn_raw = Estimate_Learner_CI_Coverage(num_train, num_test, d, num_bootstrap, type_data="", NULL)
  list_rtn_K = Estimate_Learner_CI_Coverage(num_train, num_test, d, num_bootstrap, type_data="Discretization", parameter_data=list(k_bin = 8))
  list_rtn_B = Estimate_Learner_CI_Coverage(num_train, num_test, d, num_bootstrap, type_data="Binary", parameter_data=list(depth_bt = 3, type_root="mean"))
  
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
      list_rtn_K = Convert_Dataset_KBin(feature_train, feature_test, parameter_data$k_bin)
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
  
  list_rtn = list(truth = cate_true, feature_test = feature_test, sl_rf = sl_rf, sl_bart = sl_bart, tl_rf = tl_rf, tl_bart = tl_bart, xl_rf = xl_rf, xl_bart = xl_bart)
  
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

