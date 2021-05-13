library(causalToolbox)
library("BART")
library("MASS")
library(foreach)
library(doParallel)
library(ggplot2)
library(egg)
library(neuralnet)
library(randomForest)
library(e1071)
library(reshape2)

# for parallel computation
cores <- detectCores(logical=F)
cl <- makeCluster(cores-1)
registerDoParallel(cl, cores=cores-1)

####################### Different base learners ##################

############### 1. GOTV data set : Should first load GOTV dataset as "gotv" #############

# compute the ground truth

gotv_groundtruth_rf <- T_RF(feat = gotv[ , 1:7], tr = gotv$treatment, yobs = gotv$voted)
cate_gotv_groundtruth_rf <- EstimateCate(gotv_groundtruth_rf, gotv[ , 1:7])


SimulationTime = 50

for(n in c(1e3, 3e3, 5e3, 1e4, 2e4, 5e4)){
  print(n)
  results <- foreach(t=1:SimulationTime, .combine='rbind', .packages = c("BART", "MASS","neuralnet", "e1071", "randomForest"), .errorhandling = 'remove') %dopar%
    {
      
      
      # sample training set and test set from the whole GOTV set
      
      gotv_index <- sample(nrow(gotv), n + 2000)
      gotv_index_train <- gotv_index[1: n]
      gotv_index_test <- gotv_index[(n + 1):(n + 2000)]
      
      gotv_sample_train <- gotv[gotv_index_train, ]
      gotv_sample_test <- gotv[gotv_index_test, ]
      feature_train <- gotv_sample_train[ , 1:7]
      tr_train <- gotv_sample_train$treatment
      yobs_train <- gotv_sample_train$voted
      feature_new <- gotv_sample_test[ , 1:7]
      cate_true <- cate_gotv_groundtruth_rf[gotv_index_test]
      yobs <- yobs_train
      feat <- feature_train
      tr <- tr_train
      
      ####   X-leaners  ####
      
      yobs_0 <- yobs[tr == 0]
      X_0 <- feat[tr == 0, ]
      yobs_1 <- yobs[tr == 1]
      X_1 <- feat[tr == 1, ]
      
      n_1 <- sum(tr)
      n_0 <- sum(1 - tr)
      
      g_weights <- sum(tr) / length(tr)
      
      f_0_test_set <- X_1
      f_1_test_set <- X_0
      
      # 1. BART
      bart_f_1 <- wbart(x.train = X_0, 
                        y.train = yobs_0, 
                        x.test = f_0_test_set,
                        ndpost=400L, nskip=100L)
      
      mu_hat_1_bart <- bart_f_1[["yhat.test.mean"]][1:n_1]
      
      bart_f_0  <- wbart(x.train = X_1, 
                         y.train = yobs_1, 
                         x.test = f_1_test_set,
                         ndpost=400L, nskip=100L)
      
      mu_hat_0_bart <- bart_f_0[["yhat.test.mean"]][1:n_0]
      
      
      D_1_bart <- yobs_1 - mu_hat_1_bart
      D_0_bart <- mu_hat_0_bart - yobs_0
      
      bart_s_1 <- wbart(x.train = X_1, 
                        y.train = D_1_bart, 
                        x.test = feature_new,
                        ndpost=400L, nskip=100L)
      
      tau_hat_1_bart <- bart_s_1[["yhat.test.mean"]]
      
      bart_s_0  <- wbart(x.train = X_0, 
                         y.train = D_0_bart, 
                         x.test = feature_new,
                         ndpost=400L, nskip=100L)
      
      tau_hat_0_bart <- bart_s_0[["yhat.test.mean"]]
      
      pred_bart <- g_weights * tau_hat_0_bart + (1 - g_weights) * tau_hat_1_bart
      
      mse_bart_x <- mean((cate_true - pred_bart)^2)
      
      # 2. Neural Networks
      
      NN_f_1 <- neuralnet(as.formula(paste("yobs_0~", paste(names(X_0), collapse = " + "))), 
                          data = data.frame(yobs_0, X_0), 
                          hidden = 7, 
                          threshold = 0.1, linear.output = FALSE,
                          err.fct = 'sse')
      mu_hat_1_NN <- predict(NN_f_1, newdata =  data.frame(f_0_test_set))
      NN_f_0 <- neuralnet(as.formula(paste("yobs_1~", paste(names(X_1), collapse = " + "))), 
                          data = data.frame(yobs_1, X_1), 
                          hidden = 7, 
                          threshold = 0.1, linear.output = FALSE,
                          err.fct = 'sse')
      mu_hat_0_NN <- predict(NN_f_0, newdata =  data.frame(f_1_test_set))
      
      D_1_NN <- yobs_1 - mu_hat_1_NN
      D_0_NN <- mu_hat_0_NN - yobs_0
      
      
      NN_s_1 <- neuralnet(as.formula(paste("D_1_NN~", paste(names(X_1), collapse = " + "))), 
                          data = data.frame(D_1_NN, X_1), 
                          hidden = 7, 
                          threshold = 0.1, linear.output = FALSE,
                          err.fct = 'sse')
      
      tau_hat_1_NN <- predict(NN_s_1, newdata =  data.frame(feature_new))
      
      NN_s_0 <- neuralnet(as.formula(paste("D_0_NN~", paste(names(X_0), collapse = " + "))), 
                          data = data.frame(D_0_NN, X_0), 
                          hidden = 7, 
                          threshold = 0.1, linear.output = FALSE,
                          err.fct = 'sse')
      
      tau_hat_0_NN <- predict(NN_s_0, newdata =  data.frame(feature_new))
      
      pred_NN <- g_weights * tau_hat_0_NN + (1 - g_weights) * tau_hat_1_NN
      
      mse_NN_x <- mean((cate_true - pred_NN)^2)
      
      # 3. randomForest
      
      rf_f_1 <- randomForest(yobs_0~., data = data.frame(yobs_0, X_0), xtest = f_0_test_set)
      mu_hat_1_rf <- rf_f_1$test$predicted
      rf_f_0 <- randomForest(yobs_1~., data = data.frame(yobs_1, X_1), xtest = f_1_test_set)
      mu_hat_0_rf <- rf_f_0$test$predicted
      D_1_rf <- yobs_1 - mu_hat_1_rf
      D_0_rf <- mu_hat_0_rf - yobs_0
      rf_s_1 <- randomForest(D_1_rf~., data = data.frame(D_1_rf, X_1), xtest = feature_new)
      tau_hat_1_rf <- rf_s_1$test$predicted
      rf_s_0 <- randomForest(D_0_rf~., data = data.frame(D_0_rf, X_0), xtest = feature_new)
      tau_hat_0_rf <- rf_s_0$test$predicted
      pred_rf<- g_weights * tau_hat_0_rf + (1 - g_weights) * tau_hat_1_rf
      mse_rf_x <- mean((cate_true - pred_rf)^2)
      
      # 4. SVM
      
      svm_f_1 <- svm(yobs_0~., data = data.frame(yobs_0, X_0), kernal="radial basis")
      mu_hat_1_svm <- predict(svm_f_1, newdata =  data.frame(f_0_test_set))
      svm_f_0 <- svm(yobs_1~., data = data.frame(yobs_1, X_1), kernal="radial basis")
      mu_hat_0_svm <- predict(svm_f_0, newdata =  data.frame(f_1_test_set))
      D_1_svm <- yobs_1 - mu_hat_1_svm
      D_0_svm <- mu_hat_0_svm - yobs_0
      svm_s_1 <- randomForest(D_1_svm~., data = data.frame(D_1_svm, X_1), xtest = feature_new)
      tau_hat_1_svm <- svm_s_1$test$predicted
      svm_s_0 <- randomForest(D_0_svm~., data = data.frame(D_0_svm, X_0), xtest = feature_new)
      tau_hat_0_svm <- svm_s_0$test$predicted
      pred_svm <- g_weights * tau_hat_0_svm + (1 - g_weights) * tau_hat_1_svm
      mse_svm_x <- mean((cate_true - pred_svm)^2)
      
      
      ####   T-leaners  ####
      
      # 1. BART
      
      bart_t_0 <- wbart(x.train = X_0, 
                        y.train = yobs_0, 
                        x.test = feature_new,
                        ndpost=400L, nskip=100L)
      
      mu_hat_0_bart_t <- bart_t_0[["yhat.test.mean"]]
      
      bart_t_1  <- wbart(x.train = X_1, 
                         y.train = yobs_1, 
                         x.test = feature_new,
                         ndpost=400L, nskip=100L)
      
      mu_hat_1_bart_t <- bart_t_1[["yhat.test.mean"]]
      
      ?wbart
      
      pred_bart_t <- mu_hat_1_bart_t - mu_hat_0_bart_t
      mse_bart_t <- mean((cate_true - pred_bart)^2)
      
      # 2. NN
      NN_t_0 <- neuralnet(as.formula(paste("yobs_0~", paste(names(X_0), collapse = " + "))), 
                          data = data.frame(yobs_0, X_0), 
                          hidden = 7, 
                          threshold = 0.1, linear.output = FALSE,
                          err.fct = 'sse')
      mu_hat_0_NN_t <- predict(NN_t_0, newdata =  feature_new)
      NN_t_1 <- neuralnet(as.formula(paste("yobs_1~", paste(names(X_1), collapse = " + "))), 
                          data = data.frame(yobs_1, X_1), 
                          hidden = 7, 
                          threshold = 0.1, linear.output = FALSE,
                          err.fct = 'sse')
      mu_hat_1_NN_t <- predict(NN_t_1, newdata =  feature_new)
      
      pred_NN_t <- mu_hat_1_NN_t -  mu_hat_0_NN_t
      
      mse_NN_t <- mean((cate_true - pred_NN_t)^2)
      
      # 3. random forest
      
      rf_t_0 <- randomForest(yobs_0~., data = data.frame(yobs_0, X_0), xtest = feature_new)
      mu_hat_0_rf_t <- rf_t_0$test$predicted
      rf_t_1 <- randomForest(yobs_1~., data = data.frame(yobs_1, X_1), xtest = feature_new)
      mu_hat_1_rf_t <- rf_t_1$test$predicted
      
      pred_rf_t <- mu_hat_1_rf_t - mu_hat_0_rf_t
      mse_rf_t <- mean((pred_rf_t - cate_true)^2)
      
      # 4. SVM
      
      svm_t_0 <- svm(yobs_0~., data = data.frame(yobs_0, X_0), kernal="radial basis")
      mu_hat_0_svm_t <- predict(svm_t_0, newdata =  data.frame(feature_new))
      svm_t_1 <- svm(yobs_1~., data = data.frame(yobs_1, X_1), kernal="radial basis")
      mu_hat_1_svm_t <- predict(svm_t_1, newdata =  data.frame(feature_new))
      
      pred_svm_t <- mu_hat_1_svm_t - mu_hat_0_svm_t
      mse_svm_t <- mean((pred_svm_t - cate_true)^2)
      
      
      ####   S-leaners  ####  
      
      # 1. bart
      
      bart_S <- wbart(x.train = data.frame(feature_train, tr), 
                      y.train = yobs_train,
                      ndpost=400L, nskip=100L)
      mu_hat_1_bart_S <- predict(bart_S, newdata = data.frame(feature_new, tr = 1))
      mu_hat_0_bart_S <- predict(bart_S, newdata = data.frame(feature_new, tr = 0))
      pred_bart_S <- mu_hat_1_bart_S - mu_hat_0_bart_S
      mse_bart_S <- mean((cate_true - pred_bart_S)^2)
      
      # 2. NN
      NN_S <- neuralnet(as.formula(paste("yobs~", paste(names(feature_train), collapse = " + "), "+ tr")), 
                        data = data.frame(yobs, feature_train, tr), 
                        hidden = 7, 
                        threshold = 0.1, linear.output = FALSE,
                        err.fct = 'sse')
      mu_hat_1_NN_S <- predict(NN_S, newdata = data.frame(feature_new, tr = 1))
      mu_hat_0_NN_S <- predict(NN_S, newdata = data.frame(feature_new, tr = 0))
      pred_NN_S <- mu_hat_1_NN_S - mu_hat_0_NN_S
      mse_NN_S <- mean((cate_true - pred_NN_S)^2)
      
      #3. random forest
      rf_S <- randomForest(yobs~., data = data.frame(yobs, feature_train, tr))
      mu_hat_1_rf_S <- predict(rf_S, newdata = data.frame(feature_new, tr = 1))
      mu_hat_0_rf_S <- predict(rf_S, newdata = data.frame(feature_new, tr = 0))
      pred_rf_S <- mu_hat_1_rf_S - mu_hat_0_rf_S
      mse_rf_S <- mean((cate_true - pred_rf_S)^2)
      
      #4. SVM
      svm_S <- svm(yobs~., data = data.frame(yobs, feature_train, tr), kernal="radial basis")
      mu_hat_1_svm_S <- predict(svm_S, newdata = data.frame(feature_new, tr = 1))
      mu_hat_0_svm_S <- predict(svm_S, newdata = data.frame(feature_new, tr = 0))
      pred_svm_S <- mu_hat_1_svm_S - mu_hat_0_svm_S
      mse_svm_S <- mean((cate_true - pred_svm_S)^2)
      
      result <- c(mse_bart_x, mse_NN_x, mse_rf_x, mse_svm_x, mse_bart_t, mse_NN_t, mse_rf_t, mse_svm_t, mse_bart_S, mse_NN_S, mse_rf_S, mse_svm_S)

      return(result)
    }
  save(results, file=paste("D://n = ", n, ".Rdata"))
} 

# box plot the results

ResultMatrix <- c()
for(n in c(1e3, 3e3, 5e3, 1e4, 2e4, 5e4)){
  load(file=paste("D://n = ", n, ".Rdata"))
  ResultMatrix <- rbind(ResultMatrix, cbind(results, n))
}

ResultMatrix <- data.frame(ResultMatrix)

colnames(ResultMatrix) <- c("BART_X", "NN_X", "RF_X", "SVM_X",
                           "BART_T", "NN_T", "RF_T", "SVM_T", 
                           "BART_S", "NN_S", "RF_S", "SVM_S", "N")

ResultMatrixMelt <- melt(ResultMatrix[ ,c(1:4, 13)],  id.vars = "N", variable.name = 'methods', value.name = "MSE")
ResultMatrixMelt[, "N"] <- as.factor(ResultMatrixMelt[ ,"N"])  
  plot_box_X <- ggplot(ResultMatrixMelt, aes(x=N, y=MSE, fill=methods)) + 
  geom_boxplot() +
  theme(legend.position = "left")+ ggtitle("X-learner") +
  xlab("training size")
  
ResultMatrixMelt <- melt(ResultMatrix[ ,c(5:8, 13)],  id.vars = "N", variable.name = 'methods', value.name = "MSE")
ResultMatrixMelt[, "N"] <- as.factor(ResultMatrixMelt[ ,"N"])  
  plot_box_T <- ggplot(ResultMatrixMelt, aes(x=N, y=MSE, fill=methods)) + 
    geom_boxplot() +
    theme(legend.position = "left")+ ggtitle("T-learner") +
    xlab("training size")
  
  ResultMatrixMelt <- melt(ResultMatrix[ ,c(9:12, 13)],  id.vars = "N", variable.name = 'methods', value.name = "MSE")
  ResultMatrixMelt[, "N"] <- as.factor(ResultMatrixMelt[ ,"N"])  
  plot_box_S <- ggplot(ResultMatrixMelt, aes(x=N, y=MSE, fill=methods)) + 
    geom_boxplot() +
    theme(legend.position = "left")+ ggtitle("S-learner") +
    xlab("training size")
  
ggarrange(plots = list(plot_box_X, plot_box_T , plot_box_S))


############### 2. Simulated data set  #############
  SimulationTime = 50
  dim = 4
  
  for(n in c(1e3, 3e3, 5e3, 1e4, 2e4, 5e4)){
    print(n)
    results <- foreach(t=1:SimulationTime, .combine='rbind', .packages = c("BART", "MASS","neuralnet", "e1071", "randomForest"), .errorhandling = 'remove') %dopar%
      {
        
        # Simualtion settings
        
        ntrain <- n
        correlation_mat <- simulate_correlation_matrix(dim, alpha = 0)
        mu <- rep(0, dim)
        feature_train <-
          data.frame(MASS::mvrnorm(
            n = ntrain,
            mu = mu,
            Sigma = correlation_mat
          ))
        tr_train <- rbinom(ntrain, size = 1, prob = 0.2)
        beta <- runif(dim, -1, 1)
        mu_0_train <- as.matrix(feature_train) %*% beta  
        treatment_effect_train <- 1 * (feature_train$X1 > 0)
        yobs_train <- as.vector(mu_0_train + tr_train * treatment_effect_train + rnorm(ntrain, 0, 0.1))
        
        ntest <- 2000
        feature_new <- data.frame(MASS::mvrnorm(
          n = ntest,
          mu = mu,
          Sigma = correlation_mat
        ))
        
        cate_true <- 1 * (feature_new$X1 > 0)
        
        yobs <- yobs_train
        feat <- feature_train
        tr <- tr_train

        
        ####   X-leaners  ####
        
        yobs_0 <- yobs[tr == 0]
        X_0 <- feat[tr == 0, ]
        yobs_1 <- yobs[tr == 1]
        X_1 <- feat[tr == 1, ]
        
        n_1 <- sum(tr)
        n_0 <- sum(1 - tr)
        
        g_weights <- sum(tr) / length(tr)
        
        f_0_test_set <- X_1
        f_1_test_set <- X_0
        
        # 1. BART
        bart_f_1 <- wbart(x.train = X_0, 
                          y.train = yobs_0, 
                          x.test = f_0_test_set,
                          ndpost=400L, nskip=100L)
        
        mu_hat_1_bart <- bart_f_1[["yhat.test.mean"]][1:n_1]
        
        bart_f_0  <- wbart(x.train = X_1, 
                           y.train = yobs_1, 
                           x.test = f_1_test_set,
                           ndpost=400L, nskip=100L)
        
        mu_hat_0_bart <- bart_f_0[["yhat.test.mean"]][1:n_0]
        
        
        D_1_bart <- yobs_1 - mu_hat_1_bart
        D_0_bart <- mu_hat_0_bart - yobs_0
        
        bart_s_1 <- wbart(x.train = X_1, 
                          y.train = D_1_bart, 
                          x.test = feature_new,
                          ndpost=400L, nskip=100L)
        
        tau_hat_1_bart <- bart_s_1[["yhat.test.mean"]]
        
        bart_s_0  <- wbart(x.train = X_0, 
                           y.train = D_0_bart, 
                           x.test = feature_new,
                           ndpost=400L, nskip=100L)
        
        tau_hat_0_bart <- bart_s_0[["yhat.test.mean"]]
        
        pred_bart <- g_weights * tau_hat_0_bart + (1 - g_weights) * tau_hat_1_bart
        
        mse_bart_x <- mean((cate_true - pred_bart)^2)
        
        # 2. Neural Networks
        
        NN_f_1 <- neuralnet(as.formula(paste("yobs_0~", paste(names(X_0), collapse = " + "))), 
                            data = data.frame(yobs_0, X_0), 
                            hidden = 4, 
                            threshold = 0.1, linear.output = FALSE,
                            err.fct = 'sse')
        mu_hat_1_NN <- predict(NN_f_1, newdata =  data.frame(f_0_test_set))
        NN_f_0 <- neuralnet(as.formula(paste("yobs_1~", paste(names(X_1), collapse = " + "))), 
                            data = data.frame(yobs_1, X_1), 
                            hidden = 4, 
                            threshold = 0.1, linear.output = FALSE,
                            err.fct = 'sse')
        mu_hat_0_NN <- predict(NN_f_0, newdata =  data.frame(f_1_test_set))
        
        D_1_NN <- yobs_1 - mu_hat_1_NN
        D_0_NN <- mu_hat_0_NN - yobs_0
        
        
        NN_s_1 <- neuralnet(as.formula(paste("D_1_NN~", paste(names(X_1), collapse = " + "))), 
                            data = data.frame(D_1_NN, X_1), 
                            hidden = 4, 
                            threshold = 0.1, linear.output = FALSE,
                            err.fct = 'sse')
        
        tau_hat_1_NN <- predict(NN_s_1, newdata =  data.frame(feature_new))
        
        NN_s_0 <- neuralnet(as.formula(paste("D_0_NN~", paste(names(X_0), collapse = " + "))), 
                            data = data.frame(D_0_NN, X_0), 
                            hidden = 4, 
                            threshold = 0.1, linear.output = FALSE,
                            err.fct = 'sse')
        
        tau_hat_0_NN <- predict(NN_s_0, newdata =  data.frame(feature_new))
        
        pred_NN <- g_weights * tau_hat_0_NN + (1 - g_weights) * tau_hat_1_NN
        
        mse_NN_x <- mean((cate_true - pred_NN)^2)
        
        # 3. randomForest
        
        rf_f_1 <- randomForest(yobs_0~., data = data.frame(yobs_0, X_0), xtest = f_0_test_set)
        mu_hat_1_rf <- rf_f_1$test$predicted
        rf_f_0 <- randomForest(yobs_1~., data = data.frame(yobs_1, X_1), xtest = f_1_test_set)
        mu_hat_0_rf <- rf_f_0$test$predicted
        D_1_rf <- yobs_1 - mu_hat_1_rf
        D_0_rf <- mu_hat_0_rf - yobs_0
        rf_s_1 <- randomForest(D_1_rf~., data = data.frame(D_1_rf, X_1), xtest = feature_new)
        tau_hat_1_rf <- rf_s_1$test$predicted
        rf_s_0 <- randomForest(D_0_rf~., data = data.frame(D_0_rf, X_0), xtest = feature_new)
        tau_hat_0_rf <- rf_s_0$test$predicted
        pred_rf<- g_weights * tau_hat_0_rf + (1 - g_weights) * tau_hat_1_rf
        mse_rf_x <- mean((cate_true - pred_rf)^2)
        
        # 4. SVM
        svm_f_1 <- svm(yobs_0~., data = data.frame(yobs_0, X_0), kernal="radial basis")
        mu_hat_1_svm <- predict(svm_f_1, newdata =  data.frame(f_0_test_set))
        svm_f_0 <- svm(yobs_1~., data = data.frame(yobs_1, X_1), kernal="radial basis")
        mu_hat_0_svm <- predict(svm_f_0, newdata =  data.frame(f_1_test_set))
        D_1_svm <- yobs_1 - mu_hat_1_svm
        D_0_svm <- mu_hat_0_svm - yobs_0
        svm_s_1 <- randomForest(D_1_svm~., data = data.frame(D_1_svm, X_1), xtest = feature_new)
        tau_hat_1_svm <- svm_s_1$test$predicted
        svm_s_0 <- randomForest(D_0_svm~., data = data.frame(D_0_svm, X_0), xtest = feature_new)
        tau_hat_0_svm <- svm_s_0$test$predicted
        pred_svm <- g_weights * tau_hat_0_svm + (1 - g_weights) * tau_hat_1_svm
        mse_svm_x <- mean((cate_true - pred_svm)^2)
        
        
        ####   T-leaners  ####
        
        # 1. BART
        
        bart_t_0 <- wbart(x.train = X_0, 
                          y.train = yobs_0, 
                          x.test = feature_new,
                          ndpost=400L, nskip=100L)
        
        mu_hat_0_bart_t <- bart_t_0[["yhat.test.mean"]]
        
        bart_t_1  <- wbart(x.train = X_1, 
                           y.train = yobs_1, 
                           x.test = feature_new,
                           ndpost=400L, nskip=100L)
        
        mu_hat_1_bart_t <- bart_t_1[["yhat.test.mean"]]
        
        ?wbart
        
        pred_bart_t <- mu_hat_1_bart_t - mu_hat_0_bart_t
        mse_bart_t <- mean((cate_true - pred_bart)^2)
        
        # 2. NN
        NN_t_0 <- neuralnet(as.formula(paste("yobs_0~", paste(names(X_0), collapse = " + "))), 
                            data = data.frame(yobs_0, X_0), 
                            hidden = 4, 
                            threshold = 0.1, linear.output = FALSE,
                            err.fct = 'sse')
        mu_hat_0_NN_t <- predict(NN_t_0, newdata =  feature_new)
        NN_t_1 <- neuralnet(as.formula(paste("yobs_1~", paste(names(X_1), collapse = " + "))), 
                            data = data.frame(yobs_1, X_1), 
                            hidden = 4, 
                            threshold = 0.1, linear.output = FALSE,
                            err.fct = 'sse')
        mu_hat_1_NN_t <- predict(NN_t_1, newdata =  feature_new)
        
        pred_NN_t <- mu_hat_1_NN_t -  mu_hat_0_NN_t
        
        mse_NN_t <- mean((cate_true - pred_NN_t)^2)
        
        # 3. random forest
        
        rf_t_0 <- randomForest(yobs_0~., data = data.frame(yobs_0, X_0), xtest = feature_new)
        mu_hat_0_rf_t <- rf_t_0$test$predicted
        rf_t_1 <- randomForest(yobs_1~., data = data.frame(yobs_1, X_1), xtest = feature_new)
        mu_hat_1_rf_t <- rf_t_1$test$predicted
        
        pred_rf_t <- mu_hat_1_rf_t - mu_hat_0_rf_t
        mse_rf_t <- mean((pred_rf_t - cate_true)^2)
        
        # 4. SVM
        
        svm_t_0 <- svm(yobs_0~., data = data.frame(yobs_0, X_0), kernal="radial basis")
        mu_hat_0_svm_t <- predict(svm_t_0, newdata =  data.frame(feature_new))
        svm_t_1 <- svm(yobs_1~., data = data.frame(yobs_1, X_1), kernal="radial basis")
        mu_hat_1_svm_t <- predict(svm_t_1, newdata =  data.frame(feature_new))
        
        pred_svm_t <- mu_hat_1_svm_t - mu_hat_0_svm_t
        mse_svm_t <- mean((pred_svm_t - cate_true)^2)
        
        
        ####   S-leaners  ####  
        
        # 1. bart
        
        bart_S <- wbart(x.train = data.frame(feature_train, tr), 
                        y.train = yobs_train,
                        ndpost=400L, nskip=100L)
        mu_hat_1_bart_S <- predict(bart_S, newdata = data.frame(feature_new, tr = 1))
        mu_hat_0_bart_S <- predict(bart_S, newdata = data.frame(feature_new, tr = 0))
        pred_bart_S <- mu_hat_1_bart_S - mu_hat_0_bart_S
        mse_bart_S <- mean((cate_true - pred_bart_S)^2)
        
        # 2. NN
        NN_S <- neuralnet(as.formula(paste("yobs~", paste(names(feature_train), collapse = " + "), "+ tr")), 
                          data = data.frame(yobs, feature_train, tr), 
                          hidden = 4, 
                          threshold = 0.1, linear.output = FALSE,
                          err.fct = 'sse')
        mu_hat_1_NN_S <- predict(NN_S, newdata = data.frame(feature_new, tr = 1))
        mu_hat_0_NN_S <- predict(NN_S, newdata = data.frame(feature_new, tr = 0))
        pred_NN_S <- mu_hat_1_NN_S - mu_hat_0_NN_S
        mse_NN_S <- mean((cate_true - pred_NN_S)^2)
        
        #3. random forest
        rf_S <- randomForest(yobs~., data = data.frame(yobs, feature_train, tr))
        mu_hat_1_rf_S <- predict(rf_S, newdata = data.frame(feature_new, tr = 1))
        mu_hat_0_rf_S <- predict(rf_S, newdata = data.frame(feature_new, tr = 0))
        pred_rf_S <- mu_hat_1_rf_S - mu_hat_0_rf_S
        mse_rf_S <- mean((cate_true - pred_rf_S)^2)
        
        #4. SVM
        svm_S <- svm(yobs~., data = data.frame(yobs, feature_train, tr), kernal="radial basis")
        mu_hat_1_svm_S <- predict(svm_S, newdata = data.frame(feature_new, tr = 1))
        mu_hat_0_svm_S <- predict(svm_S, newdata = data.frame(feature_new, tr = 0))
        pred_svm_S <- mu_hat_1_svm_S - mu_hat_0_svm_S
        mse_svm_S <- mean((cate_true - pred_svm_S)^2)
        
        result <- c(mse_bart_x, mse_NN_x, mse_rf_x, mse_svm_x, mse_bart_t, mse_NN_t, mse_rf_t, mse_svm_t, mse_bart_S, mse_NN_S, mse_rf_S, mse_svm_S)
        
        return(result)
      }
    save(results, file=paste("D://n = ", n, ".Rdata"))
  } 
 
  results = rbind(results, results2)
  
  ResultMatrix <- c()
  for(n in c(1e3, 3e3, 5e3, 1e4, 2e4, 5e4)){
    load(file=paste("D://n = ", n, ".Rdata"))
    ResultMatrix <- rbind(ResultMatrix, cbind(results, n))
  }
  
  ResultMatrix <- data.frame(ResultMatrix)
  
  colnames(ResultMatrix) <- c("BART_X", "NN_X", "RF_X", "SVM_X",
                              "BART_T", "NN_T", "RF_T", "SVM_T", 
                              "BART_S", "NN_S", "RF_S", "SVM_S", "N")
  
  ResultMatrixMelt <- melt(ResultMatrix[ ,c(1:4, 13)],  id.vars = "N", variable.name = 'methods', value.name = "MSE")
  ResultMatrixMelt[, "N"] <- as.factor(ResultMatrixMelt[ ,"N"])  
  plot_box_X <- ggplot(ResultMatrixMelt, aes(x=N, y=MSE, fill=methods)) + 
    geom_boxplot() +
    theme(legend.position = "left")+  ggtitle("X-learner") +
    xlab("training size")
  
  ResultMatrixMelt <- melt(ResultMatrix[ ,c(5:8, 13)],  id.vars = "N", variable.name = 'methods', value.name = "MSE")
  ResultMatrixMelt[, "N"] <- as.factor(ResultMatrixMelt[ ,"N"])  
  plot_box_T <- ggplot(ResultMatrixMelt, aes(x=N, y=MSE, fill=methods)) + 
    geom_boxplot() +
      theme(legend.position = "left")+ ggtitle("T-learner") +
    xlab("training size")
  
  ResultMatrixMelt <- melt(ResultMatrix[ ,c(9:12, 13)],  id.vars = "N", variable.name = 'methods', value.name = "MSE")
  ResultMatrixMelt[, "N"] <- as.factor(ResultMatrixMelt[ ,"N"])  
  plot_box_S <- ggplot(ResultMatrixMelt, aes(x=N, y=MSE, fill=methods)) + 
    geom_boxplot() +
    theme(legend.position = "left")+ ggtitle("S-learner")+
    xlab("training size")
  
  ggarrange(plots = list(plot_box_X, plot_box_T , plot_box_S))
  
