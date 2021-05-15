library(ggrepel)
library(reshape2)
library(scales)

# A_X = c(0.1, 0.2, 0.3)
# A_Y = c(1, 2, 3)
# A_M = c("A1", "A2", "A3")
# DF_A <- data.frame(
#   Average_CI_Length = A_X, lotlll
#   Coverage = A_Y, 
#   PointName = A_M
# )  
# 
# B_X = c(0.15, 0.25, 0.35)
# B_Y = c(1.5, 2.5, 3.5)
# B_M = c("B1", "B2", "B3")
# DF_B <- data.frame(
#   Average_CI_Length = B_X, 
#   Coverage = B_Y, 
#   PointName = B_M
# )
# 
# C_X = c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35)
# C_Y = c(1, 1.5, 2, 2.5, 3, 3.5)
# C_Name = c("RF", "BART", "RF", "BART", "RF", "BART")
# C_Color = c("RAW", "RAW", "B", "B", "K", "K")
# C_Shape = c("RAW", "RAW", "B", "B", "K", "K")
# DF_C <- data.frame(
#   Average_CI_Length = C_X, 
#   Coverage = C_Y, 
#   PointName = C_Name,
#   ColorGroup = C_Color,
#   ShapeGroup = C_Shape
# )

Ploter_Bias<- function(DF){
  DF_Plot <- melt(DF ,  id.vars = 'train_size', variable.name = 'series')
  p <- ggplot(DF_Plot, aes(train_size, value)) + geom_line(aes(colour = series))
  p
}

Get_Ploter_DF_Bias_V2<- function(DF, vec_train_size){
  
  DF_sl <- data.frame(
    train_size = vec_train_size,
    sl_rf = DF$sl_rf, 
    sl_bart = DF$sl_bart, 
    sl_rf_B = DF$sl_rf_B, 
    sl_bart_B = DF$sl_bart_B, 
    sl_rf_K = DF$sl_rf_K,
    sl_bart_K = DF$sl_bart_K)
  
  DF_tl <- data.frame(
    train_size = vec_train_size,
    tl_rf = DF$tl_rf,
    tl_bart = DF$tl_bart,
    tl_rf_B = DF$tl_rf_B,
    tl_bart_B = DF$tl_bart_B,
    tl_rf_K = DF$tl_rf_K,
    tl_bart_K = DF$tl_bart_K)
  
  DF_xl <- data.frame( 
    train_size = vec_train_size,
    xl_rf = DF$xl_rf,
    xl_bart = DF$xl_bart,
    xl_rf_B = DF$xl_rf_B,
    xl_bart_B = DF$xl_bart_B,
    xl_rf_K = DF$xl_rf_K,
    xl_bart_K = DF$xl_bart_K)
  
  list_rtn = list(DF_sl=DF_sl, DF_tl=DF_tl, DF_xl=DF_xl)
  return(list_rtn)
  
}

Get_Ploter_DF_Bias<- function(DF, vec_trials){
  
  sl_rf = c()
  sl_bart = c()
  tl_rf = c()
  tl_bart = c()
  xl_rf = c()
  xl_bart = c()
  
  sl_rf_B = c()
  sl_bart_B = c()
  tl_rf_B = c()
  tl_bart_B = c()
  xl_rf_B = c()
  xl_bart_B = c()
  
  sl_rf_K = c()
  sl_bart_K = c()
  tl_rf_K = c()
  tl_bart_K = c()
  xl_rf_K = c()
  xl_bart_K = c()
  
  for(v in vec_trials){
    sl_rf_v = mean(DF$sl_rf[1:v])
    sl_rf = append(sl_rf, sl_rf_v)
    
    sl_bart_v = mean(DF$sl_bart[1:v])
    sl_bart = append(sl_bart, sl_bart_v)
    
    tl_rf_v = mean(DF$tl_rf[1:v])
    tl_rf = append(tl_rf, tl_rf_v)
    
    tl_bart_v = mean(DF$tl_bart[1:v])
    tl_bart = append(tl_bart, tl_bart_v)
    
    xl_rf_v = mean(DF$xl_rf[1:v])
    xl_rf = append(xl_rf, xl_rf_v)
    
    xl_bart_v = mean(DF$xl_bart[1:v])
    xl_bart = append(xl_bart, xl_bart_v)
    
    
    sl_rf_B_v = mean(DF$sl_rf_B[1:v])
    sl_rf_B = append(sl_rf_B, sl_rf_B_v)
    
    sl_bart_B_v = mean(DF$sl_bart_B[1:v])
    sl_bart_B = append(sl_bart_B, sl_bart_B_v)
    
    tl_rf_B_v = mean(DF$tl_rf_B[1:v])
    tl_rf_B = append(tl_rf_B, tl_rf_B_v)
    
    tl_bart_B_v = mean(DF$tl_bart_B[1:v])
    tl_bart_B = append(tl_bart_B, tl_bart_B_v)
    
    xl_rf_B_v = mean(DF$xl_rf_B[1:v])
    xl_rf_B = append(xl_rf_B, xl_rf_B_v)
    
    xl_bart_B_v = mean(DF$xl_bart_B[1:v])
    xl_bart_B = append(xl_bart_B, xl_bart_B_v)
    
    
    sl_rf_K_v = mean(DF$sl_rf_K[1:v])
    sl_rf_K = append(sl_rf_K, sl_rf_K_v)
    
    sl_bart_K_v = mean(DF$sl_bart_K[1:v])
    sl_bart_K = append(sl_bart_K, sl_bart_K_v)
    
    tl_rf_K_v = mean(DF$tl_rf_K[1:v])
    tl_rf_K = append(tl_rf_K, tl_rf_K_v)
    
    tl_bart_K_v = mean(DF$tl_bart_K[1:v])
    tl_bart_K = append(tl_bart_K, tl_bart_K_v)
    
    xl_rf_K_v = mean(DF$xl_rf_K[1:v])
    xl_rf_K = append(xl_rf_K, xl_rf_K_v)
    
    xl_bart_K_v = mean(DF$xl_bart_K[1:v])
    xl_bart_K = append(xl_bart_K, xl_bart_K_v)
    
  }
  
  DF_sl <- data.frame(
    trials = vec_trials,
    sl_rf = sl_rf, 
    sl_bart = sl_bart, 
    sl_rf_B = sl_rf_B, 
    sl_bart_B = sl_bart_B, 
    sl_rf_K = sl_rf_K,
    sl_bart_K = sl_bart_K)
  
  DF_tl <- data.frame(
    trials = vec_trials,
    tl_rf = tl_rf,
    tl_bart = tl_bart,
    tl_rf_B = tl_rf_B,
    tl_bart_B = tl_bart_B,
    tl_rf_K = tl_rf_K,
    tl_bart_K = tl_bart_K)
   
  DF_xl <- data.frame( 
    trials = vec_trials,
    xl_rf = xl_rf,
    xl_bart = xl_bart,
    xl_rf_B = xl_rf_B,
    xl_bart_B = xl_bart_B,
    xl_rf_K = xl_rf_K,
    xl_bart_K = xl_bart_K)
  
  list_rtn = list(DF_sl=DF_sl, DF_tl=DF_tl, DF_xl=DF_xl)
  return(list_rtn)

}
Ploter_CI<- function(DF){
  
  p <- ggplot(DF, aes(x=Average_CI_Length, y=Coverage, color=ColorGroup, shape=ShapeGroup)) + geom_point() +geom_text_repel(aes(label=PointName), size=5) 
  #+ scale_x_continuous(expand = c(.025, .025)) + scale_y_continuous(expand = c(.2, .2))
  p
}