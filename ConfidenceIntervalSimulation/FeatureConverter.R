Test_Converter_Print <- function(str_print) {
  print(str_print)
}

Test_Converter_Print2 <- function(str_print) {
  print(paste(str_print, "22222222"))
}

Convert_Dataset_Binary <- function(dataset_train, dataset_test, depth_bt, type_root) {
  
  # list_mat_1 = Convert_Column_Binary(dataset_train[ , 1], dataset_test[ , 1], depth_bt, type_root)
  # list_mat_2 = Convert_Column_Binary(dataset_train[ , 2], dataset_test[ , 2], depth_bt, type_root)
  # mat_train = cbind(list_mat_1$train, list_mat_2$train)
  # mat_test = cbind(list_mat_1$test, list_mat_2$test)
  # print(mat_train)
  # print(mat_test)
  # df_train = as.data.frame(mat_train)
  # df_test = as.data.frame(mat_test)
  # list_rtn = list(train = df_train, test = df_test)
  # 
  # return(list_rtn)
  
  N_train = length(dataset_train[ , 1])
  N_test = length(dataset_test[ , 1])
  # N_train = 3
  # N_test = 3
  mat_train = matrix(nrow = N_train, ncol = 0)
  mat_test = matrix(nrow = N_test, ncol = 0)
  # print(mat_c)

  for(i in 1:ncol(dataset_train)) {
    list_mat_i = Convert_Column_Binary(dataset_train[ , i], dataset_test[ , i], depth_bt, type_root)
    # print(mat_i)
    
    mat_train = cbind(mat_train, list_mat_i$train)
    mat_test = cbind(mat_test, list_mat_i$test)
    
  }

  df_train = as.data.frame(mat_train)
  df_test = as.data.frame(mat_test)
  # print(df_train)
  # print(df_test)
  
  list_rtn = list(train = df_train, test = df_test)
  return(list_rtn)
}

Convert_Column_Binary <- function(column_train, column_test, depth_bt, type_root) {
  
  root_bt = Helper_Create_BT(column_train, depth_bt, type_root)
  # print("----------------------------")
  # Helper_Traversal_In_Order(root_bt)
  
  
  mat_digits_train = Helper_Convert_Column_To_Bin(column_train, root_bt, depth_bt)
  mat_digits_test = Helper_Convert_Column_To_Bin(column_test, root_bt, depth_bt)
  
  list_rtn = list(train = mat_digits_train, test = mat_digits_test)
  return(list_rtn)
}

Helper_Create_BT<- function(arr_data, depth_bt, type_root){
  
  m_data = mean(arr_data)
  if(type_root=="median"){
    m_data = median(arr_data)
  }
  
  node = list(val_m = m_data, left = NULL, right = NULL)
  
  
  if(depth_bt > 1){
    arr_data_l = arr_data[(arr_data<=m_data)]
    node_left = Helper_Create_BT(arr_data_l, depth_bt - 1, type_root)
    node$left = node_left
    
    arr_data_g = arr_data[(arr_data>m_data)]
    node_right = Helper_Create_BT(arr_data_g, depth_bt - 1, type_root)
    node$right = node_right
  }
  
  return(node)
}

Helper_Convert_Column_To_Bin<- function(column, root_bt, depth_bt){
  N = length(column)
  # N = 3
  mat_digits = matrix(NA, nrow=N, ncol=depth_bt)
  
  for(i in 1:N){
    val = column[i]
    c_digits = Helper_Convert_Val_To_Bin(val, root_bt, depth_bt)
    
    mat_digits[i,] = c_digits
  }
  
  return(mat_digits)
}

Helper_Convert_Val_To_Bin<- function(val, root_bt, depth_bt){
  c_digits = c()
  
  node = root_bt
  # print(node)
  while(!is.null(node)){
    
    digit = 0
    node_next = node$left
    
    if(val > node$val_m){
      digit = 1
      node_next = node$right
    }
    
    c_digits = c(c_digits, digit)
    node = node_next
  }
  
  return(c_digits)
}

Helper_Traversal_In_Order <- function(node) {
  
  if(!is.null(node)){
    
    Helper_Traversal_In_Order(node$left)
    
    print(node$val_m)
    
    Helper_Traversal_In_Order(node$right)
  }
}

Convert_Dataset_KBin <- function(dataset_train, dataset_test, k_bin) {

  # list_mat_1 = Convert_Column_KBin(dataset_train[ , 1], dataset_test[ , 1], k_bin)
  # list_mat_2 = Convert_Column_KBin(dataset_train[ , 2], dataset_test[ , 2], k_bin)
  # 
  # mat_train = cbind(list_mat_1$train, list_mat_2$train)
  # mat_test = cbind(list_mat_1$test, list_mat_2$test)
  # print(mat_train)
  # print(mat_test)
  # df_train = as.data.frame(mat_train)
  # df_test = as.data.frame(mat_test)
  # list_rtn = list(train = df_train, test = df_test)
  # 
  # return(list_rtn)
  

  N_train = length(dataset_train[ , 1])
  N_test = length(dataset_test[ , 1])
  # N_train = 10
  # N_test = 10

  mat_train = matrix(nrow = N_train, ncol = 0)
  mat_test = matrix(nrow = N_test, ncol = 0)
  # print(mat_c)

  for(i in 1:ncol(dataset_train)) {
    list_mat_i = Convert_Column_KBin(dataset_train[ , i], dataset_test[ , i], k_bin)

    mat_train = cbind(mat_train, list_mat_i$train)
    mat_test = cbind(mat_test, list_mat_i$test)
  }

  df_train = as.data.frame(mat_train)
  df_test = as.data.frame(mat_test)

  # print(df_train)
  # print(df_test)
  
  list_rtn = list(train = df_train, test = df_test)
  return(list_rtn)
  
}

Convert_Column_KBin <- function(column_train, column_test, k_bin) {
  
  val_min = min(column_train)
  val_max = max(column_train)
  step_size = (val_max - val_min)/k_bin
  
  vec_threasholds = c()
  thd_i = val_min
  for(i in 1:(k_bin-1)){
    
    val_low = thd_i
    val_high = thd_i + step_size
    vec_threasholds = c(vec_threasholds, val_high)
    thd_i = val_high
  }
  
  # print(val_min)
  # print(val_max)
  # print(vec_threasholds)
  
  mat_train = Helper_Convert_Column_To_K(column_train, vec_threasholds, k_bin)
  mat_test = Helper_Convert_Column_To_K(column_test, vec_threasholds, k_bin)
  
  list_rtn = list(train = mat_train, test = mat_test)
  return(list_rtn)
}

Helper_Convert_Column_To_K <- function(column, vec_threasholds, k_bin) {
  N = length(column)
  # N = 10
  mat_digits = matrix(NA, nrow=N, ncol=1)
  
  for(i in 1:N){
    data = column[i]
    digit = -1
    for(j in 1:(k_bin-1)){
      thd_j = vec_threasholds[j]
      if(data <= thd_j){
        digit = j
        break
      }
    }
    
    if(digit == -1){
      digit = k_bin
    }
    
    mat_digits[i,] = digit
  }
  
  return(mat_digits)
}