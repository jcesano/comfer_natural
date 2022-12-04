# This function create a list with:
#  - "unique" id based on the date and time of the system
#  - "Date": containing the date used to create the Id
#  - "Time": containing the time used to create the Id
generate_unique_id <- function(){
  current_datetime <- Sys.time()
  #current_datetime
  datetime_splitted <- strsplit(as.character(current_datetime), split = ' ')
  
  Id_generated <- gsub("-","",current_datetime)
  Id_generated <- gsub(" ","_",Id_generated)
  Id_generated <- gsub(":","",Id_generated)
  
  Date <- datetime_splitted[[1]][1]
  Time <- datetime_splitted[[1]][2]
  multiple_result_list <- list("Id" = Id_generated, "Date" = Date, "Time" = Time)
  return(multiple_result_list)
}

create_sim_params_data_frame <- function(pop, iniY, endY, ini_c, n0, nsim, ne, iter, N){
  sim_names <- c("pop", "iniY", "endY", "ini_c", "n0", "nsim", "ne", "iter", "N")
  sim_values <- c(pop, iniY, endY, ini_c, n0, nsim, ne, iter, N)
  sim_params <- data.frame(sim_names, sim_values)
  
  return(sim_params)
}

# create an empty log dataframe and save it as a csv file in the log_path
create_and_save_empty_log <- function(log_path, log_filename){
  
  full_log_path <- file.path(log_path,log_filename)
  
  column_names <- c("Date", "Time", "Id", "Fake_results","pop", "iniY", "endY", "ini_c", "n0", "nsim", "ne", "iter", "N", "alpha", "kappa", "g_shape", "g_rate", "mau", "nsp", "fake_alpha", "fake_kappa", "fake_g_shape", "fake_g_rate", "fake_mau", "fake_nsp")
  
  df_log <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  
  colnames(df_log) <- column_names
  
  write.csv(df_log, full_log_path, row.names = TRUE)
  
  return(df_log)
}

insert_in_log_only_sim <- function(execution_log, log_path_filename, id_datetime_result, sim_params, info_rank_1){
  
  new_pos <- nrow(execution_log)+1
  
  # current_datetime <- Sys.time()
  # current_datetime
  # datetime_splitted <- strsplit(as.character(current_datetime), split = ' ')
  # 
  # Id_generated <- gsub("-","",current_datetime)
  # Id_generated <- gsub(" ","_",Id_generated)
  # Id_generated <- gsub(":","",Id_generated)
  
  # id_datetime_result <- generate_unique_id()
  Id_generated <- id_datetime_result$Id
  date_id <- id_datetime_result$Date
  time_id <-  id_datetime_result$Time
  
  execution_log[new_pos,]$Id <- Id_generated
  # execution_log[new_pos,]$Date <- datetime_splitted[[1]][1]
  # execution_log[new_pos,]$Time <- datetime_splitted[[1]][2]
  
  execution_log[new_pos,]$Date <- date_id
  execution_log[new_pos,]$Time <- time_id
  execution_log[new_pos,]$Fake_results <- FALSE
  
  execution_log[new_pos,]$pop <- sim_params[1,2]
  execution_log[new_pos,]$iniY <- sim_params[2,2]
  execution_log[new_pos,]$endY <- sim_params[3,2]
  execution_log[new_pos,]$ini_c <- sim_params[4,2]
  execution_log[new_pos,]$n0 <- sim_params[5,2]
  execution_log[new_pos,]$nsim <- sim_params[6,2]
  execution_log[new_pos,]$ne <- sim_params[7,2]
  execution_log[new_pos,]$iter <- sim_params[8,2]
  execution_log[new_pos,]$N <- sim_params[9,2]
  
  execution_log[new_pos,]$alpha <- info_rank_1[2,2]
  execution_log[new_pos,]$kappa <- info_rank_1[3,2]
  execution_log[new_pos,]$g_shape <- info_rank_1[4,2]
  execution_log[new_pos,]$g_rate <- info_rank_1[5,2]
  execution_log[new_pos,]$mau <- info_rank_1[6,2]
  execution_log[new_pos,]$nsp <- info_rank_1[7,2]
  
  execution_log[new_pos,]$fake_alpha <- 0
  execution_log[new_pos,]$fake_kappa <- 0
  execution_log[new_pos,]$fake_g_shape <- 0
  execution_log[new_pos,]$fake_g_rate <- 0
  execution_log[new_pos,]$fake_mau <- 0
  execution_log[new_pos,]$fake_nsp <- 0
  
  write.csv(execution_log, log_path_filename, row.names = TRUE)
  
  return(execution_log)
}

insert_in_log_complete <- function(execution_log, log_path_filename, id_datetime_result, sim_params,info_rank_1, info_fake){
  
  new_pos <- nrow(execution_log)+1
  
  #current_datetime <- Sys.time()
  #current_datetime
  #datetime_splitted <- strsplit(as.character(current_datetime), split = ' ')
  #Id_generated <- gsub("-","",current_datetime)
  #Id_generated <- gsub(" ","_",Id_generated)
  #Id_generated <- gsub(":","",Id_generated)
  
  # id_datetime_result <- generate_unique_id()
  Id_generated <- id_datetime_result$Id
  date_id <- id_datetime_result$Date
  time_id <-  id_datetime_result$Time
  
  execution_log[new_pos,]$Id <- Id_generated
  # execution_log[new_pos,]$Date <- datetime_splitted[[1]][1]
  # execution_log[new_pos,]$Time <- datetime_splitted[[1]][2]
  execution_log[new_pos,]$Date <- date_id
  execution_log[new_pos,]$Time <- time_id
  execution_log[new_pos,]$Fake_results <- TRUE
  
  execution_log[new_pos,]$pop <- sim_params[1,2]
  execution_log[new_pos,]$iniY <- sim_params[2,2]
  execution_log[new_pos,]$endY <- sim_params[3,2]
  execution_log[new_pos,]$ini_c <- sim_params[4,2]
  execution_log[new_pos,]$n0 <- sim_params[5,2]
  execution_log[new_pos,]$nsim <- sim_params[6,2]
  execution_log[new_pos,]$ne <- sim_params[7,2]
  execution_log[new_pos,]$iter <- sim_params[8,2]
  execution_log[new_pos,]$N <- sim_params[9,2]
  execution_log[new_pos,]$alpha <- info_rank_1[2,2]
  execution_log[new_pos,]$kappa <- info_rank_1[3,2]
  execution_log[new_pos,]$g_shape <- info_rank_1[4,2]
  execution_log[new_pos,]$g_rate <- info_rank_1[5,2]
  execution_log[new_pos,]$mau <- info_rank_1[6,2]
  execution_log[new_pos,]$nsp <- info_rank_1[7,2]
  
  execution_log[new_pos,]$fake_alpha <- info_fake[2,2]
  execution_log[new_pos,]$fake_kappa <- info_fake[3,2]
  execution_log[new_pos,]$fake_g_shape <- info_fake[4,2]
  execution_log[new_pos,]$fake_g_rate <- info_fake[5,2]
  execution_log[new_pos,]$fake_mau <- info_fake[6,2]
  execution_log[new_pos,]$fake_nsp <- info_fake[7,2]
  
  write.csv(execution_log, log_path_filename, row.names = TRUE)
  
  return(execution_log)
}

get_log <-function(log_path_filename){
  df_log <- read.csv(file=log_path_filename,header = TRUE)
  
  return(df_log)
}

save_log <-function(id_datetime_result, sim_params, info_sim, info_fake, fake_obs = TRUE){
  # saving information of fake data to generate population and information of simulation of the model
  save_log_complete <-function(log_path, log_filename, sim_params, info_sim, info_fake){
    
    # browser()
    execution_log <- get_log(file.path(log_path,log_filename))
    execution_log <- insert_in_log_complete(execution_log, file.path(log_path, log_filename), id_datetime_result, sim_params, info_sim, info_fake)
    
    return(execution_log)
  }
  
  # saving only data of simulation of the model
  save_log_only_sim <- function(log_path, log_filename, sim_params, info_sim){
    execution_log <- get_log(file.path(log_path,log_filename))
    execution_log <- insert_in_log_only_sim(execution_log, file.path(log_path, log_filename), id_datetime_result, sim_params, info_true)
    
    return(execution_log)
  }
  
  log_filename <- "comfert_natural_log.csv"
  log_path <- file.path("..","log")
  
  # log_df <- get_log(log_path, log_filename)
  
  if(fake_obs){
    current_log <- save_log_complete(log_path, log_filename, sim_params, info_sim, info_fake)
  }
  else{
    current_log <- save_log_only_sim(log_path, log_filename,info_sim)
  }
  
  return(current_log)
}


#### Testing Code #####
# log_path <- "/Users/juliocesano/Nextcloud/comfert/comfert_natural/compute/log"
# log_filename <- "comfer_natural_log.csv"
# 
# create_and_save_empty_log(log_path, log_filename)
# 
# # get all the information to be logged in the log_dataframe
# info_fake <- read.table("Info.txt",sep=":",dec=".")
# info_rank_1 <- read.table("Info.txt",sep=":",dec=".")
# sim_params <- create_sim_params_data_frame(pop, iniY, endY, ini_c, n0, nsim, ne, iter, N)
# 
# # Log all the information in the model execution log
# save_log(log_path, log_filename, sim_params, info_fake, info, fake_obs = TRUE)
# 
