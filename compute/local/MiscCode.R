library("rstudioapi") # This allows to automatically set working directory to source opened file location
setwd(dirname(getActiveDocumentContext()$path))

source(file.path("..","estimation","log.R"))

log_folder_path <- file.path("..","log")

# comparar opt_res_dir con el info.txt de fake_obs
info_fake <- read.table(file.path(log_folder_path,"Info_fake.txt"),sep=":",dec=".")
info_rank_1 <- read.table(file.path(log_folder_path,"Info.txt"),sep=":",dec=".")

pop <- "HT"  # pop for which simulations are performed 
iniY <- 1925 # years for which the simulations are performed
endY <- 1976 # years for which the simulations are performed
ini_c <- 2000 # size of the initial birth cohorts of the model 
n0 <- 40     # size of initial sample of param combinations
nsim <- 2    # nr of simulations in each evaluated point - this will produce a cluster of size n0*nsim
ne <- 40     # nr of new evaluations at each iteration of the bayes opt. algorithm
iter <- 20
N <- ne*iter     # total nr of evaluations = n0+N

 # This function create a list with:
 #  - "unique" id based on the date and time of the system
 #  - "Date": containing the date used to create the Id
 #  - "Time": containing the time used to create the Id
# generate_unique_id <- function(){
#   current_datetime <- Sys.time()
#   #current_datetime
#   datetime_splitted <- strsplit(as.character(current_datetime), split = ' ')
#   
#   Id_generated <- gsub("-","",current_datetime)
#   Id_generated <- gsub(" ","_",Id_generated)
#   Id_generated <- gsub(":","",Id_generated)
#   
#   Date <- datetime_splitted[[1]][1]
#   Time <- datetime_splitted[[1]][2]
#   multiple_result_list <- list("Id" = Id_generated, "Date" = Date, "Time" = Time)
#   return(multiple_result_list)
# }
 
# create_sim_params_data_frame <- function(pop, iniY, endY, ini_c, n0, nsim, ne, iter, N){
#   sim_names <- c("pop", "iniY", "endY", "ini_c", "n0", "nsim", "ne", "iter", "N")
#   sim_values <- c(pop, iniY, endY, ini_c, n0, nsim, ne, iter, N)
#   sim_params <- data.frame(sim_names, sim_values)
#   
#   return(sim_params)
# }

# create a dataframe with the hyperparameters used to execute the model
sim_params <- create_sim_params_data_frame(pop, iniY, endY, ini_c, n0, nsim, ne, iter, N)

sim_params
# sim_names sim_values
# 1       pop         HT
# 2      iniY       1925
# 3      endY       1976
# 4     ini_c       2000
# 5        n0         40
# 6      nsim          2
# 7        ne         40
# 8      iter         20
# 9         N        800

# info_fake
# V1    V2
# 1 Results obtained with combination of parameters     NA
# 2                                          alpha   35.50
# 3                                          kappa    0.37
# 4                                        g_shape    5.00
# 5                                         g_rate   37.50
# 6                                            mau   17.75
# 7                                            nsp    7.75

# info_rank_1
# V1         V2
# 1 Results obtained with combination of parameters          NA
# 2                                          alpha   35.1964700
# 3                                          kappa    0.2831802
# 4                                        g_shape    3.9446560
# 5                                         g_rate   37.7917500
# 6                                            mau   20.1100100
# 7                                            nsp    5.8630960

# create an empty log dataframe and save it as a csv file in the log_path
# create_and_save_empty_log <- function(log_path, log_filename){
#   
#   full_log_path <- file.path(log_path,log_filename)
#   
#   column_names <- c("Date", "Time", "Id", "Fake_results","pop", "iniY", "endY", "ini_c", "n0", "nsim", "ne", "iter", "N", "alpha", "kappa", "g_shape", "g_rate", "mau", "nsp", "fake_alpha", "fake_kappa", "fake_g_shape", "fake_g_rate", "fake_mau", "fake_nsp")
#   
#   df_log <- data.frame(matrix(ncol = length(column_names), nrow = 0))
#   
#   colnames(df_log) <- column_names
#   
#   write.csv(df_log, full_log_path, row.names = TRUE)
#   
#   return(df_log)
# }


# insert_in_log_only_sim <- function(execution_log, log_path_filename, id_datetime_result, sim_params, info_rank_1){
#   
#   new_pos <- nrow(execution_log)+1
  
  # current_datetime <- Sys.time()
  # current_datetime
  # datetime_splitted <- strsplit(as.character(current_datetime), split = ' ')
  # 
  # Id_generated <- gsub("-","",current_datetime)
  # Id_generated <- gsub(" ","_",Id_generated)
  # Id_generated <- gsub(":","",Id_generated)
  
  # id_datetime_result <- generate_unique_id()
  # Id_generated <- id_datetime_result$Id
  # date_id <- id_datetime_result$Date
  # time_id <-  id_datetime_result$Time
  
  # execution_log[new_pos,]$Id <- Id_generated
  # execution_log[new_pos,]$Date <- datetime_splitted[[1]][1]
  # execution_log[new_pos,]$Time <- datetime_splitted[[1]][2]
  
  # execution_log[new_pos,]$Date <- date_id
  # execution_log[new_pos,]$Time <- time_id
  # execution_log[new_pos,]$Fake_results <- FALSE
  
#   execution_log[new_pos,]$pop <- sim_params[1,2]
#   execution_log[new_pos,]$iniY <- sim_params[2,2]
#   execution_log[new_pos,]$endY <- sim_params[3,2]
#   execution_log[new_pos,]$ini_c <- sim_params[4,2]
#   execution_log[new_pos,]$n0 <- sim_params[5,2]
#   execution_log[new_pos,]$nsim <- sim_params[6,2]
#   execution_log[new_pos,]$ne <- sim_params[7,2]
#   execution_log[new_pos,]$iter <- sim_params[8,2]
#   execution_log[new_pos,]$N <- sim_params[9,2]
#   execution_log[new_pos,]$alpha <- info_rank_1[2,2]
#   execution_log[new_pos,]$kappa <- info_rank_1[3,2]
#   execution_log[new_pos,]$g_shape <- info_rank_1[4,2]
#   execution_log[new_pos,]$g_rate <- info_rank_1[5,2]
#   execution_log[new_pos,]$mau <- info_rank_1[6,2]
#   execution_log[new_pos,]$nsp <- info_rank_1[7,2]
#   
#   execution_log[new_pos,]$fake_alpha <- 0
#   execution_log[new_pos,]$fake_kappa <- 0
#   execution_log[new_pos,]$fake_g_shape <- 0
#   execution_log[new_pos,]$fake_g_rate <- 0
#   execution_log[new_pos,]$fake_mau <- 0
#   execution_log[new_pos,]$fake_nsp <- 0
#   
#   write.csv(execution_log, log_path_filename, row.names = TRUE)
#   
#   return(execution_log)
# }

# insert_in_log_complete <- function(execution_log, log_path_filename, id_datetime_result, sim_params,info_rank_1, info_fake){
#   
#   new_pos <- nrow(execution_log)+1
  
  #current_datetime <- Sys.time()
  #current_datetime
  #datetime_splitted <- strsplit(as.character(current_datetime), split = ' ')
  #Id_generated <- gsub("-","",current_datetime)
  #Id_generated <- gsub(" ","_",Id_generated)
  #Id_generated <- gsub(":","",Id_generated)
  
  # id_datetime_result <- generate_unique_id()
  # Id_generated <- id_datetime_result$Id
  # date_id <- id_datetime_result$Date
  # time_id <-  id_datetime_result$Time
  # 
  # execution_log[new_pos,]$Id <- Id_generated
  # execution_log[new_pos,]$Date <- datetime_splitted[[1]][1]
  # execution_log[new_pos,]$Time <- datetime_splitted[[1]][2]
#   execution_log[new_pos,]$Date <- date_id
#   execution_log[new_pos,]$Time <- time_id
#   execution_log[new_pos,]$Fake_results <- TRUE
#   
#   execution_log[new_pos,]$pop <- sim_params[1,2]
#   execution_log[new_pos,]$iniY <- sim_params[2,2]
#   execution_log[new_pos,]$endY <- sim_params[3,2]
#   execution_log[new_pos,]$ini_c <- sim_params[4,2]
#   execution_log[new_pos,]$n0 <- sim_params[5,2]
#   execution_log[new_pos,]$nsim <- sim_params[6,2]
#   execution_log[new_pos,]$ne <- sim_params[7,2]
#   execution_log[new_pos,]$iter <- sim_params[8,2]
#   execution_log[new_pos,]$N <- sim_params[9,2]
#   execution_log[new_pos,]$alpha <- info_rank_1[2,2]
#   execution_log[new_pos,]$kappa <- info_rank_1[3,2]
#   execution_log[new_pos,]$g_shape <- info_rank_1[4,2]
#   execution_log[new_pos,]$g_rate <- info_rank_1[5,2]
#   execution_log[new_pos,]$mau <- info_rank_1[6,2]
#   execution_log[new_pos,]$nsp <- info_rank_1[7,2]
#   
#   execution_log[new_pos,]$fake_alpha <- info_fake[2,2]
#   execution_log[new_pos,]$fake_kappa <- info_fake[3,2]
#   execution_log[new_pos,]$fake_g_shape <- info_fake[4,2]
#   execution_log[new_pos,]$fake_g_rate <- info_fake[5,2]
#   execution_log[new_pos,]$fake_mau <- info_fake[6,2]
#   execution_log[new_pos,]$fake_nsp <- info_fake[7,2]
#   
#   write.csv(execution_log, log_path_filename, row.names = TRUE)
#   
#   return(execution_log)
# }

comfert_log_file <- "comfert_natural_log.csv"

id_datetime_result <- generate_unique_id()


df_log <- create_and_save_empty_log(log_folder_path, comfert_log_file)

df_log

save_log(id_datetime_result, sim_params, info_rank_1, info_fake, fake_obs = TRUE)

# df_log <- insert_in_log_complete(df_log, file.path(log_folder_path, comfert_log_file), id_datetime_result, sim_params, info_rank_1, info_fake)
# 
# df_log
# 
# df_log <- insert_in_log_complete(df_log, file.path(log_folder_path, comfert_log_file), id_datetime_result, sim_params, info_rank_1, info_fake)
# 
# df_log <- insert_in_log_only_sim(df_log, file.path(log_folder_path, comfert_log_file), id_datetime_result, sim_params, info_rank_1)
# df_log
# sim_params[1,]$sim_names
# nrow(df_log)
# sim_params[1,2]
# 
# new_pos <- nrow(df_log)+1
# 
# current_datetime <- Sys.time()
# current_datetime
# datetime_splitted <- strsplit(as.character(current_datetime), split = ' ')
# 
# Id_generated <- gsub("-","",current_datetime)
# Id_generated <- gsub(" ","_",Id_generated)
# Id_generated <- gsub(":","",Id_generated)
# 
# df_log[new_pos,]$Id <- Id_generated
# df_log[new_pos,]$Date <- datetime_splitted[[1]][1]
# df_log[new_pos,]$Time <- datetime_splitted[[1]][2]
# 
# df_log[new_pos,]$pop <- sim_params[1,2]
# df_log[new_pos,]$iniY <- sim_params[2,2]
# df_log[new_pos,]$endY <- sim_params[3,2]
# df_log[new_pos,]$ini_c <- sim_params[4,2]
# df_log[new_pos,]$n0 <- sim_params[5,2]
# df_log[new_pos,]$nsim <- sim_params[6,2]
# df_log[new_pos,]$ne <- sim_params[7,2]
# df_log[new_pos,]$iter <- sim_params[8,2]
# df_log[new_pos,]$N <- sim_params[9,2]
# df_log[new_pos,]$alpha <- info_rank_1[2,2]
# df_log[new_pos,]$kappa <- info_rank_1[3,2]
# df_log[new_pos,]$g_shape <- info_rank_1[4,2]
# df_log[new_pos,]$g_rate <- info_rank_1[5,2]
# df_log[new_pos,]$mau <- info_rank_1[6,2]
# df_log[new_pos,]$nsp <- info_rank_1[7,2]
# 
# df_log[new_pos,]$fake_alpha <- info_fake[2,2]
# df_log[new_pos,]$fake_kappa <- info_fake[3,2]
# df_log[new_pos,]$fake_g_shape <- info_fake[4,2]
# df_log[new_pos,]$fake_g_rate <- info_fake[5,2]
# df_log[new_pos,]$fake_mau <- info_fake[6,2]
# df_log[new_pos,]$fake_nsp <- info_fake[7,2]
# 
#remove(df_log)
