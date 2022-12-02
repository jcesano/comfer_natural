library("rstudioapi") # This allows to automatically set working directory to source opened file location
setwd(dirname(getActiveDocumentContext()$path))

library(data.table); library(lubridate);
library(mlegp); library(parallel); library(scales)
library(lhs)

#This source select the middle point of the grid. 
source (file.path("..","estimation","get_middle_grid_point.R"))
source(file.path("..","estimation","get_new_points.R"))
source(file.path("..","estimation","get_fake_obs.R"))
source(file.path("..","estimation","parallel_comfert.R"))
source(file.path("..","estimation","optimize_comfert.R"))
source(file.path("..","estimation","save_res.R"))
#source(file.path("..","estimation","save_fake_res.R")) This file is not used anymore
source(file.path("..","estimation","generate_mean_fake_res.R"))
source(file.path("..","estimation","log.R"))

start <- Sys.time()

# Model parameters
# pop <- "HT"  # pop for which simulations are performed 
# iniY <- 1925 # years for which the simulations are performed
# endY <- 1976 # years for which the simulations are performed
 # ini_c <- 500 # size of the initial birth cohorts of the model 
# n0 <- 20     # size of initial sample of param combinations
# nsim <- 2    # nr of simulations in each evaluated point - this will produce a cluster of size n0*nsim
# ne <- 10     # nr of new evaluations at each iteration of the bayes opt. algorithm
# N <- 30     # total nr of evaluations = n0+N

# Model parameters #
pop <- "HT"  # pop for which simulations are performed 
iniY <- 1925 # years for which the simulations are performed
endY <- 1976 # years for which the simulations are performed
ini_c <- 2000 # size of the initial birth cohorts of the model 
n0 <- 40     # size of initial sample of param combinations
nsim <- 2    # nr of simulations in each evaluated point - this will produce a cluster of size n0*nsim
ne <- 40     # nr of new evaluations at each iteration of the bayes opt. algorithm
iter <- 20
N <- ne*iter     # total nr of evaluations = n0+N

# create a dataframe with the hyperparameters used to execute the model
sim_params <- create_sim_params_data_frame(pop, iniY, endY, ini_c, n0, nsim, ne, iter, N)

id_datetime_result <- generate_unique_id()

# Path to files 
fake_global_path <- file.path("results", id_datetime_result$Id,"fake_results", paste(pop),
                              paste0("n_sim_", nsim),
                              paste0("ini_c_", ini_c),
                              paste0("ne_", ne),
                              paste0("N_", N))

# fake results' path
fake_res_path <- file.path(fake_global_path,"fake_results")

# Priors    
priors <- data.frame(alpha = c(34,37),     # inflection point decline of fecundability
                     kappa = c(0.29,0.45), # rate decline of fec. 
                     g_shape = c(4,6),     # gamma shape - heterogeneity fecundability
                     g_rate = c(35,40),    # gamma rate - heterogeneity fecundability
                     mau = c(17,18.5),       # mean age at union
                     nsp = c(5.5,10))         # non susceptibility period

fake_params <- get_middle_grid_point(priors) # Initial sample of parameter combinations  

# Compute model at initial parameter set 
output <- parallel_comfert(params = fake_params[rep(seq_len(nrow(fake_params)), each = nsim),],
                              pop = pop,
                              ini_c = ini_c,
                              iniY = iniY,
                              endY = endY)

# respetar el formato de asfrs.csv lo guardo en el mismo path. fake_asfrs.csv
# cambiar el path para que busque el fake_asfrs.csv get_fake_obs()
save_res(results = output, pars = fake_params, fake_res_path, seq=1:1, nsim, delete = TRUE)

# saving fake obs in a csv file. The name of the generated file is fake_asfrs.csv. 
generate_mean_fake_res()

# Path to files 
global_path <- file.path("results", id_datetime_result$Id, paste(pop),
                         paste0("n_sim_", nsim),
                         paste0("ini_c_", ini_c),
                         paste0("ne_", ne),
                         paste0("N_", N))

# results' path  
res_path <- file.path(global_path,"results")

params <- get_new_points(priors, n0) # Initial sample of parameter combinations  

# Compute model at initial parameter set 
output <- parallel_comfert(params = params[rep(seq_len(nrow(params)), each = nsim),],
                           pop = pop,
                           ini_c = ini_c,
                           iniY = iniY,
                           endY = endY)

# Save Results
#save_res(results = output, pars = params, n0, nsim, delete = TRUE)
save_res(results = output, pars = params, res_path, seq = 1:n0, nsim, delete = TRUE)

dir.create(file.path(global_path, "post"), recursive = T)
dir.create(file.path(global_path, "params"), recursive = T)

# Bayesian optimization
optimize_comfert(res_path, global_path, pop, iniY, endY,
                 ini_c, n0, nsim, N, ne, params, priors, fake_obs = TRUE) 

end <- Sys.time()
print(end-start)

# ANALYSIS
source(file.path("..","analysis","check_paramset.R"))
source(file.path("..","analysis","plot_out.R"))
source(file.path("..","estimation","get_obs.R"))

# Get posterior distribution
post <- readRDS(file.path(global_path, "post", "posterior.rds"))
post[order(post$mse),]
opt_res_dir <- check_paramset(global_path, rank = 1)

# compare opt_res_dir with info.txt from fake_obs
info_fake <- read.table(file.path(fake_res_path,"param_set_1","Info.txt"),sep=":",dec=".")
info_rank_1 <- read.table(file.path(opt_res_dir,"Info.txt"),sep=":",dec=".")

# create a dataframe with the hyperparameters used to execute the model
sim_params <- create_sim_params_data_frame(pop, iniY, endY, ini_c, n0, nsim, ne, iter, N)

save_log(id_datetime_result, sim_params, info_rank_1, info_fake, fake_obs = TRUE)

plot_out(opt_res_dir, pop, save = F)

# keep 10%  of values with lowest distance 
epsilon <- quantile(post$mse,probs = seq(0, 1, 0.10))[2]
accepted <- post[post$mse < epsilon,] 

# Aprox Posterior
hist(accepted$alpha)
abline(v = info_fake[2,]$V2, col="red", lwd=3, lty=2)

hist(accepted$kappa)
abline(v = info_fake[3,]$V2, col="red", lwd=3, lty=2)

hist(accepted$g_shape)
abline(v = info_fake[4,]$V2, col="red", lwd=3, lty=2)

hist(accepted$g_rate)
abline(v = info_fake[5,]$V2, col="red", lwd=3, lty=2)

hist(accepted$mau)
abline(v = info_fake[6,]$V2, col="red", lwd=3, lty=2)

hist(accepted$nsp)
abline(v = info_fake[7,]$V2, col="red", lwd=3, lty=2)
