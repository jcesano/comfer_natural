library(data.table); library(lubridate);
library(mlegp); library(parallel); library(scales)

library("rstudioapi") # This allows to automatically set working directory to source opened file location
setwd(dirname(getActiveDocumentContext()$path))


start <- Sys.time()

source(file.path("..","estimation","get_new_points.R"))
source(file.path("..","estimation","parallel_comfert.R"))
source(file.path("..","estimation","optimize_comfert.R"))
source(file.path("..","estimation","save_res.R"))
source(file.path("..","estimation","log.R"))

pop <- "HT"  # pop for which simulations are performed 
iniY <- 1925 # years for which the simulations are performed
endY <- 1976 # years for which the simulations are performed
ini_c <- 2000 # size of the initial birth cohorts of the model 
n0 <- 40     # size of initial sample of param combinations
nsim <- 2    # nr of simulations in each evaluated point - this will produce a cluster of size n0*nsim
ne <- 30     # nr of new evaluations at each iteration of the bayes opt. algorithm
iter <- 10
N <- ne*iter     # total nr of evaluations = n0+N

id_datetime_result <- generate_unique_id()

# Path to files 
global_path <- file.path(id_datetime_result$Id, "results", paste(pop),
                         paste0("n_sim_", nsim),
                         paste0("ini_c_", ini_c),
                         paste0("ne_", ne),
                         paste0("N_", N))
# results' path  
res_path <- file.path(global_path,"results")

# Priors
priors <- data.frame(alpha = c(34,37),     # inflection point decline of fecundability
                     kappa = c(0.29,0.45), # rate decline of fec. 
                     g_shape = c(4,6),     # gamma shape - heterogeneity fecundability
                     g_rate = c(35,40),    # gamma rate - heterogeneity fecundability
                     mau = c(17,18.5),       # mean age at union
                     nsp = c(5.5,10))         # non susceptibility period

params <- get_new_points(priors, n=n0) # Initial sample of parameter combinations  

# Compute model at initial parameter set 
output <- parallel_comfert(params = params[rep(seq_len(nrow(params)), each = nsim),],
                           pop = pop,
                           ini_c = ini_c,
                           iniY = iniY,
                           endY = endY)

# Save Results
save_res(results = output, pars = params, res_path, seq = 1:n0, delete = TRUE)

dir.create(file.path(global_path, "post"), recursive = T)
dir.create(file.path(global_path, "params"), recursive = T)

# Bayesian optimization
optimize_comfert(res_path, global_path, pop, iniY, endY,
                 ini_c, n0, nsim, N, ne, params, priors) 

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

# comparar opt_res_dir con el info.txt de fake_obs
info_fake <- read.table(file.path(fake_res_path,"param_set_1","Info.txt"),sep=":",dec=".")
info_rank_1 <- read.table(file.path(opt_res_dir,"Info.txt"),sep=":",dec=".")

# create a dataframe with the hyperparameters used to execute the model
sim_params <- create_sim_params_data_frame(pop, iniY, endY, ini_c, n0, nsim, ne, iter, N)

save_log(id_datetime_result, sim_params, info_rank_1, data.frame(), fake_obs = FALSE)

# 
plot_out(opt_res_dir, pop, save = F)

# keep 10%  of values with lowest distance 
epsilon <- quantile(post$mse,probs = seq(0, 1, 0.10))[2]
accepted <- post[post$mse < epsilon,] 

# Aprox Posterior
hist(accepted$alpha)
hist(accepted$kappa)
hist(accepted$g_shape)
hist(accepted$g_rate)
hist(accepted$mau)
hist(accepted$nsp)

