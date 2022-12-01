generate_mean_fake_res <-function(){
  
  fake_res_dir <- file.path(fake_res_path,"param_set_1")
  fake_res_names <- sapply(fake_res_dir, function(x) {list.files(x, "RData", full.names = TRUE)})
  sim <- lapply(fake_res_names, readRDS)
  
  # calculate the mean of simulated data sets
  mean_dat_sim <- as.data.frame(Reduce("+", sim) / length(sim))
  
  dir_o <- file.path("..","data", pop)
  
  write.csv(mean_dat_sim,file.path(dir_o,"fake_asfrs.csv"), row.names = FALSE)
}
