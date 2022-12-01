

compute_mse <-  function(sim, obs, weights){
  
  sse <- list()
  scale_01 <- function(x, max, min){return((x-min)/(max-min))}
  
  sim_ls <- lapply(sim, function(x) x[,2])
  
  
  all <- c(unlist(sim_ls),obs[[2]])
  scaled_sim <- lapply(sim_ls, scale_01, max(all, na.rm = T), min(all, na.rm = T))
  scaled_obs <- scale_01(obs[[2]], max(all, na.rm = T), min(all, na.rm = T))
  
  sse <- sapply(scaled_sim, function(x){sum((x - scaled_obs)^2) / length(x)})
  
  mse <- mean(sse)
  
  return(mse)
  
}



