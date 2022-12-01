
# get simulated rates
get_sim <- function(res_dir, nsim, obs){
  
  get_sim_results <- function(res_dir, obs){ 
    
    res_names <- list.files(res_dir, "RData", full.names = TRUE)
    
      obs_rnames <- rownames(obs)
      obs_cnames <- colnames(obs)

    # asfr
    sim_asfr <- lapply(res_names, function(x) readRDS(x))
    
    sim_asfr <- lapply(sim_asfr,
                       function(x){x[rownames(x) %in% obs_rnames,
                                     colnames(x) %in% obs_cnames]})
    # results
    return(sim_asfr)
  }
  
  if (length(res_dir)>1){
    nsim_sim_results <- lapply(res_dir, get_sim_results, obs)
  }else{
    nsim_sim_results <- get_sim_results(res_dir, obs)
  }
  
  return(nsim_sim_results)
  
}
