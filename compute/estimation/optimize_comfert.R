optimize_comfert <- function(res_dir, global_dir, pop, iniY, endY, ini_c, n0, nsim,
                             N, ne, params, priors, fake_obs = FALSE){
  
  #browser()
  
  # OS
  switch(Sys.info()[['sysname']],
         Windows= {sa <- "NUL"},
         Linux  = {sa <- "/dev/null"},
         Darwin = {sa <- "/dev/null"})
  
  source(file.path("..","estimation","get_obs.R"))
  source(file.path("..","estimation","get_sim.R"))
  source(file.path("..","estimation","get_new_points.R"))
  source(file.path("..","estimation","parallel_comfert.R"))
  source(file.path("..","estimation","compute_mse.R"))
  source(file.path("..","estimation","save_res.R"))
  
  
  # estimate gp, eval new points and repeat  
  n <- n0
  all_mse <- as.data.frame(matrix(ncol = ncol(params)+1))
  names(all_mse) <- c(names(params), "mse")
  
  if(fake_obs == FALSE){
    obs <- get_obs(pop) # observed data  
  }
  else{
    obs <- get_fake_obs(pop) # fake observed data  
  }
  
  while (n < (n0+N)) {
    
    # main dir
    param_dir <- dir(res_dir, pattern = "param_set", full.names = T)
    
    # reorder results in numerical order (1-nrpops)
    order_res <- sapply(X = strsplit(param_dir, split = "param_set_"), 
                        FUN = function(x){rev(x)[1]})
    res_dirs <- param_dir[match(1:length(param_dir), order_res)]
    
    sim <- get_sim(res_dirs, nsim, obs) # simulated data
    
    # compute mse
    em_data <- params
    em_data$mse <- sapply(sim, compute_mse, obs, weights)  
    
    # training gp
    cat("\n training gp...\n")
    gpe <- mlegp::mlegp(em_data[!is.na(em_data$mse),!names(em_data) == "mse"], 
                        em_data[!is.na(em_data$mse), "mse"], 
                        nugget = T, verbose = 0)
    
    gp <- list(params, gpe)
    
    new_set <- get_new_points(priors, n = n0)
    
    pred <- predict.gp(gp[[2]], newData = new_set[!names(new_set) %in% "modName"], se.fit = T)
    
    mu <- pred$fit
    sigma <- pred$se.fit
    
    kappa_af <- 2
    
    lcb <- mu - kappa_af * sigma
    
    if(nrow(new_set[which(lcb %in% sort(lcb)[1:ne]),]) > ne){
      cat("\n warning: multiple identical predictions")
      cat("\n printing prediction object: \n")
      
      print(lcb)
    }
    
    new_eval <- new_set[which(lcb %in% sort(lcb)[1:ne]),][1:ne,]
    
    # compute the model at new points
    cat("\n computing new evaluations...\n")
    output <- parallel_comfert(params = new_eval[rep(1:nrow(new_eval), each = nsim),],
                               pop,
                               ini_c,
                               iniY,
                               endY)

    sink(sa)
    
    # Save Results
    save_res(results = output, pars = new_eval, res_dir, seq = (n+1):(n+ne), nsim)
    
    sink()
    
    res_dirs_ne <- lapply((n+1):(n+ne), function(x) file.path(paste0(res_dir, "/param_set_", x)))
    
    # simulated rates
    sim <- get_sim(res_dirs_ne, nsim, obs)
    
    new_eval_mse <- as.data.table(new_eval)
    new_eval_mse[, mse := sapply(sim, compute_mse, obs)]  
    
    all_mse <- rbind(all_mse, new_eval_mse)
    
    params <- rbind(params, new_eval)
    
    n <- n+ne
    print(n)
    
  }
  
  sink(sa)
  #browser() 
  saveRDS(all_mse[-1,], paste0(paste0(global_dir,"/post/posterior.rds")))
  saveRDS(params, paste0(paste0(global_dir,"/params/parameters.rds")))
  sink()
}
