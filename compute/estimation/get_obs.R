get_obs <- function(pop){
  
  dir_o <- file.path("..","data", pop)
  # Observed ASFR
  obs_asfrs <-  read.csv(file.path(dir_o, "asfrs.csv"), header = T) 

  return(obs_asfrs)  
}  



