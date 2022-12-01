get_fake_obs <- function(pop){
  
  dir_o <- file.path("..","data", pop)
  # Fake Observed ASFR
  fake_obs_asfrs <-  read.csv(file.path(dir_o, "fake_asfrs.csv"), header = T) 

  return(fake_obs_asfrs)  
}  



