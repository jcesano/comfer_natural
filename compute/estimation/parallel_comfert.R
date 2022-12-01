
parallel_comfert <- function(params, pop, ini_c, iniY, endY, scenario = F) {

  path_to <- function(file){
    file.path("..","data",pop,"in", file)}
  
  o_file <- file.path("..","out_files",
                      paste0("parallel_comfert_",
                      pop,"_", Sys.Date(),".txt"))
  
  if (file.exists(o_file)){file.remove(o_file)} 
  
  cl <- makeCluster(nrow(params), type = "PSOCK", outfile = paste0("pc_",unname(Sys.info()[4]),".txt"))
  
  clusterEvalQ(cl, library(lubridate))
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(truncdist))
  
  clusterExport(cl,"pop")
  clusterExport(cl,"path_to", envir = environment())
  clusterExport(cl,"ini_c", envir = environment())
  clusterExport(cl,"params", envir = environment())
  clusterExport(cl,"iniY", envir = environment())
  clusterExport(cl,"endY", envir = environment())

  clusterCall(cl, function() {
    source(file.path("..","run","comfert_natural.R")) # load model
  })
  
  s <- Sys.time()

  output <- parLapply(cl, 1:nrow(params), function (x) comfert_natural(seed_val = x, params[x,]))
  e <- Sys.time()
  print(e-s)
  
  cat("finished parallel processes \n")
  stopCluster(cl)

  return(output)
}