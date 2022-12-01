comfert_natural <- function(seed_val, param, sdau = 0.1){
  
  setDTthreads(1)
  
  param_list <- colnames(param)
  for(i in param_list){
    if(length(param[, i]) > 0){
      assign(i, param[, i])
    }
  }
  
  ###########################################################
  # TIME                                                    #  
  ###########################################################
  ini <- as.POSIXct(paste0(iniY,"-01-01 00:00:00"), tz = "UTC")
  end <- as.POSIXct(paste0(endY,"-01-01 00:00:00"), tz = "UTC")
  time <- ini
  year <- year(time)
  
  ###########################################################
  # AUX FUN / VARS                                          #  
  ###########################################################
  pregnancy <- 270*86400L # 270 Days in seconds
  secs_m <- 2592000L
  secs_y <- 31536000L
  srb <- 0.515 # Sex ratio at birth
  
  ###########################################################
  # POP                                                     #  
  ###########################################################
  c_names <- c("wt_union",
               "wt_birth",
               "wt_update",
               "age",
               "tob",
               "phi",
               "union",
               "age_union",
               "kids",
               paste0("age_", 1:25))
  
  pop <- data.table(matrix(nrow = ini_c, ncol = length(c_names)))
  pop <- setNames(pop, c_names)
  pop[, names(pop) := lapply(.SD, as.numeric)]
  pop[, `:=` (age = 0L,
              age_union = NA,
              union = 0L,
              kids = 0L,
              wt_birth = Inf,
              wt_update = Inf
  )]
  wtb <- runif(ini_c)
  wt_ini_births <- wtb/max(wtb) * secs_y 
  pop[, tob := as.integer(time + wt_ini_births)] 
  wt_union_years <- rlnorm(ini_c, meanlog = log(mau),
                           sdlog = sdau) 
  pop[, wt_union := wt_union_years*secs_y] 
  pop[, phi := rgamma(ini_c, g_shape, g_rate)]
  
  ###################
  # Aux Vars        # 
  ###################
  events <- c("union", "birth", "update")
  wt_col <- paste0("wt_", events)
  event_count <- setNames(as.list(rep(0, length(events))), events)
  
  #****************************************************************************************************************************************
  #-------------------------------------------------------- RUNNING THE SIMULATION -------------------------------------------------------#
  #****************************************************************************************************************************************
  while (time < end){
    
    min_idx <- pop[, arrayInd(which.min(as.matrix(.SD)), .dim = dim(.SD)), .SDcols = wt_col]
    time_next_event <- pop[[min_idx[1], min_idx[2]]]
    next_event <- events[min_idx[2]]
    rid <- min_idx[1]
    
    #****************************************************************************************************************************************
    #--------- UPDATING ---------------------------------------------------------------------------------------------------------------------
    #****************************************************************************************************************************************
    # Clock.
    time <- time + time_next_event
    
    # Update Age of all agents.
    pop[, age := age + time_next_event]
    # Update WTs of all agents
    pop[, c(wt_col):= lapply(.SD, function(x){x-time_next_event}), .SDcols = wt_col]
    
    ########################
    ##       UNION        ##
    ########################
    if (next_event == "union") {
      
      pop[rid, `:=`(union = 1L, 
                    age_union = age,
                    wt_union = Inf)]
      
      wt_conception <- rexp(1, rate = pop[rid, phi] / (1 + exp(kappa*(pop[rid, age/secs_y] - alpha)))) * secs_m
      
      if(wt_conception < secs_y){ # conception within the year
        
        pop[rid, wt_birth := wt_conception + pregnancy]  
        
      }else{ # failed to conceive
        
        pop[rid, wt_update := secs_y]
        
      }
      
    } 
    
    ########################
    ##       BIRTH        ##
    ########################
    if (next_event == "birth") {
      
      # Change Indicators for mother
      if(!is.na(rid)){
        
        pop[rid, kids := kids + 1L]
        pop[rid, (paste0("age_", pop[rid, kids])) := age]
        pop[rid,`:=` (wt_birth = Inf,
                      wt_update = nsp*secs_m)]
        
      }
    }
    
    ########################
    ##       UPDATE       ##
    ########################
    if (next_event == "update") {
      
      wt_conception <- rexp(1, rate = pop[rid, phi] / (1 + exp(kappa*(pop[rid, age/secs_y] - alpha)))) * secs_m
      
      if(wt_conception < secs_y){ # conception within the year
        
        pop[rid, wt_birth := wt_conception + pregnancy] 
        pop[rid, wt_update := Inf]
        
      }else{ # failed to conceive
        
        pop[rid, wt_update := secs_y]
        
      }
      
    }
    
    #****************************************************************************************************************************************#
    #--------- NEW YEAR --------------------------------------------------------------------------------------------------------------------#
    #****************************************************************************************************************************************#
    
    if (year != year(time)){
      
      year <- year + 1
      print(time)
    }
  }
  
  ages <- grep('age', names(pop), value = T)
  pop[, (ages) := lapply(.SD, function(x) x/secs_y), .SDcols = ages]
  
  hst <- pop[, 7:ncol(pop)]
  hst[, id := 1:nrow(hst)]
  
  hst <- reshape(hst, 
                 direction = "long",
                 varying = paste0("age_", 1:25),
                 v.names = "age",
                 idvar = "id",
                 timevar = "parity",
                 times = 1:25)
  
  hst <- hst[!is.na(age),]
  hst <- hst[order(hst$id)]
  age <- 10:56
  hst <- merge(hst, as.data.frame(age), by = "age", all= T)
  # fecundidad acumulada a age E(x) 
  hst$cum_births <- cumsum(!is.na(hst$kids))
  hst$cum_fert <- hst$cum_births/max(hst$id, na.rm = T)
  lower <- hst[hst$age %in% seq(10,55,1),c("age", "cum_fert")]
  upper <- hst[hst$age %in% seq(11,56,1),c("age", "cum_fert")]
  fx <- upper$cum_fert - lower$cum_fert 
  fx <- as.data.frame(cbind(age = 10:55, fx))
  
  return(fx)
  
} 