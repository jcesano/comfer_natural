plot_out <- function(res_dir, pop, obs = T, sim_obs = T, save = F, pch. = 19, cex. = 0.2, alpha. = 1){
  
  # Nombre de colores = "firebrick", "darkorchid"
  # rgb()
  # col2rgb()
  
  obs_path <- file.path("..","data", pop)
  save_path <- file.path("..","..", "write", "plots")
  
  res_names <- sapply(res_dir, function(x) {list.files(x, "RData", full.names = TRUE)})
  
  save_plot <- function(name){
    p <- recordPlot()
    pdf(paste0(save_path, name,".pdf"), width=6, height=6) 
    print(p)
    dev.off()
  }

  add_empty_plot_with_grid <- function(x_data, y_data, x_label_text, y_label_text, grid_rows = 25){
    
    draw_grid <-function(x_coords, y_coords, grid_rows){
      
      for(i in 1:grid_rows){
        
        # we draw the i_th vertical line
        segments(x0 = x_coords[i], x1 = x_coords[i],
                 y0 = min(y_coords), y1 = max(y_coords), col=alpha(rgb(0,0,0), 0.1))
        
        # we draw the i_th horizontal line
        segments(x0 = min(x_coords), x1 = max(x_coords),
                 y0 = y_coords[i], y1 = y_coords[i], col=alpha(rgb(0,0,0), 0.1))
      }
    }
    
    x_coords <- seq(min(x_data), max(x_data), length.out = grid_rows)
    y_coords <- seq(min(y_data), max(y_data), length.out = grid_rows)
    
    plot(x_data, y_data, xlab=x_label_text, ylab= y_label_text, xlim = c(min(x_data),max(x_data)), ylim = c(min(y_data),max(y_data)), type = 'n', bty = "n")
    
    # specifty x-axis interval
    axis(side=1, at=seq(min(x_data), max(x_data), by = 5))
    
    #specify y-axis interval if it is neccesary
    
    draw_grid(x_coords, y_coords, grid_rows)
    
    par(new=TRUE)
  }

  plot_obs <- function(obs., save. = save){
    
    # change background color
    par(bg = "white")
    
    add_empty_plot_with_grid(obs.$age, obs.$fx, "Age", "f(x)")
    
    # Add points to plot
    points(obs.$age, obs.$fx, pch = 19, col = "#c14e4e",bty = "n")
    
    # with mtext we can adjust the position of title and subtitle
    # see: https://stackoverflow.com/questions/55002046/adjust-plot-title-and-sub-title-in-base-r    
    
    if(save.){
      save_plot("ComfertNatural_RBase_Obs")
    }
  }
  
  plot_sim_obs <- function(sim, obs, save. = save){
    
    # title(xlab = "age", ylab = "fx",main = "Comfert Natural")
    
    # calculate the mean of simulated data sets
    mean_dat_sim <- as.data.frame(Reduce("+", sim) / length(sim))
    
    # change background color
    par(bg = "white")
    
    # create the scatter plot the first simulated data set
    # plot(sim[[1]]$age,sim[[1]]$fx, frame=FALSE,pch = 19, col="#c14e4e",
    #      xlab = "age", ylab = "fx", cex=0.2)
    
    add_empty_plot_with_grid(sim[[1]]$age, sim[[1]]$fx, "Age", "f(x)")
    
    # NOTE:
    #       line type (lty) can be specified using either 
    #       text (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”) 
    #       or number (0, 1, 2, 3, 4, 5, 6). 
    #        Notice that lty = “solid” is identical to lty=1.
    
    # Add the first simulated data set as points to plot
    points(sim[[1]]$age, sim[[1]]$fx, type = "l", lty = 3, col = "#c14e4e")
    
    #col = "#c14e4e"
    # Add the second simulated data set as points to plot
    points(sim[[2]]$age,sim[[2]]$fx, type = "l", lty = 3, col = "#c14e4e", pch = 19, cex = 0.2)
    
    # Adding observed data as points to plot
    points(obs$age, obs$fx, col="#834b9f", pch = 19)
    
    # Adding mean of simulated data as points to plot
    points(mean_dat_sim$age, mean_dat_sim$fx, col="#c14e4e", pch = 19)
    
    # add grid
    #grid(19,19,col = "lightgrey", lty = "solid")
    
    # Legend
    op <- par(family = "serif")
    legend(40, 0.5, legend = c("Simulated", "Observed", "Mean of simulations"),
           lwd = c(3,1,1), col = c("#c14e4e", "#834b9f", "#c14e4e"), lty = c(3,0,0),
           pch = c(NA,16, 16),
           cex=0.95, bty = "n",
           y.intersp = 0.8,
           x.intersp = 0.5)
    
    if(save.){
      save_plot("ComfertNatural_RBase_Sim")
    }
  }
  
  if(obs){
    obs <- read.csv(file.path(obs_path, "asfrs.csv"), header = T) 
    plot_obs(obs)
  }
  
  if(sim_obs){
    obs <- read.csv(file.path(obs_path, "asfrs.csv"), header = T) 
    sim <- lapply(res_names, readRDS)
    plot_sim_obs(sim, obs)
  }
}

