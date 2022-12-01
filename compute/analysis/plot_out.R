plot_out <- function(res_dir, pop, obs = T,
                     sim_obs = T, save = F,
                     pch. = 19, cex. = 0.2,
                     alpha. = 0.7){
  
  # Nombre de colores = "firebrick", "darkorchid"
  # rgb()
  # col2rgb()
  
  obs_col <- rgb(131/255,75/255,159/255)
  sim_col <- rgb(193/255,78/255,78/255)
  
  obs_path <- file.path("..","data", pop)
  save_path <- file.path("..","..", "write", "plots")
  
  res_names <- sapply(res_dir, function(x) {list.files(x, "RData", full.names = TRUE)})
  
  save_plot <- function(name){
    p <- recordPlot()
    pdf(file.path(save_path, paste0(name,".pdf")), width=7, height=7) 
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
    
    plot(x_data, y_data, xlab=x_label_text, ylab= y_label_text,
         xlim = c(min(x_data),max(x_data)), ylim = c(min(y_data),max(y_data)),
         type = 'n', bty = "n", axes = F)
    
    # specify x-axis interval
    axis(side=1, at=seq(min(x_data), max(x_data), by = 5))

    draw_grid(x_coords, y_coords, grid_rows)
    
    par(new=TRUE)
  }

  plot_obs <- function(obs., save. = save){

    # change background color
    par(bg = "white")
    
    add_empty_plot_with_grid(obs.$age, obs.$fx, "Age", "f(x)")
    
    ticks <- seq(min(obs.$fx), max(obs.$fx), by = 0.1)
    axis(side=2, at=ticks)
    
    
    # Add points to plot
    points(obs.$age, obs.$fx, pch = pch., col = alpha(obs_col, alpha.),bty = "n")
    #points(obs.$age, obs.$fx, pch = 19, col = "#c14e4e",bty = "n")
    
    # Legend
    op <- par(family = "sans")
    legend(40, 0.5, legend = c("Observed"),
           lwd = c(1), col = alpha(obs_col, alpha.), lty = c(0),
           pch = c(16),
           cex=0.95, bty = "n",
           y.intersp = 0.8,
           x.intersp = 0.5)
    
    ## reset plotting parameters
    par(op)
    
    if(save.){
      save_plot("comfert_nat_obs")
    }
    # with mtext we can adjust the position of title and subtitle
    # see: https://stackoverflow.com/questions/55002046/adjust-plot-title-and-sub-title-in-base-r    
    
    # if(save.){
    #   save_plot("ComfertNatural_RBase_Obs")
    # }
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
    
    add_empty_plot_with_grid(x_data = sim[[1]]$age,
                             y_data =  sim[[1]]$fx,
                             x_label_text = "Age",
                             y_label_text =  "f(x)")
    
    # specify y-axis interval 
    #tick_labels <- c(rbind(seq(min(y_data), max(y_data), by = 0.1),""))
    ticks <- seq(min(sim[[1]]$fx), max(sim[[1]]$fx), by = 0.1)
    axis(side=2, at=ticks, labels = ticks)
    
    
    # NOTE:
    #       line type (lty) can be specified using either 
    #       text (“blank”, “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”) 
    #       or number (0, 1, 2, 3, 4, 5, 6). 
    #        Notice that lty = “solid” is identical to lty=1.
    
    # Add the first simulated data set as points to plot
    points(sim[[1]]$age, sim[[1]]$fx, type = "l", lty = 3, col = alpha(sim_col,1), lwd = 0.5)
    
    #col = "#c14e4e"
    # Add the second simulated data set as points to plot
    points(sim[[2]]$age,sim[[2]]$fx, type = "l", lty = 3, col = alpha(sim_col,1), pch = pch., cex = cex., lwd = 0.5)
    
    # Adding observed data as points to plot
    points(obs$age, obs$fx, col=alpha(obs_col,alpha.), pch = pch., lwd = 2)
    #834b9f = rgb(131,75,159,)
    
    # Adding mean of simulated data as points to plot
    points(mean_dat_sim$age, mean_dat_sim$fx, col=alpha(sim_col, alpha.), pch = pch., lwd = 2)
    #c14e4e = rgb(193,78,78)
    # Legend
    op <- par(family = "sans")
    legend(40, 0.5, legend = c("Simulated", "Observed", "Mean of simulations"),
           lwd = c(2,1,1), col = c(alpha(sim_col,1), alpha(obs_col, alpha.), alpha(sim_col,alpha.)), lty = c(3,0,0),
           pch = c(NA,16, 16),
           cex=0.95, bty = "n",
           y.intersp = 0.8,
           x.intersp = 0.5)
    
    ## reset plotting parameters
    par(op)
    
    if(save.){
      save_plot("comfert_nat_sim_obs")
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



