get_middle_grid_point <- function(priors){

middle_grid_point <- (priors[1, ] + priors[2, ]) / 2
# multipliers <- priors[2, ] - locations

#lhs_sample <- as.data.frame(lhs::improvedLHS(n, ncol(priors)))

# mapped_sample <- mapply(function(x, multiplier, location) (x*multiplier) + location,
#                       lhs_sample, multipliers, locations)

# if(is.vector(mapped_sample)){
#   mapped_sample <- matrix(mapped_sample, ncol = length(mapped_sample))
#   }

final_sample <- data.frame(middle_grid_point)

colnames(final_sample) <- colnames(priors)

return(final_sample)

}

