rm(list = ls())
p <- 100

get_interaction_indices <- function(pairs, p) {
  interaction.ind <- t(combn(p, 2))
  results <- integer(nrow(pairs))
  for(i in 1:nrow(pairs)) {
    a <- pairs[i, 1]
    b <- pairs[i, 2]
    ind <- which(interaction.ind[,1] == a & interaction.ind[,2] == b)
    if(length(ind) == 0) {
      results[i] <- NA
    } else {
      results[i] <- ind + p
    }
  }
  return(results)
}

Model <- matrix(c(1,2,1,3,2,3,2,5,3,4,6,8,6,10,7,8,7,9,9,10), ncol = 2, byrow = TRUE)
Model
get_interaction_indices(Model, p)