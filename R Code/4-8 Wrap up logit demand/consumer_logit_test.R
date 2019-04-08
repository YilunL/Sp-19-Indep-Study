#############################################################################
# Consumer logit demand
#############################################################################

# cons_demand <- function(p, x, store, brand){
#   beta_0 = 2/3 
#   beta_1 = c(1, 1)
#   beta_2 = c(0,0)
#   beta_3 = c(0,0)
#   alpha = c(0.5,0.5)
#   
#   odds <- exp(beta_0 + t(x) %*% beta_1 - t(p) %*% alpha + store %*% beta_2 + brand %*% beta_3)
#   return(odds)
# }

cons_demand_2 <- function(p, store){
  beta_0 = 10 # intercept
  beta_1 = 1  # price effect
  beta_2 = 0  # store effect for store 2
  beta_3 = -3  # brand effect for item B
  
  p_A <- p[1]
  p_B <- p[2]
  
  odds_A <- exp(beta_0 - p_A * beta_1 + store * beta_2)
  odds_B <- exp(beta_0 - p_B * beta_1 + store * beta_2 + beta_3)
  
  return(c(odds_A, odds_B))
}


