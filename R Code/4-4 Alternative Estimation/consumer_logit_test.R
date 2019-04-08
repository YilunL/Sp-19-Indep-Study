#############################################################################
# Consumer logit demand
#############################################################################

cons_demand_2 <- function(p, store){
  beta_0 = 10  # intercept
  beta_1 = 1  # price effect
  beta_2 = 0  # store effect for store 2
  beta_3 = -3  # brand effect for item B
  
  p_A <- p[1]
  p_B <- p[2]
  
  odds_A <- exp(beta_0 - p_A * beta_1 + store * beta_2)
  odds_B <- exp(beta_0 - p_B * beta_1 + store * beta_2 + beta_3)
  
  return(c(odds_A, odds_B))
}


