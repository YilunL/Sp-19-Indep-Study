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

cons_demand <- function(p){
  beta_0 = 1 # intercept
  beta_1 = 1  # price effect
  beta_2 = 0  # store effect for store 2
  beta_3 = 0  # brand effect for item B
  
  p_1A <- p[1]
  p_1B <- p[2]
  p_2A <- p[3]
  p_2B <- p[4]
  
  odds_1A <- exp(beta_0 - p_1A * beta_1)
  odds_1B <- exp(beta_0 - p_1B * beta_1 + beta_3)
  odds_2A <- exp(beta_0 - p_2A * beta_1 + beta_2)
  odds_2B <- exp(beta_0 - p_2B * beta_1 + beta_2 + beta_3)
  
  x_1A <- odds_1A / (1 + odds_1A + odds_1B + odds_2A + odds_2B)
  x_1B <- odds_1B / (1 + odds_1A + odds_1B + odds_2A + odds_2B)
  x_2A <- odds_2A / (1 + odds_1A + odds_1B + odds_2A + odds_2B)
  x_2B <- odds_2B / (1 + odds_1A + odds_1B + odds_2A + odds_2B)
  
  return(c(x_1A, x_1B, x_2A, x_2B))
}


