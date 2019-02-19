# Linear production


#############################################################################
# Unintegrated downstream firms
#############################################################################
# d_firm_price <- function(w_A, w_B) {
#   w_1A <- w_A[1]
#   w_1B <- w_B[1]
#   w_2A <- w_A[2]
#   w_2B <- w_B[2]
#   
#   p_1 <- list(p_1A = (1 + w_1A)/(2 - k), p_1B = (1 + w_1B)/(2 - k))
#   p_2 <- list(p_2A = (1 + w_2A)/(2 - k), p_2B = (1 + w_2B)/(2 - k)) 
#   
#   return(c(p_1, p_2))
# }    

d_firm_main <- 

d_firm_prob <- function(p, x){
  
  odds <- cons_demand(p, x)
  
  return(odds)
}

d_firm_1 <- function(w, p, x, odds) {
  w_A <- w[1]
  w_B <- w[2]
  p_A <- p[1]
  p_B <- p[2]
  x_A <- x[1]
  x_B <- x[2]
  
  odds_1 <- odds[1]
  odds_2 <- odds[2]
  
  prob <- odds_1 / (1 + odds_1 + odds_2)
  pi <- prob * (p_A + p_B) - (w_A*x_A + w_B*x_B)
  
  return(-pi)
}

d_firm_2 <- function(w, p, x, odds) {
  w_A <- w[1]
  w_B <- w[2]
  p_A <- p[1]
  p_B <- p[2]
  x_A <- x[1]
  x_B <- x[2]
  odds_1 <- odds[1]
  odds_2 <- odds[2]
  
  prob <- odds_2 / (1 + odds_1 + odds_2)
  pi <- prob * (p_A + p_B) - (w_A*x_A + w_B*x_B)
  
  return(-pi)
}

d_firm_1(c(0.4, 0.4), c(0.6, 0.6), c(0.4, 0.4), c(1.2, 1.2))
