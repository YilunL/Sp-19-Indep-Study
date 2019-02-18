# Linear production


#############################################################################
# Unintegrated downstream firms
#############################################################################
d_firm_price <- function(w_A, w_B) {
  w_1A <- w_A[1]
  w_1B <- w_B[1]
  w_2A <- w_A[2]
  w_2B <- w_B[2]
  
  p_1 <- list(p_1A = (1 + w_1A)/(2 - k), p_1B = (1 + w_1B)/(2 - k))
  p_2 <- list(p_2A = (1 + w_2A)/(2 - k), p_2B = (1 + w_2B)/(2 - k)) 
  
  return(c(p_1, p_2))
}    


d_firm_1 <- function(w){
  w_A <- w[1]
  w_B <- w[2]
  

  return(0)
}