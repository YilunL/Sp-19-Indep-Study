#############################################################################
# Upstream firms take it or leave it offers
#############################################################################

u_firm_unint <- function(w_A, w_B, firm_A){
  w_1A <- max(w_A[1],0)
  w_1B <- max(w_B[1],0)
  w_2A <- max(w_A[2],0)
  w_2B <- max(w_B[2],0)
  
  q = d_firm_unint_main(c(w_1A, w_2A), c(w_1B, w_2B))
  
  eq_pi$pi_A <<- w_1A * q$q_1A + w_2A * q$q_2A
  eq_pi$pi_B <<- w_1B * q$q_1B + w_2B * q$q_2B
  
  if (firm_A == 1){
    return(-eq_pi$pi_A)
  } else {
    return(-eq_pi$pi_B)
  }
}

u_firm_int <- function(w_A, w_B, firm_A){
  w_1A <- 0
  w_1B <- max(w_B[1],0)
  w_2A <- max(w_A[2],0)
  w_2B <- max(w_B[2],0)
  
  q = d_firm_int_main(c(w_1A, w_2A), c(w_1B, w_2B))
  
  eq_pi$pi_A <<- w_1A * q$q_1A + w_2A * q$q_2A
  eq_pi$pi_B <<- w_1B * q$q_1B + w_2B * q$q_2B
  
  if (firm_A == 1){
    return(-eq_pi$pi_A - eq_pi$pi_1)
  } else {
    return(-eq_pi$pi_B)
  }
}


#################################################### FORECLOSED ##############################################

