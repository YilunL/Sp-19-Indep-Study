# Linear production


#############################################################################
# Unintegrated downstream firms
#############################################################################

d_firm_main <- function(p_1, p_2, w, firm_1){
  # offer by firm A to downstream firm 1
  p_1A <- p_1[1]
  
  # offer by firm B to downstream firm 1
  p_1B <- p_1[2]
  
  # offer by firm A to downstream firm 2
  p_2A <- p_2[1]
  
  # offer by firm B to downstream firm 2
  p_2B <- p_2[2]
  
  # create price vector
  p <- c(p_1A, p_1B, p_2A, p_2B)
  
  # get quantities 
  x = cons_demand_2(p)
  
  # profits
  # w is (w_1A, w_2A, w_1B, w_2B)
  # x is (x_1A, x_1B, x_2A, x_2B)
  pi_1 <- x[1] * (p_1A - w[1]) + x[2] * (p_1B - w[3])
  pi_2 <- x[3] * (p_2A - w[2]) + x[4] * (p_2B - w[4])
  
  eq_pi$pi_1 <<- pi_1
  eq_pi$pi_2 <<- pi_2
  eq_int_good$x_1A <<- x[1]
  eq_int_good$x_1B <<- x[2]
  eq_int_good$x_2A <<- x[3]
  eq_int_good$x_2B <<- x[4]
  
  
  if(firm_1 == 1){
    if (integrated == 0){
      return(-pi_1)
    } else if (integrated == 1) {
      return(-pi_1 - (w[2] * x[3]))
    }
  } else {
    return(-pi_2)
  }
}
