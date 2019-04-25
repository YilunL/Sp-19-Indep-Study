#############################################################################
# Unintegrated downstream firms
#############################################################################

objective_d <- function(p, w){
  # w is (w_1A, w_2A, w_1B, w_2B)
  # x is (x_1A, x_1B, x_2A, x_2B)
  
  penalty = 0
  
  q = cons_demand_2(p) #q_1A, q_1B, q_2A, q_2B
  
  jac = jacobian(func = cons_demand_2, x = p)
  dq_1A = jac[1, 1:4]
  dq_1B = jac[2, 1:4]
  dq_2A = jac[3, 1:4]
  dq_2B = jac[4, 1:4]

  
  # Firm 1 FOC (need to add integrated FOC)
  dpi_1_dp_1A = (q[1] + (p[1] - w[1]) * dq_1A[1] 
                 + (p[2] - w[3])*dq_1B[1])
  
  dpi_1_dp_1B = (q[2] + (p[2] - w[3]) * dq_1B[2] 
                 + (p[1] - w[1])*dq_1A[2])
  
  # Firm 2 FOC
  dpi_2_dp_2A = (q[3] + (p[3] - w[2]) * dq_2A[3] 
                 + (p[4] - w[4])*dq_2B[3])
  
  dpi_2_dp_2B = (q[4] + (p[4] - w[4]) * dq_2B[4] 
                 + (p[3] - w[2])*dq_2A[4])
  
  penalty = sqrt(dpi_1_dp_1A^2 + dpi_1_dp_1B^2 + dpi_2_dp_2A^2 + dpi_2_dp_2B^2)
  
  return(penalty)
}

d_main <- function(w){
  # w is (w_1A, w_2A, w_1B, w_2B)
  # x is (x_1A, x_1B, x_2A, x_2B)
  
  p_1A = 2  # prices
  p_1B = 2
  p_2A = 2
  p_2B = 2
  
  p = c(p_1A, p_1B, p_2A, p_2B)
  
  res = optim(
    par = p,
    fn = objective_d,
    w = w,
    method = "BFGS",
    control = list(maxit = 10000)
    )
  
  x <- cons_demand_2(res$par)
  out_q <<- x
  out_p <<- res$par
  
  if (integrated == 0){
    pi_1 <- x[1] * (p[1] - w[1]) + x[2] * (p[2] - w[3])
  }

  if (integrated == 1){
    pi_1 <- x[1] * (p[1] - w[1]) + x[2] * (p[2] - w[3]) + w[2] * x[3]
  }
  
  pi_2 <- x[3] * (p[3] - w[2]) + x[4] * (p[4] - w[4])

  return(c(out_p, pi_1, pi_2))
}

