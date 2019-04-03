#############################################################################
# Unintegrated downstream firms
#############################################################################

quantities <- function(p_1, p_2){
  odds_1 = cons_demand_2(p_1, 0)  # store 1
  odds_2 = cons_demand_2(p_2, 1)  # store 2
  q_1A = odds_1[1]/(1 + sum(c(odds_1, odds_2)))
  q_1B = odds_1[2]/(1 + sum(c(odds_1, odds_2)))
  q_2A = odds_2[1]/(1 + sum(c(odds_1, odds_2)))
  q_2B = odds_2[2]/(1 + sum(c(odds_1, odds_2)))
  return(c(q_1A, q_1B, q_2A, q_2B))
}

quantities_good <- function(p, good){
  p_1 = c(p[1], p[2])
  p_2 = c(p[3], p[4])
  odds_1 = cons_demand_2(p_1, 0)  # store 1
  odds_2 = cons_demand_2(p_2, 1)  # store 2
  if (good == "1A"){
    return(odds_1[1]/(1 + sum(c(odds_1, odds_2))))
  } 
  if (good == "1B"){
    return(odds_1[2]/(1 + sum(c(odds_1, odds_2))))
  } 
  if (good == "2A"){
    return(odds_2[1]/(1 + sum(c(odds_1, odds_2))))
  } 
  if (good == "2B"){
    return(odds_2[2]/(1 + sum(c(odds_1, odds_2))))
  }
}

objective <- function(p, w){
  penalty = 0
  
  p_1 = c(p[1], p[2])
  p_2 = c(p[3], p[4])
  
  q = quantities(p_1, p_2) #q_1A, q_1B, q_2A, q_2B
  
  dq_dp_1A = grad(func = quantities_good, x = p, good = "1A")
  dq_dp_1B = grad(func = quantities_good, x = p, good = "1B")
  dq_dp_2A = grad(func = quantities_good, x = p, good = "2A")
  dq_dp_2B = grad(func = quantities_good, x = p, good = "2B")
  
  
  # Firm 1 FOC
  dpi_1_dp_1A = (q[1] + (p[1] - w[1]) * dq_dp_1A[1] 
                 + (p[2] - w[2])*dq_dp_1A[2])
  
  dpi_1_dp_1B = (q[2] + (p[2] - w[2]) * dq_dp_1B[2] 
                 + (p[1] - w[1])*dq_dp_1B[1])
  
  # Firm 2 FOC
  dpi_2_dp_2A = (q[3] + (p[3] - w[3]) * dq_dp_2A[3] 
                 + (p[4] - w[4])*dq_dp_2A[4])
  
  dpi_2_dp_2B = (q[4] + (p[4] - w[4]) * dq_dp_2B[4] 
                 + (p[3] - w[3])*dq_dp_2B[3])
  
  penalty = sqrt(dpi_1_dp_1A^2 + dpi_1_dp_1B^2 + dpi_2_dp_2A^2 + dpi_2_dp_2B^2)
  
  return(penalty)
}

d_main <- function(w){
  
  p_1A = 1  # prices
  p_1B = 1
  p_2A = 1
  p_2B = 1
  
  p = c(p_1A, p_1B, p_2A, p_2B)
  
  res = optim(
    par = p,
    fn = objective,
    w = w,
    method = "BFGS",
    control = list(maxit = 10000, abstol = 1E-10)
    )
  
  out_q <- quantities(res$par[1:2], res$par[3:4])
  
  return(c(res$par, out_q))
}    

