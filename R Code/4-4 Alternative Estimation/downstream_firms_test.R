# Linear production


#############################################################################
# Unintegrated downstream firms
#############################################################################
quantities <- function(p){
  p_1 = c(p[1], p[2])
  p_2 = c(p[3], p[4])
  odds_1 = cons_demand_2(p_1, 0)  # store 1
  odds_2 = cons_demand_2(p_2, 1)  # store 2
  q_1A = odds_1[1]/(1 + sum(c(odds_1, odds_2)))
  q_1B = odds_1[2]/(1 + sum(c(odds_1, odds_2)))
  q_2A = odds_2[1]/(1 + sum(c(odds_1, odds_2)))
  q_2B = odds_2[2]/(1 + sum(c(odds_1, odds_2)))
  return(c(q_1A, q_1B, q_2A, q_2B))
}

d_firm_unint <- function(w, p_1, p_2, firm_1){
  w_1A <- w[1]
  w_1B <- w[2]
  w_2A <- w[3]
  w_2B <- w[4]
  p_1A <- p_1[1]
  p_1B <- p_1[2]
  p_2A <- p_2[1]
  p_2B <- p_2[2]
  
  q = quantities(c(p_1, p_2))
  
  eq_q$q_1A <<- q[1]
  eq_q$q_1B <<- q[2]
  eq_q$q_2A <<- q[3]
  eq_q$q_2B <<- q[4]
  
  eq_pi$pi_1 <<- (p_1A - w_1A)*eq_q$q_1A + (p_1B - w_1B)*eq_q$q_1B
  eq_pi$pi_2 <<- (p_2A - w_2A)*eq_q$q_2A + (p_2B - w_2B)*eq_q$q_2B
  
  if(firm_1 == 1){
    return(-eq_pi$pi_1)
  } else {
    return(-eq_pi$pi_2)
  }
}

d_firm_unint_main <- function(w_A, w_B){
  w_1A <- w_A[1]
  w_1B <- w_B[1]
  w_2A <- w_A[2]
  w_2B <- w_B[2]
  
  w = c(w_1A, w_1B, w_2A, w_2B)
  
  curr_tol = 1
  iter = 0
  while (curr_tol > tol & iter < 10000) {
    old_dist <- sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
    
    downstream1 <- optim(
      par = c(eq_p$p_1A, eq_p$p_1B),
      fn =  d_firm_unint,
      w = w,
      p_2 = c(eq_p$p_2A, eq_p$p_2B),
      firm_1 = 1,
      method = "BFGS",
      control = list(maxit = 10000, reltol = 1E-12)
    )
    eq_p$p_1A <<- downstream1$par[1]
    eq_p$p_1B <<- downstream1$par[2]
    
    downstream2 <- optim(
      par = c(eq_p$p_2A, eq_p$p_2B),
      fn =  d_firm_unint,
      w = w,
      p_1 = c(eq_p$p_1A, eq_p$p_1B), 
      firm_1 = 0,
      method = "BFGS",
      control = list(maxit = 10000, reltol = 1E-12)
    )
    eq_p$p_2A <<- downstream2$par[1]
    eq_p$p_2B <<- downstream2$par[2]
    
    new_dist <- sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
    
    curr_tol = abs(new_dist - old_dist)/old_dist
    iter = iter + 1
    
    return(eq_q)
  }
}
# c(2.19, 2.19, 2.19, 2.19)
# c(1.11, 1.11, 1.11, 1.11)

############################## INTEGRATED ###########################
d_firm_int <- function(w, p_1, p_2, firm_1){
  w_1A <- w[1]
  w_1B <- w[2]
  w_2A <- w[3]
  w_2B <- w[4]
  p_1A <- p_1[1]
  p_1B <- p_1[2]
  p_2A <- p_2[1]
  p_2B <- p_2[2]
  
  q = quantities(c(p_1, p_2))
  
  eq_q$q_1A <<- q[1]
  eq_q$q_1B <<- q[2]
  eq_q$q_2A <<- q[3]
  eq_q$q_2B <<- q[4]
  
  eq_pi$pi_1 <<- (p_1A - w_1A)*eq_q$q_1A + (p_1B - w_1B)*eq_q$q_1B
  eq_pi$pi_2 <<- (p_2A - w_2A)*eq_q$q_2A + (p_2B - w_2B)*eq_q$q_2B
  
  if(firm_1 == 1){
    return(-eq_pi$pi_1 - eq_pi$pi_A)
  } else {
    return(-eq_pi$pi_2)
  }
}

d_firm_int_main <- function(w_A, w_B){
  w_1A <- w_A[1]
  w_1B <- w_B[1]
  w_2A <- w_A[2]
  w_2B <- w_B[2]
  
  w = c(w_1A, w_1B, w_2A, w_2B)
  
  curr_tol = 1
  iter = 0
  while (curr_tol > tol & iter < 10000) {
    old_dist <- sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_A^2 + eq_pi$pi_2 ^ 2)
    
    downstream1 <- optim(
      par = c(eq_p$p_1A, eq_p$p_1B),
      fn =  d_firm_int,
      w = w,
      p_2 = c(eq_p$p_2A, eq_p$p_2B),
      firm_1 = 1,
      method = "BFGS",
      control = list(maxit = 10000, reltol = 1E-12)
    )
    eq_p$p_1A <<- downstream1$par[1]
    eq_p$p_1B <<- downstream1$par[2]
    
    downstream2 <- optim(
      par = c(eq_p$p_2A, eq_p$p_2B),
      fn =  d_firm_int,
      w = w,
      p_1 = c(eq_p$p_1A, eq_p$p_1B), 
      firm_1 = 0,
      method = "BFGS",
      control = list(maxit = 10000, reltol = 1E-12)
    )
    eq_p$p_2A <<- downstream2$par[1]
    eq_p$p_2B <<- downstream2$par[2]
    
    new_dist <- sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_A^2 + eq_pi$pi_2 ^ 2)
    
    curr_tol = abs(new_dist - old_dist)/old_dist
    iter = iter + 1
    
    return(eq_q)
  }
}
