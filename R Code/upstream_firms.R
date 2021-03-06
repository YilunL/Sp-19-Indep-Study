#############################################################################
# Upstream firms take it or leave it offers
#############################################################################
u_firm_unint <- function(w_A, w_B){
  tol = 1
  
  while (tol > 1E-8) {
    optim_1 = sample(c(0,1), 1)
    downstream_iter <<- downstream_iter + 1  # count iterations
    # optim_1 = 1 - optim_1  # optimizing which intermediate firm's offer
    pi_1_old <- eq_pi$pi_1 # keeping tabs on old profits
    pi_2_old <- eq_pi$pi_2
    old_dist <- sqrt(pi_1_old ^ 2 + pi_2_old ^ 2)
    
    # optimize firm 1 given firm 2 prices are fixed
    if (optim_1 == 1) {
      # firm 1 goes first
      firm_1_optim <-
        optim(
          par = c(eq_downstream_p$p_1A, eq_downstream_p$p_1B),
          fn = d_firm_main,
          p_2 = c(eq_downstream_p$p_2A, eq_downstream_p$p_2B),
          w = c(w_A, w_B),
          firm_1 = optim_1,
          method = "BFGS",
          control = list(maxit = 100000, reltol = 1E-12)
        )
      if (firm_1_optim$par[1] < 10){
        eq_downstream_p$p_1A <<- firm_1_optim$par[1]
      } else {
        eq_downstream_p$p_1A <<- 0
      }
      
      if (firm_1_optim$par[2] < 10){
        eq_downstream_p$p_1B <<- firm_1_optim$par[2]
      } else {
        eq_downstream_p$p_1B <<- 0
      }
      
      new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
      tol = abs(1 - new_dist / old_dist)
      
      # firm 2 goes second
      firm_2_optim <-
        optim(
          par = c(eq_downstream_p$p_2A, eq_downstream_p$p_2B),
          fn = d_firm_main,
          p_1 = c(eq_downstream_p$p_1A, eq_downstream_p$p_1B),
          w = c(w_A, w_B),
          firm_1 = 1 - optim_1,
          method = "BFGS",
          control = list(maxit = 100000, reltol = 1E-12)
        )
      if (firm_2_optim$par[1] < 10){
        eq_downstream_p$p_2A <<- firm_2_optim$par[1]
      } else {
        eq_downstream_p$p_2A <<- 0
      }
      
      if (firm_2_optim$par[2] < 10){
        eq_downstream_p$p_2B <<- firm_2_optim$par[2]
      } else {
        eq_downstream_p$p_2B <<- 0
      }
      new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
      tol = abs(1 - new_dist / old_dist)
      
    } else {
      # firm 2 goes first 
      firm_2_optim <-
        optim(
          par = c(eq_downstream_p$p_2A, eq_downstream_p$p_2B),
          fn = d_firm_main,
          p_1 = c(eq_downstream_p$p_1A, eq_downstream_p$p_1B),
          w = c(w_A, w_B),
          firm_1 = optim_1,
          method = "BFGS",
          control = list(maxit = 100000, reltol = 1E-12)
        )
      if (firm_2_optim$par[1] < 10){
        eq_downstream_p$p_2A <<- firm_2_optim$par[1]
      } else {
        eq_downstream_p$p_2A <<- 0
      }
      
      if (firm_2_optim$par[2] < 10){
        eq_downstream_p$p_2B <<- firm_2_optim$par[2]
      } else {
        eq_downstream_p$p_2B <<- 0
      }
      new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
      tol = abs(1 - new_dist / old_dist)
      
      # firm 1 goes second 
      firm_1_optim <-
        optim(
          par = c(eq_downstream_p$p_1A, eq_downstream_p$p_1B),
          fn = d_firm_main,
          p_2 = c(eq_downstream_p$p_2A, eq_downstream_p$p_2B),
          w = c(w_A, w_B),
          firm_1 = 1 - optim_1,
          method = "BFGS",
          control = list(maxit = 100000, reltol = 1E-12)
        )
      if (firm_1_optim$par[1] < 10){
        eq_downstream_p$p_1A <<- firm_1_optim$par[1]
      } else {
        eq_downstream_p$p_1A <<- 0
      }
      
      if (firm_1_optim$par[2] < 10){
        eq_downstream_p$p_1B <<- firm_1_optim$par[2]
      } else {
        eq_downstream_p$p_1B <<- 0
      }
      
      new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
      tol = abs(1 - new_dist / old_dist)
    }
  }
  
  # offer by firm A to downstream firm 1
  w_1A <- w_A[1]
  
  # offer by firm A to downstream firm 2
  w_2A <- w_A[2]
  
  # offer by firm B to downstream firm 1
  w_1B <- w_B[1]
  
  # offer by firm B to downstream firm 2
  w_2B <- w_B[2]
  
  # downstream input demands
  x_1A <- eq_int_good$x_1A
  x_1B <- eq_int_good$x_1B
  
  x_2A <- eq_int_good$x_2A
  x_2B <- eq_int_good$x_2B
  
  
  # upstream profits
  pi_A = w_1A * x_1A + w_2A * x_2A # profits
  pi_B = w_1B * x_1B + w_2B * x_2B # profits
  
  # upstream profits
  
  eq_pi$pi_A <<- pi_A
  eq_pi$pi_B <<- pi_B
  
  
  if (optim_A == 1) {
    return(-pi_A)
  } else {
    return(-pi_B)
  }
}

#################################################### INTEGRATED ##############################################

u_firm_int <- function(w_A, w_B) {
  tol = 1
  w_A[1] <- 0
  
  while (tol > 1E-8) {
    optim_1 = 1 - sample(c(0,1),1)
    downstream_iter <<- downstream_iter + 1  # count iterations
    # optim_1 = 1 - optim_1  # optimizing which intermediate firm's offer
    pi_1_old <- eq_pi$pi_1 # keeping tabs on old profits
    pi_2_old <- eq_pi$pi_2
    old_dist <- sqrt(pi_1_old ^ 2 + pi_2_old ^ 2)

    # optimize firm 1 given firm 2 prices are fixed
    if (optim_1 == 1) {
      firm_1_optim <-
        optim(
          par = c(eq_downstream_p$p_1A, eq_downstream_p$p_1B),
          fn = d_firm_main,
          p_2 = c(eq_downstream_p$p_2A, eq_downstream_p$p_2B),
          w = c(0, w_A[2], w_B),
          firm_1 = optim_1,
          method = "BFGS",
          control = list(maxit = 100000, reltol = 1E-12)
        )
      if (firm_1_optim$par[1] < 10){
        eq_downstream_p$p_1A <<- firm_1_optim$par[1]
      } else {
        eq_downstream_p$p_1A <<- 0
      }
      
      if (firm_1_optim$par[2] < 10){
        eq_downstream_p$p_1B <<- firm_1_optim$par[2]
      } else {
        eq_downstream_p$p_1B <<- 0
      }
      new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
      tol = abs(1 - new_dist / old_dist)
      
      # now optimize firm 2 second
      firm_2_optim <-
        optim(
          par = c(eq_downstream_p$p_2A, eq_downstream_p$p_2B),
          fn = d_firm_main,
          p_1 = c(eq_downstream_p$p_1A, eq_downstream_p$p_1B),
          w = c(0, w_A[2], w_B),
          firm_1 = 1 - optim_1,
          method = "BFGS",
          control = list(maxit = 100000, reltol = 1E-12)
        )
      if (firm_2_optim$par[1] < 10){
        eq_downstream_p$p_2A <<- firm_2_optim$par[1]
      } else {
        eq_downstream_p$p_2A <<- 0
      }
      
      if (firm_2_optim$par[2] < 10){
        eq_downstream_p$p_2B <<- firm_2_optim$par[2]
      } else {
        eq_downstream_p$p_2B <<- 0
      }
      new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
      tol = abs(1 - new_dist / old_dist)
      
    } else {
      # optimize firm 2 first
      firm_2_optim <-
        optim(
          par = c(eq_downstream_p$p_2A, eq_downstream_p$p_2B),
          fn = d_firm_main,
          p_1 = c(eq_downstream_p$p_1A, eq_downstream_p$p_1B),
          w = c(0, w_A[2], w_B),
          firm_1 = optim_1,
          method = "BFGS",
          control = list(maxit = 100000, reltol = 1E-12)
        )
      if (firm_2_optim$par[1] < 10){
        eq_downstream_p$p_2A <<- firm_2_optim$par[1]
      } else {
        eq_downstream_p$p_2A <<- 0
      }
      
      if (firm_2_optim$par[2] < 10){
        eq_downstream_p$p_2B <<- firm_2_optim$par[2]
      } else {
        eq_downstream_p$p_2B <<- 0
      }
      new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
      tol = abs(1 - new_dist / old_dist)
      
      # then optimize firm 1 second
      firm_1_optim <-
        optim(
          par = c(eq_downstream_p$p_1A, eq_downstream_p$p_1B),
          fn = d_firm_main,
          p_2 = c(eq_downstream_p$p_2A, eq_downstream_p$p_2B),
          w = c(0, w_A[2], w_B),
          firm_1 = 1 - optim_1,
          method = "BFGS",
          control = list(maxit = 100000, reltol = 1E-12)
        )
      if (firm_1_optim$par[1] < 10){
        eq_downstream_p$p_1A <<- firm_1_optim$par[1]
      } else {
        eq_downstream_p$p_1A <<- 0
      }
      
      if (firm_1_optim$par[2] < 10){
        eq_downstream_p$p_1B <<- firm_1_optim$par[2]
      } else {
        eq_downstream_p$p_1B <<- 0
      }
      new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
      tol = abs(1 - new_dist / old_dist)
    }
  }

  # offer by firm A to downstream firm 1
  w_1A <- 0
  
  # offer by firm A to downstream firm 2
  w_2A <- w_A[2]
  
  # offer by firm B to downstream firm 1
  w_1B <- w_B[1]
  
  # offer by firm B to downstream firm 2
  w_2B <- w_B[2]
  
  # downstream input demands
  x_1A <- eq_int_good$x_1A
  x_1B <- eq_int_good$x_1B
  
  x_2A <- eq_int_good$x_2A
  x_2B <- eq_int_good$x_2B
  
  
  # upstream profits
  pi_A = w_1A * x_1A + w_2A * x_2A # profits
  pi_B = w_1B * x_1B + w_2B * x_2B # profits
  
  # upstream profits
  
  eq_pi$pi_A <<- pi_A
  eq_pi$pi_B <<- pi_B
  
  if (optim_A == 1) {
    return(-pi_A - eq_pi$pi_1)
  } else {
    return(-pi_B)
  }
}

#################################################### FORECLOSED ##############################################

