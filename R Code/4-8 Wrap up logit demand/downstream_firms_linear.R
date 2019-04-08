# Linear production


#############################################################################
# Unintegrated downstream firms 
#############################################################################

d_firm_1_lin_unint <- function(w_A, w_B) {
  w = w_A
  # Bertrand game
  p_1 <<- (1 + w) / (2 - k) # constant marginal cost, assuming w_A = w_B
  p_2 <<- (1 + w) / (2 - k)
  q_1 <<- 1 - p_1 + k * p_2     # Differentiated product Bertrand, substitution param = 0.5
  
  
  
  # model input demand
  # assume production function is q = x_1 + x_2
  
  
  eval_1 <- function(x) {
    return(list(
      "objective" = -(p_1 * (x[1] + x[2]) - w * (x[1] + x[2])),
      "gradient" = c(-p_1 + w,-p_1 + w)
    ))
  }
  
  eval_g_eq <- function(x) {
    constr <- c(x[1] + x[2] - q_1)
    
    grad <- c(1,1)
    return(list("constraints" = constr, "jacobian" = grad))
  }
  
  #initial values
  x0 <- c(0, 0)
  
  # upper and lower bounds
  lb <- c(0, 0)
  ub <- c(1, 1)
  
  # optimize
  local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                      "xtol_rel"  = 1.0e-7 )
  
  opts <- list("algorithm" = "NLOPT_LD_AUGLAG",
               "xtol_rel" = 1.0e-7,
               "local_opts" = local_opts)
  
  res <- nloptr(
    x0 = x0,
    eval_f = eval_1,
    eval_g_eq = eval_g_eq,
    lb = lb,
    ub = ub,
    opts = opts
  )
  
  pi = -(res$objective)  #profit is negative of obj value because optim mins
  

  return(c(res$solution, p_1, q_1, pi))
}

d_firm_2_lin <- function(w_A, w_B) {
  w = w_A
  # Bertrand game
  p_1 <<- (1 + w) / (2 - k) # constant marginal cost, assuming w_A = w_B
  p_2 <<- (1 + w) / (2 - k)
  q_2 <<- 1 - p_2 + k * p_1     # Differentiated product Bertrand, substitution param = 0.5
  
  
  
  # model input demand
  # assume production function is q = x_1 + x_2
  
  
  eval_1 <- function(x) {
    return(list(
      "objective" = -(p_1 * (x[1] + x[2]) - w * (x[1] + x[2])),
      "gradient" = c(-p_1 + w,-p_1 + w)
    ))
  }
  
  eval_g_eq <- function(x) {
    constr <- c(x[1] + x[2] - q_2)
    
    grad <- c(1,1)
    return(list("constraints" = constr, "jacobian" = grad))
  }
  
  #initial values
  x0 <- c(0, 0)
  
  # upper and lower bounds
  lb <- c(0, 0)
  ub <- c(1, 1)
  
  # optimize
  local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                      "xtol_rel"  = 1.0e-7 )
  
  opts <- list("algorithm" = "NLOPT_LD_AUGLAG",
               "xtol_rel" = 1.0e-7,
               "local_opts" = local_opts)
  
  res <- nloptr(
    x0 = x0,
    eval_f = eval_1,
    eval_g_eq = eval_g_eq,
    lb = lb,
    ub = ub,
    opts = opts
  )
  
  pi = -(res$objective)  #profit is negative of obj value because optim mins
  
  return(c(res$solution, p_2, q_2, pi))
}

