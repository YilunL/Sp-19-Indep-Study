#############################################################################
# Unintegrated downsream firms 
#############################################################################

d_firm_1 <- function(x_1A, x_1B, w_A, w_B) {
  # model consumer demand
  
  # given the production function q = x_1x_2, the cost equation is 
  # c(q) = 2\sqrt{w_1w_1q}
  
  # Bertrand game with non-linear cost function
  p_1 = 
  p_2 = 
  q_1 =   
  
  # model input demand
  eval_1 <- function(x){
    return(list("objective" = -(p_1*(x[1]*x[2]) - w_A*x[1] - w_B*x[2]),
                "gradient" = c(-p_1*x[2] + w_A, 
                               -p_1*x[1] + w_B)))
  }
  
  eval_g_eq <- function(x){
    q <- q_1
    
    constr <- c(q - x[1]*x[2])
    
    grad <- c(-x[2],
              -x[1])
    return(list("constraints" = constr, "jacobian" = grad))
  }
  
  #initial values 
  x0 <- c(x_1A, x_1B)
  
  lb <- c(0,0)
  
  opts <- list("algorithm"="NLOPT_LD_LBFGS",
               "xtol_rel"=1.0e-8)
  
  res <- nloptr(x0 = x0, 
                eval_f = eval_1, 
                eval_g_eq = eval_g_eq, 
                lb = lb,
                opts = opts)
  
  return(c(res, p_2, q_1))
}

d_firm_1(0.2,0.2,0.4,0.4)
