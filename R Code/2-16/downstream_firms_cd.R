# Linear production


#############################################################################
# Unintegrated downstream firms
#############################################################################
d_firm_cd_price <- function(offer_A, offer_B) {
  w_A <- offer_A[1]
  w_B <- offer_B[1]
  
  if(w_A >= 0 & w_B >= 0){
    p <-
      (1 + ((a / b) ^ b + (a / b) ^ (-a)) * w_A ^ a * w_B ^ b) / (2 - k)  # using costfunction for cobb-douglas
  } else {
    p <- 0
  }
 

  
  return(p)
}


d_firm_1_cd_unint <- function(offer_A, offer_B) {
  # Bertrand game
  q_demand <-
    max(1 - p_1 + k * p_2, 0)     # Differentiated product Bertrand, subst. param = k
  
  
  w_A <- offer_A[1]
  w_B <- offer_B[1]
  
  x_1A <- offer_A[2]
  x_1B <- offer_B[2]
  
  if ((x_1A >= 0) & (x_1B >= 0)) {
    q_1 <- min(x_1A ^ a * x_1B ^ b, q_demand)
  } else {
    q_1 <- 0
  }
  
  
  pi <- p_1 * q_1 - w_A * x_1A - w_B * x_1B
  
  if (pi < 0) {
    q_1 <- 0
    x_1A <- 0
    x_1B <- 0
  }
  
  return(c(x_1A, x_1B, q_1, pi))
}

d_firm_2_cd <- function(offer_A, offer_B) {
  # Bertrand game
  q_demand <-
    max(1 - p_2 + k * p_1, 0)     # Differentiated product Bertrand, substitution param = 0.5
  
  w_A <- offer_A[1]
  w_B <- offer_B[1]
  
  x_2A <- offer_A[2]
  x_2B <- offer_B[2]
  
  if ((x_2A >= 0) & (x_2B >= 0)) {
    q_2 <- min(x_2A ^ a * x_2B ^ b, q_demand)
  } else {
    q_2 <- 0
  }
  
  pi <- p_2 * q_2 - w_A * x_2A - w_B * x_2B
  
  if (pi < 0) {
    q_2 <- 0
    x_2A <- 0
    x_2B <- 0
  }
  
  return(c(x_2A, x_2B, q_2, pi))
}

#############################################################################
# integrated downstream firms
#############################################################################
d_firm_1_cd_int <- function(offer_A, offer_B) {
  # Bertrand game
  
  q_demand <-
    max(1 - p_1 + k * p_2, 0)     # Differentiated product Bertrand, subst. param = k
  
  w_A <- offer_A[1]
  w_B <- offer_B[1]
  
  x_1A <- offer_A[2]
  x_1B <- offer_B[2]
  
  q_1 <- min(x_1A ^ a * x_1B ^ b, q_demand)
  
  pi <- p_1 * q_1 - w_A * x_1A - w_B * x_1B
  
  if (pi < 0) {
    q_1 <- 0
    x_1A <- 0
    x_1B <- 0
  }
  
  return(c(x_1A, x_1B, q_1, pi))
}