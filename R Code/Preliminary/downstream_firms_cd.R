# Linear production


#############################################################################
# Unintegrated downstream firms
#############################################################################

d_firm_1_cd_unint <- function(w_A, w_B) {
  # Bertrand game
  p_1 <<-
    (1 + ((a/b)^b + (a/b)^(-a))*w_A^a*w_B^b) / (2 - k)  # costfunction for cobb-douglas
  p_2 <<- (1 + ((a/b)^b + (a/b)^(-a))*w_A^a*w_B^b) / (2 - k) 
  
  q_1 <<- max(1 - p_1 + k * p_2, 0)     # Differentiated product Bertrand, subst. param = k
  
  
  
  # model input demand
  # assume production function is q = x_1^a x_2^b; a + b = 1
  
  x_1A <- a*q_1*((a/b)^b + (a/b)^(-a))*(w_B/w_A)^b
  x_1B <- b*q_1*((a/b)^b + (a/b)^(-a))*(w_A/w_B)^a
  
  pi <- p_1*q_1 - w_A*x_1A - w_B*x_1B
  
  return(c(x_1A, x_1B, p_1, q_1, pi))
}

d_firm_2_cd <- function(w_A, w_B) {
  # Bertrand game
  p_1 <<-
    (1 + ((a/b)^b + (a/b)^(-a))*w_A^a*w_B^b) / (2 - k)  # costfunction for cobb-douglas is c(q) = 2q\sqrt{w1w2}
  p_2 <<- (1 + ((a/b)^b + (a/b)^(-a))*w_A^a*w_B^b) / (2 - k) 
  q_2 <<- max(1 - p_2 + k * p_1, 0)     # Differentiated product Bertrand, substitution param = 0.5
  
  x_2A <- a*q_2*((a/b)^b + (a/b)^(-a))*(w_B/w_A)^b
  x_2B <- b*q_2*((a/b)^b + (a/b)^(-a))*(w_A/w_B)^a
  
  pi <- p_2*q_2 - w_A*x_2A - w_B*x_2B
  
  
  return(c(x_2A, x_2B, p_2, q_2, pi))
}

#############################################################################
# integrated downstream firms
#############################################################################
