## downstream firms
d_firm_1 <- function(w){
  # observe following demand curve: Q = a - b1p1 + b2p2;  b1 > b2
  w_1 = w[1]
  w_2 = w[2]
  
  a <- 1
  b1 <- 1
  b2 <- .5
  
  # firm's production function: (x1x2)^0.5
  c <- 2*(w_1*w_2)^0.5  # marginal cost
  
  # diff prod. bertrand
  p <<- (a + c * b1)/(2 * b1 - b2)
  q <<- b1*(a + c*(b2 - b1))/(2*b1 - b2)
  
  # profit function
  pi <- p * q - 2*q*(w_1*w_2)^0.5
  
  return(-pi)
}

u_firm_1 <- function(w){
  
}




q = 0
p = 0

d_firm_1(c(0.4, 0.4))

out <- optim(c(0.4,0.4), d_firm_1)




############################ downstream

d_firm_1_cd_unint <- function(w_A, w_B) {
  # Bertrand game
  p_1 <<-
    (1 + 2*(w_A*w_B)^(1/2)) / (2 - k) # costfunction for cobb-douglas is c(q) = 2q\sqrt{w1w2}
  p_2 <<- (1 + 2*(w_A*w_B)^(1/2)) / (2 - k)
  
  q_1 <<-
    1 - p_1 + k * p_2     # Differentiated product Bertrand, subst. param = k
  

  
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
    (1 + 2*(w_A*w_B)^(1/2)) / (2 - k) # costfunction for cobb-douglas is c(q) = 2q\sqrt{w1w2}
  p_2 <<- (1 + 2*(w_A*w_B)^(1/2)) / (2 - k)
  q_2 <<-
    1 - p_2 + k * p_1     # Differentiated product Bertrand, substitution param = 0.5
  
  x_2A <- a*q_2*((a/b)^b + (a/b)^(-a))*(w_B/w_A)^b
  x_2B <- b*q_2*((a/b)^b + (a/b)^(-a))*(w_A/w_B)^a
  
  pi <- p_2*q_2 - w_A*x_2A - w_B*x_2B
  

  return(c(x_2A, x_2B, p_2, q_2, pi))
}

d_firm_1_cd_unint(0.185, 0.7)
d_firm_2_cd(0.185, 0.7)



