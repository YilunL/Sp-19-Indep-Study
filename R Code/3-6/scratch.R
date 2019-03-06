d_firm_main_test <- function(p_1, p_2, w_A, w_B, M){
  # offer by firm A to downstream firm 1
  w_1A <- w_A[1]
  
  # offer by firm A to downstream firm 2
  w_2A <- w_A[2]
  
  # offer by firm B to downstream firm 1
  w_1B <- w_B[1]
  
  # offer by firm B to downstream firm 2
  w_2B <- w_B[2]
  
  odds <- d_firm_prob(p_1,p_2)
  
  downstream1 <- d_firm_1(c(w_1A, w_1B), p_1, odds, M)
  downstream2 <- d_firm_2(c(w_2A, w_2B), p_2, odds, M)
  
  # quantity demanded
  x_1A <- downstream1[1]
  x_1B <- downstream1[2]
  
  x_2A <- downstream2[1]
  x_2B <- downstream2[2]
  
  # profits
  pi_1 <- downstream1[3]
  pi_2 <- downstream2[3]
  
  # price by firm 1 for good A
  p_1A <<- p_1[1]
  
  # price by firm 1 for good B
  p_1B <<- p_1[2]
  
  # price by firm 1 for good A
  p_2A <<- p_2[1]
  
  # price by firm 1 for good A
  p_2B <<- p_2[2]
  
  return(c(p_1A, p_1B, p_2A, p_2B, x_1A, x_1B, x_2A, x_2B, pi_1, pi_2))
}

# M is market size
M = 1
optim_1 = 1
w_A <- c(0.2, 0.2)
p_1 <- c(0.632, 0.632)
w_B <- c(0.2, 0.2)
p_2 <- c(0.632, 0.632)

odds <- d_firm_prob(p_1, p_2)

d_firm_main_test(p_1, p_2, w_A, w_B, M)



