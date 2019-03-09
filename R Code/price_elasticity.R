# price elasticity calculator

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
  p_1A <- p_1[1]
  
  # price by firm 1 for good B
  p_1B <- p_1[2]
  
  # price by firm 1 for good A
  p_2A <- p_2[1]
  
  # price by firm 1 for good A
  p_2B <- p_2[2]
  
  return(c(p_1A, p_1B, p_2A, p_2B, x_1A, x_1B, x_2A, x_2B, pi_1, pi_2))
}

# M is market size
epsilon = 1E-5

# original output
w_A <- c(eq_int_good$w_1A, eq_int_good$w_2A)
p_1 <- c(eq_downstream_p$p_1A, eq_downstream_p$p_1B)
w_B <- c(eq_int_good$w_1B, eq_int_good$w_2B)
p_2 <- c(eq_downstream_p$p_2A, eq_downstream_p$p_2B)

odds <- d_firm_prob(p_1, p_2)

orig_out <- d_firm_main_test(p_1, p_2, w_A, w_B, M)

# slight change in p_1A
p_1 <- c(eq_downstream_p$p_1A + epsilon, eq_downstream_p$p_1B)

odds <- d_firm_prob(p_1, p_2)

new_out1A <- d_firm_main_test(p_1, p_2, w_A, w_B, M)

dq_1A_dp_1A <- (new_out1A[5] - orig_out[5])/epsilon #dq1A/dp1A
dq_1B_dp_1A <-(new_out1A[6] - orig_out[6])/epsilon #dq1B/dp1A

# slight change in p_1B
p_1 <- c(eq_downstream_p$p_1A, eq_downstream_p$p_1B + epsilon)

odds <- d_firm_prob(p_1, p_2)

new_out1B <- d_firm_main_test(p_1, p_2, w_A, w_B, M)

dq_1A_dp_1B <- (new_out1B[5] - orig_out[5])/epsilon #dq1A/dp1B
dq_1B_dp_1B <- (new_out1B[6] - orig_out[6])/epsilon #dq1B/dp1B

# slight change in p_2A
p_1 <- c(eq_downstream_p$p_1A, eq_downstream_p$p_1B)
p_2 <- c(eq_downstream_p$p_2A + epsilon, eq_downstream_p$p_2B)

odds <- d_firm_prob(p_1, p_2)

new_out2A <- d_firm_main_test(p_1, p_2, w_A, w_B, M)

dq_2A_dp_2A <- (new_out2A[7] - orig_out[7])/epsilon #dq2A/dp2A
dq_2B_dp_2A <- (new_out2A[8] - orig_out[8])/epsilon #dq2B/dp2A

# slight change in p_2B
p_2 <- c(eq_downstream_p$p_2A, eq_downstream_p$p_2B + epsilon)

odds <- d_firm_prob(p_1, p_2)

new_out2B <- d_firm_main_test(p_1, p_2, w_A, w_B, M)

dq_2A_dp_2B <- (new_out2B[7] - orig_out[7])/epsilon #dq2A/dp2B
dq_2B_dp_2B <- (new_out2B[8] - orig_out[8])/epsilon #dq2B/dp2B