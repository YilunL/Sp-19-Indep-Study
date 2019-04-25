# p = c(1,2,3,4)
# 
# p_1A <- p[1]
# p_1B <- p[2]
# p_2A <- p[3]
# p_2B <- p[4]
# 
# ### params in nest A
# beta_0a = 1 # intercept
# beta_1a = 1  # price effect
# beta_2a = 0  # store effect for store 2
# 
# # Nest of Brand A goods
# odds_1A = exp(beta_0a - beta_1a * p_1A)
# odds_2A = exp(beta_0a - beta_1a * p_2A + beta_2a)
# 
# # probabilities
# P1A_A = odds_1A/(odds_1A + odds_2A)
# P2A_A = odds_2A/(odds_1A + odds_2A)
# 
# ### params in nest B
# beta_0b = 1 # intercept
# beta_1b = 1  # price effect
# beta_2b = 0  # store effect for store 2
# 
# # nest of brand B goods
# odds_1B = exp(beta_0b - beta_1b * p_1B)
# odds_2B = exp(beta_0b - beta_1b * p_2B + beta_2b)
# 
# # probabilities
# P1B_B = odds_1A/(odds_1A + odds_2A)
# P2B_B = odds_2A/(odds_1A + odds_2A)
# 
# ### Inclusive values
# IV_A = log(odds_1A + odds_2A)
# IV_B = log(odds_1B + odds_2B)
# 
# ### Branch level parameters
# lambda_A = 0.5
# lambda_B = 0.5
# gamma = c(1,2,3)  # coefficients for nest level
# y_A = c(0.5,0.5, 0.5)  # characteristics of nest A
# y_B = c(1, 0, 1)  # characteristics of nest B
# odds_A = exp(lambda_A * (t(y_A) %*% gamma) + IV_A)
# odds_B = exp(lambda_B * (t(y_B) %*% gamma) + IV_B)
# PA = odds_A/(1 + odds_A + odds_B)  # normalize for outside good
# PB = odds_B/(1 + odds_A + odds_B)
# 
# P1A = P1A_A * PA
# P1B = P1B_B * PB
# P2A = P2A_A * PA
# P2B = P2B_B * PB
# 
# P1A * (p[1] - 1) + P1B * (p[2] - 1) 
# 
# ############## returns same thing as above #################
# integrated = 0
# d_firm_main(c(1,2), c(3,4), c(1,1,1,1), 1)
# u_firm_unint(c(1,1), c(2,2))
# ###########################################################3

# # eq profits
pi_A <- 0  #upstream A
pi_B <- 0  #upstream B
pi_1 <- 0  #downstream 1
pi_2 <- 0  #downstream 2
eq_pi <- list(
  pi_A = pi_A,
  pi_B = pi_B,
  pi_1 = pi_1,
  pi_2 = pi_2
)

# firms specific intermediate demads
x_1A <- 0  # downstream firm 1's demand for intermediate good A
x_2A <- 0  # downstream firm 2's demand for intermediate good A
x_1B <- 0  # downstream firm 1's demand for intermediate good B
x_2B <- 0  # downstream firm 2's demand for intermediate good B

eq_int_good <-
  list(
    w_1A = 2, # initial guess for intermediate good costs
    w_1B = 1,
    w_2A = 2,
    w_2B = 1,
    x_1A = x_1A,
    x_1B = x_1B,
    x_2A = x_2A,
    x_2B = x_2B
  )

eq_downstream_p <-
  list(
    p_1A = 3,  # initial guess for downstream prices
    p_1B = 3,
    p_2A = 3,
    p_2B = 3
  )


integrated = 0
downstream_iter = 0
w_B = c(eq_int_good$w_1B, eq_int_good$w_2B)
w_A = c(eq_int_good$w_1A, eq_int_good$w_2A)

optim_1 = 1

downstream_iter <- downstream_iter + 1  # count iterations
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
      w = c(w_A, w_B),
      firm_1 = optim_1,
      method = "BFGS",
      control = list(maxit = 100000, reltol = 1E-12)
    )
  if (firm_1_optim$par[1] < 10){
    eq_downstream_p$p_1A <- firm_1_optim$par[1]
  } else {
    eq_downstream_p$p_1A <- 0
  }
  
  if (firm_1_optim$par[2] < 10){
    eq_downstream_p$p_1B <- firm_1_optim$par[2]
  } else {
    eq_downstream_p$p_1B <- 0
  }
  
  new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
  tol = abs(1 - new_dist / old_dist)
  
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
    eq_downstream_p$p_2A <- firm_2_optim$par[1]
  } else {
    eq_downstream_p$p_2A <- 0
  }
  
  if (firm_2_optim$par[2] < 10){
    eq_downstream_p$p_2B <- firm_2_optim$par[2]
  } else {
    eq_downstream_p$p_2B <- 0
  }
  new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
  tol = abs(1 - new_dist / old_dist)
  
} else {
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
    eq_downstream_p$p_2A <- firm_2_optim$par[1]
  } else {
    eq_downstream_p$p_2A <- 0
  }
  
  if (firm_2_optim$par[2] < 10){
    eq_downstream_p$p_2B <- firm_2_optim$par[2]
  } else {
    eq_downstream_p$p_2B <- 0
  }
  new_dist = sqrt(eq_pi$pi_1 ^ 2 + eq_pi$pi_2 ^ 2)
  tol = abs(1 - new_dist / old_dist)
}
