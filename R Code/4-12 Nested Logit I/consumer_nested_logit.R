# cons_demand <- function(p){
#   # consumer utility inside nest: u(k|j) = a_k|j + x_k|j'B
#   # consumer utility of nest: y'Gamma
# 
#   p_1A <- p[1]
#   p_1B <- p[2]
#   p_2A <- p[3]
#   p_2B <- p[4]
# 
#   ### params in nest A
#   beta_0a = 0 # intercept
#   beta_1a = 2  # price effect
#   beta_2a = 3  # store effect for store 2
# 
#   # Nest of Brand A goods
#   odds_1A = exp(beta_0a - beta_1a * p_1A)
#   odds_2A = exp(beta_0a - beta_1a * p_2A + beta_2a)
# 
#   # probabilities
#   P1A_A = odds_1A/(odds_1A + odds_2A)
#   P2A_A = odds_2A/(odds_1A + odds_2A)
# 
#   ### params in nest B
#   beta_0b = -2   # intercept
#   beta_1b = 2  # price effect
#   beta_2b = 0  # store effect for store 2
#   beta_3 = 0 # brand effect B
# 
#   # nest of brand B goods
#   odds_1B = exp(beta_0b - beta_1b * p_1B + beta_3)
#   odds_2B = exp(beta_0b - beta_1b * p_2B + beta_2b + beta_3)
# 
#   # probabilities
#   P1B_B = odds_1B/(odds_1B + odds_2B)
#   P2B_B = odds_2B/(odds_1B + odds_2B)
# 
#   ### Inclusive values
#   IV_A = log(odds_1A + odds_2A)
#   IV_B = log(odds_1B + odds_2B)
# 
#   ### Branch level parameters
#   lambda_A = 0.5
#   lambda_B = 0.5
#   gamma = c(.5,.5,.5)  # coefficients for nest level
#   y_A = c(1.5, 1.5, 1.5)  # characteristics of nest A
#   y_B = c(1, 1, 1)  # characteristics of nest B
#   odds_A = exp(lambda_A * (t(y_A) %*% gamma + IV_A))
#   odds_B = exp(lambda_B * (t(y_B) %*% gamma + IV_B))
#   PA = odds_A/(1 + odds_A + odds_B)  # normalize for outside good
#   PB = odds_B/(1 + odds_A + odds_B)
# 
#   P1A = P1A_A * PA
#   P1B = P1B_B * PB
#   P2A = P2A_A * PA
#   P2B = P2B_B * PB
# 
#   return(c(P1A, P1B, P2A, P2B))
# }


cons_demand <- function (p) {
  p_1A <- p[1]
  p_1B <- p[2]
  p_2A <- p[3]
  p_2B <- p[4]

  ### params in nest A
  beta_0a = 1 # intercept
  beta_1a = 1  # price effect
  beta_2a = 0  # store effect for store 2
  lambda_A = .1

  # Nest of Brand A goods
  odds_1A = exp((beta_0a - beta_1a * p_1A)/lambda_A)
  odds_2A = exp((beta_0a - beta_1a * p_2A + beta_2a)/lambda_A)

  # probabilities
  P1A_A = odds_1A/(odds_1A + odds_2A)
  P2A_A = odds_2A/(odds_1A + odds_2A)

  ### params in nest B
  beta_0b = 2 # intercept
  beta_1b = 1  # price effect
  beta_2b = 0  # store effect for store 2
  beta_3 = 0 # brand effect B
  lambda_B = .5

  # nest of brand B goods
  odds_1B = exp((beta_0b - beta_1b * p_1B + beta_3)/lambda_B)
  odds_2B = exp((beta_0b - beta_1b * p_2B + beta_2b + beta_3)/lambda_B)

  # probabilities
  P1B_B = odds_1B/(odds_1B + odds_2B)
  P2B_B = odds_2B/(odds_1B + odds_2B)

  ### Inclusive values
  IV_A = log(odds_1A + odds_2A)
  IV_B = log(odds_1B + odds_2B)

  ### Branch level parameters


  gamma = c(.5,.5,.5)  # coefficients for nest level
  y_A = c(1, 1, 1)  # characteristics of nest A
  y_B = c(1, 1, 1)  # characteristics of nest B
  odds_A = exp((t(y_A) %*% gamma) + lambda_A * IV_A)
  odds_B = exp((t(y_B) %*% gamma) + lambda_B * IV_B)
  PA = odds_A/(1 + odds_A + odds_B)  # normalize for outside good
  PB = odds_B/(1 + odds_A + odds_B)

  P1A = P1A_A * PA
  P1B = P1B_B * PB
  P2A = P2A_A * PA
  P2B = P2B_B * PB

  return(c(P1A, P1B, P2A, P2B))
}