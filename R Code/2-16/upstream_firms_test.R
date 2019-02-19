#############################################################################
# Upstream firms take it or leave it offers
#############################################################################

u_firms_unint <- function(w_A, w_B) {
  # offer by firm A to downstream firm 1
  w_1A <- w_A[1]
  
  # offer by firm A to downstream firm 2
  w_2A <- w_A[2]
  
  # offer by firm B to downstream firm 1
  w_1B <- w_B[1]
  
  # offer by firm B to downstream firm 2
  w_2B <- w_B[2]
  
  # cobb-douglas production in downstream
  if (cobb_douglas == 1) {
    # downstream firms set prices given their inputs
    p_1 <- d_firm_cd_price(w_1A, w_1B)
    p_2 <- d_firm_cd_price(w_2A, w_2B)
    
    # seeing prices, downstream firms produce (or don't)
    downstream1 <- d_firm_1_cd_unint(w_1A, w_1B, c(p_1, p_2))
    downstream2 <- d_firm_2_cd(w_2A, w_2B, c(p_1, p_2))
  }
  
  # downstream outputs
  q_1 <- downstream1[3]
  q_2 <- downstream2[3]
  
  # downstream input demands (if rejected)
  x_1A <- downstream1[1]
  x_1B <- downstream1[2]
  
  x_2A <- downstream2[1]
  x_2B <- downstream2[2]
  
  # profits
  pi_A <- w_1A * x_1A + w_2A * x_2A # profits
  pi_B <- w_1B * x_1B + w_2B * x_2B # profits
  pi_1 <- downstream1[4]
  pi_2 <- downstream2[4]
  
  # aggregate intermediate good price and output
  x_A = x_1A + x_2A
  x_B = x_1B + x_2B
  w_A = (w_1A * x_1A + w_2A * x_2A) / x_A
  w_B = (w_1B * x_1B + w_2B * x_2B) / x_B
  
  eq <<-
    list(
      x_A = x_A,
      x_B = x_B,
      w_A = w_A,
      w_B = w_B,
      p_1 = p_1,
      p_2 = p_2,
      q_1 = q_1,
      q_2 = q_2
    )
  
  eq_pi <<- list(
    pi_A = pi_A,
    pi_B = pi_B,
    pi_1 = pi_1,
    pi_2 = pi_2
  )
  eq_int_good <<-
    list(
      w_1A = w_1A,
      w_2A = w_2A,
      w_1B = w_1B,
      w_2B = w_2B,
      x_1A = x_1A,
      x_2A = x_2A,
      x_1B = x_1B,
      x_2B = x_2B
    )
  
    return(-pi_A)

}

