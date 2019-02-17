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
  
  if (optim_A == 1) {
    return(-pi_A)
  } else {
    return(-pi_B)
  }
}

# u_firms_unint_B <- function(w_B, w_A) {
#   # offer by firm A to downstream firm 1
#   w_1A <- w_A[1]
#   
#   # offer by firm A to downstream firm 2
#   w_2A <- w_A[2]
#   
#   # offer by firm B to downstream firm 1
#   w_1B <- w_B[1]
#   
#   # offer by firm B to downstream firm 2
#   w_2B <- w_B[2]
#   
#   # cobb-douglas production in downstream
#   if (cobb_douglas == 1) {
#     # downstream firms set prices given their inputs
#     p_1 <- d_firm_cd_price(w_1A, w_1B)
#     p_2 <- d_firm_cd_price(w_2A, w_2B)
#     
#     # seeing prices, downstream firms produce (or don't)
#     downstream1 <- d_firm_1_cd_unint(w_1A, w_1B, c(p_1, p_2))
#     downstream2 <- d_firm_2_cd(w_2A, w_2B, c(p_1, p_2))
#   }
#   
#   # downstream outputs
#   q_1 <- downstream1[3]
#   q_2 <- downstream2[3]
#   
#   # downstream input demands (if rejected)
#   x_1A <- downstream1[1]
#   x_1B <- downstream1[2]
#   
#   x_2A <- downstream2[1]
#   x_2B <- downstream2[2]
#   
#   # profits
#   pi_A <- w_1A * x_1A + w_2A * x_2A # profits
#   pi_B <- w_1B * x_1B + w_2B * x_2B # profits
#   pi_1 <- downstream1[4]
#   pi_2 <- downstream2[4]
#   
#   eq <<-
#     list(
#       w_A = w_A,
#       w_B = w_B,
#       x_A = x_A,
#       x_B = x_B,
#       p_1 = p_1,
#       p_2 = p_2,
#       q_1 = q_1,
#       q_2 = q_2
#     )
#   eq_pi <<- list(
#     pi_A = pi_A,
#     pi_B = pi_B,
#     pi_1 = pi_1,
#     pi_2 = pi_2
#   )
#   eq_int_good <<-
#     list(
#       w_1A = w_1A,
#       w_2A = w_2A,
#       w_1B = w_1B,
#       w_2B = w_2B,
#       x_1A = x_1A,
#       x_2A = x_2A,
#       x_1B = x_1B,
#       x_2B = x_2B
#     )
#   
#   if (optim_A == 1) {
#     return(-pi_A)
#   } else {
#     return(-pi_B)
#   }
# }

#############################################################################
# integrated firms
#############################################################################

u_firms_int <- function() {
  # firm A and 1 are integrated, so price is 0 and unlimited quantity
  w_1A <<- 0
  x_1A <<- 1
  
  # offer by firm A to downstream firm 2
  w_2A <<- .421
  x_2A <<- .421
  
  # offer by firm B to downstream firm 1
  w_1B <<- .421
  x_1B <<- .421
  
  # offer by firm B to downstream firm 2
  w_2B <<- .421
  x_2B <<- .421
  
  # turn offers into vectors
  offer_1A <- c(w_1A, x_1A)
  offer_1B <- c(w_1B, x_1B)
  offer_2A <- c(w_2A, x_2A)
  offer_2B <- c(w_2B, x_2B)
  
  # cobb-douglas production in downstream
  if (cobb_douglas == 1) {
    # downstream firms set prices given their inputs
    p_1 <<- d_firm_cd_price(offer_1A, offer_1B)
    p_2 <<- d_firm_cd_price(offer_2A, offer_2B)
    
    markup <<- .6  # integrated firm has an uncompetitive incentive
    p_1 <<- p_1 + markup
    
    # seeing prices, downstream firms produce (or don't)
    downstream1 <- d_firm_1_cd_int(offer_1A, offer_1B)
    downstream2 <- d_firm_2_cd(offer_2A, offer_2B)
  }
  
  # downstream outputs
  q_1 <<- downstream1[3]
  q_2 <<- downstream2[3]
  
  # downstream input demands (if rejected)
  x_1A <<- downstream1[1]
  x_1B <<- downstream1[2]
  
  x_2A <<- downstream2[1]
  x_2B <<- downstream2[2]
  
  # profits
  pi_A <<- w_1A * x_1A + w_2A * x_2A # profits
  pi_B <<- w_1B * x_1B + w_2B * x_2B # profits
  pi_1 <<- downstream1[4]
  pi_2 <<- downstream2[4]
}