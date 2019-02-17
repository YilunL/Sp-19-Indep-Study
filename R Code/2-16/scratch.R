#############################################################################
# Upstream firms take it or leave it offers
#############################################################################

u_firms_unint <- function(w_x){
  # offer by firm A to downstream firm 1
  w_1A <<- w_x[1]
  x_1A <<- w_x[2]
  
  # offer by firm A to downstream firm 2
  w_2A <<- w_x[3]
  x_2A <<- w_x[4]
  
  # offer by firm B to downstream firm 1
  w_1B <<- w_x[5]
  x_1B <<- w_x[6]
  
  # offer by firm B to downstream firm 2
  w_2B <<- w_x[7]
  x_2B <<- w_x[8]
  
  # turn offers into vectors
  offer_1A <- c(w_1A, x_1A)
  offer_1B <- c(w_1B, x_1B)
  offer_2A <- c(w_2A, x_2A)
  offer_2B <- c(w_2B, x_2B)
  
  # cobb-douglas production in downstream
  if (cobb_douglas == 1){
    
    # downstream firms set prices given their inputs
    p_1 <<- d_firm_cd_price(offer_1A, offer_1B)
    p_2 <<- d_firm_cd_price(offer_2A, offer_2B)
    
    # seeing prices, downstream firms produce (or don't)
    downstream1 <- d_firm_1_cd_unint(offer_1A, offer_1B)
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
  
  return(-(pi_A + pi_B)) #max upstream profits
}

#############################################################################
# integrated firms
#############################################################################

u_firms_int <- function(w_x){
  # firm A and 1 are integrated, so price is 0 and unlimited quantity
  w_1A <<- 0
  x_1A <<- w_x[2]
  
  # offer by firm A to downstream firm 2
  w_2A <<- w_x[3]
  x_2A <<- w_x[4]
  
  # offer by firm B to downstream firm 1
  w_1B <<- w_x[5]
  x_1B <<- w_x[6]
  
  # offer by firm B to downstream firm 2
  w_2B <<- w_x[7]
  x_2B <<- w_x[8]
  
  # turn offers into vectors
  offer_1A <- c(w_1A, x_1A)
  offer_1B <- c(w_1B, x_1B)
  offer_2A <- c(w_2A, x_2A)
  offer_2B <- c(w_2B, x_2B)
  
  # cobb-douglas production in downstream
  if (cobb_douglas == 1){
    
    # downstream firms set prices given their inputs
    p_1 <- d_firm_cd_price(offer_1A, offer_1B)
    p_2 <<- d_firm_cd_price(offer_2A, offer_2B)
    
    markup = 0.6  # integrated firm has an uncompetitive incentive
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
  
  return(-(pi_A + pi_1 + pi_B)) #max int firm + upstream profit
}

