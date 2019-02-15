#############################################################################
# Upstream firms take it or leave it offers
#############################################################################

u_firms_unint <- function(){
  # w_A <<- runif(1)
  # w_B <<- runif(1)
  w_A <<- 0.375
  w_B <<- 0.375
  
  # linear production in downstream
  if (linear == 1){
    downstream1 <- d_firm_1_lin_unint(w_A,w_B)
    downstream2 <- d_firm_2_lin(w_A,w_B)
  }
  
  # cobb-douglas production in downstream
  if (cobb_douglas == 1){
    downstream1 <- d_firm_1_cd_unint(w_A,w_B)
    downstream2 <- d_firm_2_cd(w_A,w_B)
  }
  
  x_1A <<- downstream1[1]
  x_2A <<- downstream2[1]
  x_1B <<- downstream1[2]
  x_2B <<- downstream2[2]
  
  pi_A <<- (x_1A + x_2A) * w_A # profits
  pi_B <<- (x_1B + x_2B) * w_B # profits
  pi_1 <<- downstream1[5]
  pi_2 <<- downstream2[5]
}

u_firms_int <- function(){
  w_A <<- 0.375
  w_B <<- 0.375
  
  
}


#############################################################################
# Unintegrated upstream firms 
#############################################################################

u_firm_A_unint <- function(w_A) {
 #normalized upstream marginal cost to 0
  
  # linear production in downstream
  if (linear == 1){
    w_B = w_A
    downstream1 <- d_firm_1_lin_unint(w_A,w_B)
    downstream2 <- d_firm_2_lin(w_A,w_B)
  }
  
  # cobb-douglas production in downstream
  if (cobb_douglas == 1){
    downstream1 <- d_firm_1_cd_unint(w_A,w_B)
    downstream2 <- d_firm_2_cd(w_A,w_B)
  }

  x_1A <<- downstream1[1]
  x_2A <<- downstream2[1]
  
  pi_A = (x_1A + x_2A) * w_A # profits
  
  w_A <<- w_A
  
  return(-pi_A) #negative because optim does minimization
}

u_firm_B <- function(w_B) {
  #normalized upstream marginal cost to 0
  # linear production in downstream
  if (linear == 1){
    w_B = w_A
    if (integrated == 0) {
      downstream1 <- d_firm_1_lin_unint(w_A,w_B)
    }
    if (integrated == 1) {
      downstream1 <- d_firm_1_lin_int(w_A,w_B)
    }
    
    downstream2 <- d_firm_2_lin(w_A,w_B)
  }
 
  # cobb-douglas production in downstream
  if (cobb_douglas == 1){
    if (integrated == 0) {
      downstream1 <- d_firm_1_cd_unint(w_A,w_B)
    }
    if (integrated == 1) {
      downstream1 <- d_firm_1_cd_int(w_A,w_B)
    }
    
    downstream2 <- d_firm_2_cd(w_A,w_B)
  }

  x_1B <<- downstream1[2]
  x_2B <<- downstream2[2]
  
  pi_B = (x_1B + x_2B) * w_B # profits
  
  w_B <<- w_B
  
  return(-pi_B)
}

#############################################################################
# Integrated Firm A and Firm 1 
#############################################################################
u_firm_A_int <- function(w_A) {
  #normalized upstream marginal cost to 0
  
  w_B = w_A 
  
  downstream1 <- d_firm_1_cd_int(w_A,w_B)
  downstream2 <- d_firm_2_cd(w_A,w_B)
  x_1A <<- downstream1[1]
  x_2A <<- downstream2[1]
  
  pi_A = p_1 * q_1 + x_2A * w_A # profits
  
  w_A <<- w_A
  
  return(-pi_A) #negative because optim does minimization
}