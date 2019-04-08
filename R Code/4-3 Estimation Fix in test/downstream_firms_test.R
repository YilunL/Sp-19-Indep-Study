# Linear production


#############################################################################
# Unintegrated downstream firms
#############################################################################
quantities <- function(p_1, p_2){
  odds_1 = cons_demand_2(p_1, 0)  # store 1
  odds_2 = cons_demand_2(p_2, 1)  # store 2
  q_1A = odds_1[1]/(1 + sum(c(odds_1, odds_2)))
  q_1B = odds_1[2]/(1 + sum(c(odds_1, odds_2)))
  q_2A = odds_2[1]/(1 + sum(c(odds_1, odds_2)))
  q_2B = odds_2[2]/(1 + sum(c(odds_1, odds_2)))
  return(c(q_1A, q_1B, q_2A, q_2B))
}

d_firm_prob <- function(p_1, p_2){
  odds_1 <- cons_demand_2(p_1, 0)  # calculate exp(vj) for store 1
  odds_2 <- cons_demand_2(p_2, 1)  # calculate exp(vj) for store 2
  odds <- c(odds_1, odds_2)
  
  return(odds)
}

d_firm_1 <- function(w, p, odds, M) {
  w_A <- w[1]  # input prices offered by upstream firms
  w_B <- w[2]
  p_A <- p[1]  # prices to offer
  p_B <- p[2]
  
  odds_1A <- odds[1]
  odds_1B <- odds[2]
  
  share_1A <- odds_1A / (1 + sum(odds))  # compute market share of good A
  share_1B <- odds_1B / (1 + sum(odds))  # compute market share of good B
  x_1A <- share_1A * M  # compute quantity demanded given market size  
  x_1B <- share_1B * M  # compute quantity demanded given market size
  
  pi <- x_1A * (p_A - w_A) + x_1B * (p_B - w_B)  # assume consumers buy one of each good
  
  return(c(x_1A, x_1B, pi))
}

# exact same copy of function for firm 1, except with variables for firm 2
d_firm_2 <- function(w, p, odds, M) {
  w_A <- w[1]  # input prices offered by upstream firms
  w_B <- w[2]
  p_A <- p[1]  # prices to offer
  p_B <- p[2]
  
  odds_2A <- odds[3]
  odds_2B <- odds[4]
  
  share_2A <- odds_2A / (1 + sum(odds))  # compute market share of good A
  share_2B <- odds_2B / (1 + sum(odds))  # compute market share of good B
  x_2A <- share_2A * M  # compute quantity demanded given market size
  x_2B <- share_2B * M  # compute quantity demanded given market size
  
  pi <- x_2A * (p_A - w_A) + x_2B * (p_B - w_B)  # assume consumers buy one of each good
  
  return(c(x_2A, x_2B, pi))
}

d_firm_main <- function(p_1, p_2, w_A, w_B, M, firm_1){
  # offer by firm A to downstream firm 1
  w_1A <- w_A[1]
  
  # offer by firm A to downstream firm 2
  w_2A <- w_A[2]
  
  # offer by firm B to downstream firm 1
  w_1B <- w_B[1]
  
  # offer by firm B to downstream firm 2
  w_2B <- w_B[2]
  
  # given a guess for prices, calculate exp(vj) for the two firms
  odds <- d_firm_prob(p_1,p_2)
  
  downstream1 <- d_firm_1(c(w_1A, w_1B), p_1, odds, M)
  downstream2 <- d_firm_2(c(w_2A, w_2B), p_2, odds, M)
  
  # quantity demanded of intermediate goods
  eq_int_good$x_1A <<- downstream1[1]
  eq_int_good$x_1B <<- downstream1[2]
  
  eq_int_good$x_2A <<- downstream2[1]
  eq_int_good$x_2B <<- downstream2[2]
  
  
  # price by firm 1 for good A
  eq_downstream_p$p_1A <<- p_1[1]
  
  # price by firm 1 for good B
  eq_downstream_p$p_1B <<- p_1[2]
  
  # price by firm 1 for good A
  eq_downstream_p$p_2A <<- p_2[1]
  
  # price by firm 1 for good A
  eq_downstream_p$p_2B <<- p_2[2]
  
  # profits
  eq_pi$pi_1 <<- downstream1[3]
  eq_pi$pi_2 <<- downstream2[3]
  
  if(firm_1 == 1){
    if (integrated == 0){
      return(-downstream1[3])
    } else if (integrated == 1) {
      return(-downstream1[3] - (w_2A * downstream2[1]))
    }
  } else {
    return(-downstream2[3])
  }
}


########################################################################################
#                                Integrated 
########################################################################################

# d_firm_int_main <- function(p_1, p_2, w_A, w_B, M, firm_1){
#   # offer by firm A to downstream firm 1
#   w_1A <- 0
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
#   # given a guess for prices, calculate exp(vj) for the two firms
#   odds <- d_firm_prob(p_1,p_2)
#   
#   q <- quantities(p_1, p_2)
#   
#   # quantity demanded of intermediate goods
#   eq_int_good$x_1A <<- q[1]
#   eq_int_good$x_1B <<- q[2]
#   
#   eq_int_good$x_2A <<- q[3]
#   eq_int_good$x_2B <<- q[4]
#   
#   if(firm_1 == 0){
#     pi_2 <- q[3] * (p_2[1] - w_2A) + q[4] * (p_2[2] - w_2B)
#   }
#   
#   # price by firm 1 for good A
#   eq_downstream_p$p_1A <<- p_1[1]
#   
#   # price by firm 1 for good B
#   eq_downstream_p$p_1B <<- p_1[2]
#   
#   # price by firm 1 for good A
#   eq_downstream_p$p_2A <<- p_2[1]
#   
#   # price by firm 1 for good A
#   eq_downstream_p$p_2B <<- p_2[2]
#   
#   # profits
#   eq_pi$pi_2 <<- pi_2
#   
#   if(firm_1 == 0){
#     return(-pi_2)
#   } 
# }

