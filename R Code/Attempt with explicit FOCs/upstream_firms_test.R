#############################################################################
# Upstream firms take it or leave it offers
#############################################################################

# #col i is d(output of main)/dw_i
# jacobian(func = d_main, x = c(0.5,0.5,0.5,0.5))

u_firm_a <- function(w_A, w_B){
  
  # downstream_iter <<- downstream_iter + 1
  
  # offer by firm A to downstream firm 1
  w_1A <- w_A[1]
  
  # offer by firm B to downstream firm 1
  w_1B <- w_B[1]
  
  # offer by firm A to downstream firm 2
  w_2A <- w_A[2]
  
  # offer by firm B to downstream firm 2
  w_2B <- w_B[2]
  
  # downstream price and quantities
  d_pq <- d_main(c(w_1A, w_1B, w_2A, w_2B))
  p <- d_pq[1:4]
  q <- d_pq[5:8] #q_1A, q_1B, q_2A, q_2B
  
  pi_A <- w_1A * q[1] + w_2A * q[3]

  return(c(pi_A, p, q))
}

u_firm_b <- function(w_A, w_B){
  
  # downstream_iter <<- downstream_iter + 1
  
  # offer by firm A to downstream firm 1
  w_1A <- w_A[1]
  
  # offer by firm B to downstream firm 1
  w_1B <- w_B[1]
  
  # offer by firm A to downstream firm 2
  w_2A <- w_A[2]
  
  # offer by firm B to downstream firm 2
  w_2B <- w_B[2]
  
  # downstream price and quantities
  d_pq <- d_main(c(w_1A, w_1B, w_2A, w_2B))
  p <- d_pq[1:4]
  q <- d_pq[5:8] #q_1A, q_1B, q_2A, q_2B
  
  pi_B <- w_1B * q[2] + w_2B * q[4]
  
  return(c(pi_B, p, q))
}


  

#################################################### FORECLOSED ##############################################

