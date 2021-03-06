old_eq_int_good = eq_int_good
old_eq_downstream_p = eq_downstream_p


grad(
  func = d_firm_main,
  x = c(old_eq_downstream_p$p_1A, old_eq_downstream_p$p_1B),
  p_2 = c(old_eq_downstream_p$p_2A, old_eq_downstream_p$p_2B),
  w = unlist(old_eq_int_good[1:4]),
  firm_1 = 1
)

grad(
  func = d_firm_main,
  x = c(old_eq_downstream_p$p_2A, old_eq_downstream_p$p_2B),
  p_1 = c(old_eq_downstream_p$p_1A, old_eq_downstream_p$p_1B),
  w = unlist(old_eq_int_good[1:4]),
  firm_1 = 0
)

optim_A = 1
if (integrated == 0) {
  grad(
    func = u_firm_unint,
    x = c(old_eq_int_good$w_1A, old_eq_int_good$w_2A),
    w_B = c(old_eq_int_good$w_1B, old_eq_int_good$w_2B)
  )
  
  # firm B
  optim_A = 0
  
  grad(
    func = u_firm_unint,
    x = c(old_eq_int_good$w_1B, old_eq_int_good$w_2B),
    w_A = c(old_eq_int_good$w_1A, old_eq_int_good$w_2A)
  )
} else {
  grad(
    func = u_firm_int,
    x = c(old_eq_int_good$w_1A, old_eq_int_good$w_2A),
    w_B = c(old_eq_int_good$w_1B, old_eq_int_good$w_2B)
  )
  
  # firm B
  optim_A = 0
  
  grad(
    func = u_firm_int,
    x = c(old_eq_int_good$w_1B, old_eq_int_good$w_2B),
    w_A = c(old_eq_int_good$w_1A, old_eq_int_good$w_2A)
  )
}

J = jacobian(cons_demand, x = unlist(eq_downstream_p))


# if integrated, optimal p_1A
(-eq_int_good$x_1A - eq_int_good$w_2A * J[3,1] + (eq_int_good$w_1B - eq_downstream_p$p_1B)*J[2,1])/(J[1,1])
