x = seq(eq_int_good$w_1A - 0.15, eq_int_good$w_1A + 0.15, 0.005)
y = vector(length = length(x))
y_int = vector(length = length(x))

eq_0 <- eq
eq_downstream_p_0 <- eq_downstream_p
eq_int_good_0 <- eq_int_good
eq_pi_0 <- eq_pi


optim_A = 1
if (integrated == 0) {
  for (i in (1:length(x))) {
    y[i] = u_firm_unint(c(x[i], eq_int_good$w_2A),
                        c(eq_int_good$w_1B, eq_int_good$w_2B),
                        1)
    eq <- eq_0 
    eq_downstream_p <- eq_downstream_p_0
    eq_int_good <- eq_int_good_0
    eq_pi <- eq_pi_0
  }

  plot(x, y)
}

if (integrated == 1) {
  for (i in (1:length(x))) {
    y_int[i] = u_firm_int(eq_int_good$w_2A,
                          c(eq_int_good$w_1B, eq_int_good$w_2B),
                          1)
  }
  plot(x, y_int)
}
