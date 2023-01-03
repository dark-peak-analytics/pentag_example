# Eric the erroneous economist...
rm(list = ls())

# number of cycles
n_cycles <- 100

# discount rate
dr <- 0.035
v_dw <- 1 / (1 + dr) ^ (0:(n_cycles-1))

# vector of health state names
v_hs_names <- c("H", "S", "D")

# vector of health state costs
v_hs_cost <- c("H" = 10, "S" = 1000, "D" = 0)

# vector of health state utilities
v_hs_utils <- c("H" = 0.9, "S" = 0.6, "D" = "0")

# discount rate


# state transition matrix
m_P <- matrix(data = c(0.9, 0.10, 0.00, 
                       0.0, 0.99, 0.01, 
                       0.0, 0.00, 1.00),
              nrow = 3,
              ncol = 3,
              byrow = T,
              dimnames = list(v_hs_names, v_hs_names))

# markov trace with 100 cycles
m_TR <- matrix(data = NA, nrow = n_cycles, ncol = length(v_hs_names))

# initialise with 100% healthy in the first period
m_TR[1, ] <- c("H" = 1, "S" = 0, "D" = 0)

# loop through periods 2 to 100
for(t in 2:n_cycles) {
  # multiply the row of the markov trace by the transition probability matrix
  m_TR[t,] <- m_TR[t - 1,] %*% m_P 
  
}

# simple plot to check...
plot(m_TR[,1], type = "l", col = "blue", ylab = "Proportion", xlab = "cycle")
lines(m_TR[,2], col = "red")
lines(m_TR[,3], col = "black")

# calculate total utility, cost and discounted utility and cost.
utility   <-  sum(m_TR %*% v_hs_utils)
cost      <-  sum(m_TR %*% v_hs_cost)
d_utility <-  as.numeric(t(v_dw) %*% m_TR %*% v_hs_utils)
d_cost    <-  as.numeric(t(v_dw) %*% m_TR %*% v_hs_cost)


