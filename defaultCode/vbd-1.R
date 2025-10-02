# Simple model of malaria

# human equations
deriv(S_h) <- - lambda_h * S_h + sigma * I_h
deriv(I_h) <- lambda_h * S_h - sigma * I_h

# mosquito equations
deriv(S_v) <- e * (S_v + I_v) - lambda_v * S_v - mu * S_v
deriv(I_v) <- lambda_v * S_v - mu * I_v

lambda_h = m * b_h * a * I_v   # human force of infection
lambda_v = b_v * a * I_h      # mosquito force of infection

# initial conditions
initial(S_h) <- 1 - I_init_h
initial(I_h) <- I_init_h

initial(S_v) <- 1 - I_init_v
initial(I_v) <- I_init_v

# outputs
output(R0) <- (m * a^2 * b_h * b_v) / (sigma * mu)
output(m_threshold) <- (sigma * mu) / (a^2 * b_h * b_v)
output(EIR) <- m * a * I_v * 365

# parameter values 
I_init_h <- 0.001             # proportion of infectious humans at start of epidemic
I_init_v <- 0                 # proportion of infectious mosquitoes at start of epidemic

mu <- user(0.1)               # mosquito death rate
e <- mu                       # mosquito birth rate
sigma <- user(0.02)           # human recovery rate

m = user(10)                  # density of female mosquitoes per person
a = user(0.3)                 # biting rate per female mosquito
b_h = 0.2                     # probability of infection in susceptible human given bite from infectious mosquito
b_v = 0.05                    # probability of infection in susceptible mosquito given bite on an infectious human
