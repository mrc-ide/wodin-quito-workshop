# force of infection
foi <- R0 * sigma * I / N # R0 = beta / sigma => beta = R0 * sigma
# popsize
N <- S + I + R + V
# variables
# unvaccinated
deriv(S) <- -foi * S
deriv(I) <- foi * S - sigma * I
deriv(R) <- (1 - IFR) * sigma * I
deriv(D) <- IFR * sigma * I

# vaccinated
deriv(V) <- 0

# initial conditions
initial(S) <- (1 - vaccine_coverage) * N_init - I_init
initial(I) <- I_init
initial(R) <- 0
initial(D) <- 0
initial(V) <- vaccine_coverage * N_init

# quantities to output
output(new_infections) <- foi * S
output(R_eff) <- R0 * S / N 
output(check_popsize) <- N + D

# parameters
N_init <- user(1e6, min = 0)
I_init <- user(1, min = 0)
R0 <- user(3, min = 0)
sigma <- user(0.5, min = 0)
IFR <- user(0.1, min = 0, max = 1)
vaccine_coverage <- user(0.5, min = 0, max = 1) # initial coverage of vaccination
