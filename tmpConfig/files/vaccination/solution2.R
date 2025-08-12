# force of infection
foi <- R0 * sigma * (I + V_I) / N # R0 = beta / sigma => beta = R0 * sigma
# popsize
N <- S + I + R + V + V_S + V_I + V_R

# variables
# unvaccinated
deriv(S) <- -foi * S
deriv(I) <- foi * S - sigma * I
deriv(R) <- (1 - IFR) * sigma * I
deriv(D) <- IFR * sigma * I

# vaccinated
deriv(V) <- 0 # protected

deriv(V_S) <- -foi * V_S
deriv(V_I) <- foi * V_S - sigma * V_I
deriv(V_R) <- (1 - IFR) * sigma * V_I
deriv(V_D) <- IFR * sigma * V_I

# initial conditions
initial(S) <- (1 - vaccine_coverage) * N_init - I_init
initial(I) <- I_init
initial(R) <- 0
initial(D) <- 0

initial(V) <- vaccine_coverage * VE * N_init

initial(V_S) <- vaccine_coverage * (1 - VE) * N_init
initial(V_I) <- 0
initial(V_R) <- 0
initial(V_D) <- 0


# quantities to output
output(R_eff) <- R0 * (S + V_S) / N 
output(infections) <- foi * (S + V_S)
output(check_popsize) <- N + D + V_D

# parameters
N_init <- user(1e6, min = 0)
I_init <- user(1, min = 0)
R0 <- user(3, min = 0)
sigma <- user(0.5, min = 0)
IFR <- user(0.1, min = 0, max = 1)
vaccine_coverage <- user(0, min = 0, max = 1) # initial coverage of vaccination
VE <- user(0.9, min = 0, max = 1) # vaccine efficacy (all-or-nothing)
