# force of infection
foi <- R0 * sigma * I / N # R0 = beta / sigma => beta = R0 * sigma
# popsize
N <- S + I + R

# variables
deriv(S) <- -foi * S
deriv(I) <- foi * S - sigma * I
deriv(R) <- (1 - IFR) * sigma * I
deriv(D) <- IFR * sigma * I

# initial conditions
initial(S) <- N_init - I_init
initial(I) <- I_init
initial(R) <- 0
initial(D) <- 0

# quantities to output
output(infections) <- foi * S
output(R_eff) <- R0 * S / N

# parameters
N_init <- user(1e6, min = 0) # initial population size
I_init <- user(1, min = 0) # initial number of infections
R0 <- user(3, min = 0) # basic reproduction number
sigma <- user(0.2, min = 0) # rate of recovery = 1 / 5 (days)
IFR <- user(0.1, min = 0, max = 1) # infection fatality ratio
