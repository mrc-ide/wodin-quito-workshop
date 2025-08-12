foi <- R0 * sigma * (I + V_I) / N
foi_V <- (1 - ve_I) * foi # reduced foi in V
IFR_V <- (1 - ve_D) * IFR # reduced IFR in V

# variables
deriv(S) <- -foi * S - r_V * S
deriv(I) <- foi * S - sigma * I
deriv(R) <- (1 - IFR) * sigma * I
deriv(D) <- IFR * sigma * I

deriv(V) <- (1 - p_take) * r_V * S - foi_V * V
deriv(V_I) <- foi_V * V
deriv(V_R) <- (1 - IFR_V) * sigma * V_I
deriv(V_D) <- IFR_V * sigma * V_I

deriv(V_P) <- p_take * r_V * S

# initial conditions
initial(S) <- N - I_init
initial(I) <- I_init
initial(R) <- 0
initial(D) <- 0
initial(V) <- 0 # vaccinated
initial(V_I) <- 0 # infected despite vaccination
initial(V_R) <- 0 # vaccinated and recovered
initial(V_D) <- 0 # died despite vaccination
initial(V_P) <- 0

# quantities to output
V_total <-  V_S + V_I + V_R + V_D
output(V_total) <- V_total
output(D_total) <- D + V_D
output(I_total) <- I + V_I
output(R_total) <- R + V_R
output(popsize) <- S + I + R + D + V_total

# parameters
N <- user(1e6)
I_init <- user(1)
R0 <- user(3, min = 0)
sigma <- user(0.5, min = 0)
IFR <- user(0.1, min = 0, max = 1)
r_V <- user(0.01) # rate of vaccination
p_take <- user(0.8, min = 0, max = 1) # probability vaccine protects perfectly
ve_I <- user(0.8, min = 0, max = 1) # vaccine efficacy against infection
ve_D <- user(0.8, min = 0, max = 1) # vaccine efficacy against death
