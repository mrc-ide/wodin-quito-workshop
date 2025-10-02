# Latent model

# human equations
deriv(S_h) <- - lambda_h * S_h + sigma * I_h
deriv(I_h) <- lambda_h * S_h - sigma * I_h

# mosquito equations
deriv(S_v) <- e - lambda_v * S_v - mu * S_v
deriv(E_v) <- lambda_v * S_v - lambda_v_delay * S_v_delay * p_surv_lat - mu * E_v
deriv(I_v) <- lambda_v_delay * S_v_delay * p_surv_lat - mu * I_v

lambda_h = m * b_h * a * I_v         # human force of infection
lambda_v = b_v * a * I_h             # mosquito force of infection
lambda_v_delay = b_v * a * I_h_delay # delayed mosquito force of infection

I_h_delay = delay(I_h, n)
S_v_delay = delay(S_v, n)

# initial conditions
initial(S_h) <- 1 - I_init_h
initial(I_h) <- I_init_h

initial(S_v) <- 1 - I_init_v
initial(E_v) <- 0
initial(I_v) <- I_init_v

# parameter values
I_init_h <- 0.001             # number of infectious humans at start of epidemic
I_init_v <- 0                 # number of infectious mosquitoes at start of epidemic

sigma <- user(0.02)           # human recovery rate

mu <- 0.1                 # mosquito death rate
e <- mu                       # mosquito birth rate
n = 12                        # extrinsic incubation period
p_surv_lat <- exp(-mu*n)	     # probability a mosquito survives the latent period.

m = user(10)                  # density of female mosquitoes per person. 
a = user(0.3)                 # biting rate per female mosquito 
b_h = 0.2                     # probability of infection in susceptible human given bite from infectious mosquito [0.2-0.5]
b_v = 0.05                    # probability of infection in susceptible mosquito given bite on an infectious human [0.5]
