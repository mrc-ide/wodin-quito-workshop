# human equations
deriv(S_h) <- -lambda_h * S_h + sigma * I_h
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

# outputs
output(R0) <- (m * a^2 * b_h * b_v * p_surv_lat) / (sigma * mu)
output(m_threshold) <- (sigma * mu) / (a^2 * b_h * b_v) * p_surv_lat
output(EIR) <- m * a * I_v * 365

## counters for infection per day 
infections_per_day <- lambda_h * (I_h + S_h)

output(infections_per_day) <- infections_per_day
##<- HERE add equivalents for infections per day in unvaccinated and vaccinated as well as total 

initial(infections_cumulative) <- 0
deriv(infections_cumulative) <- infections_per_day
##<- HERE add equivalents for cumulative infections in unvaccinated and vaccinated as well as total 

output(cases_per_day) <- p_case * infections_per_day
##<- HERE add equivalents for infections per day in unvaccinated and vaccinated as well as total 

output(cases_cumulative) <- p_case * infections_cumulative
##<- HERE add equivalents for cumulative infections in unvaccinated and vaccinated as well as total 

# parameter values
p_case <- 0.1
I_init_h <- 0.001            # number of infectious humans at start of epidemic
I_init_v <- 0                # number of infectious mosquitoes at start of epidemic

dur_infectivity <- 50        # duration of infectivity
sigma <- 1 / dur_infectivity # human recovery rate


mu <- 0.1                   # mosquito death rate
e <- mu                     # mosquito birth rate
n <- 12                     # extrinsic incubation period
p_surv_lat <- exp(-mu * n)  # probability a mosquito survives the latent period.
m <- 10                     # density of female mosquitoes per person. [0.5-40]
a <- 0.3                    # biting rate per female mosquito [0.01-0.5]
b_h <- 0.2                  # probability of infection in susceptible human given bite from infectious mosquito [0.2-0.5]
b_v <- 0.05                 # probability of infection in susceptible mosquito given bite on an infectious human [0.5]
