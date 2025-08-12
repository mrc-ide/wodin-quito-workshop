# human equations
deriv(S_h) <- -lambda_h * S_h + sigma * I_h- Vx_rate * S_h
deriv(I_h) <- lambda_h * S_h - sigma * I_h - Vx_rate * I_h
deriv(SV_h) <- -lambda_h * (1 - VIB_eff) * SV_h + sigma_vacc * IV_h + Vx_rate * S_h
deriv(IV_h) <- lambda_h * (1 - VIB_eff) * SV_h - sigma_vacc * IV_h + Vx_rate * I_h

# mosquito equations
deriv(S_v) <- e - lambda_v * S_v - mu * S_v
deriv(E_v) <- lambda_v * S_v - lambda_v_delay * S_v_delay * p_surv_lat - mu * E_v
deriv(I_v) <- lambda_v_delay * S_v_delay * p_surv_lat - mu * I_v

lambda_h <- m * b_h * a * I_v         # human force of infection
lambda_v <- b_v * a * (I_h + (1 - VTB_eff) * IV_h) # mosquito force of infection
lambda_v_delay <- b_v * a * (I_h_delay + (1 - VTB_eff) * IV_h_delay)   # delayed mosquito force of infection

I_h_delay  <- delay(I_h, n)
IV_h_delay <- delay(IV_h, n)
S_v_delay  <- delay(S_v, n)

# initial conditions
initial(S_h)  <- 1 - I_init_h
initial(I_h)  <- I_init_h
initial(SV_h) <- 0
initial(IV_h) <- 0
initial(S_v)  <- 1 - I_init_v
initial(E_v)  <- 0
initial(I_v)  <- I_init_v

## Key outputs and counters 
output(infectious) <- I_h + IV_h
infections_per_day_unvacc <- lambda_h * (I_h + S_h)
infections_per_day_vacc <- lambda_h * (1 - VIB_eff) * (IV_h + SV_h)
output(infections_per_day_unvacc) <- infections_per_day_unvacc
output(infections_per_day_vacc) <- infections_per_day_vacc
output(infections_per_day) <- infections_per_day_unvacc + infections_per_day_vacc
initial(infections_cumulative_unvacc) <- 0
deriv(infections_cumulative_unvacc) <- infections_per_day_unvacc
initial(infections_cumulative_vacc) <- 0
deriv(infections_cumulative_vacc) <- infections_per_day_vacc
output(infections_cumulative) <- infections_cumulative_unvacc + infections_cumulative_vacc

p_case <- 0.1
cases_per_day_vacc <- infections_per_day_vacc * p_case * (1 - VDB_eff)
cases_per_day_unvacc <- infections_per_day_unvacc * p_case
output(cases_per_day_vacc) <- cases_per_day_vacc
output(cases_per_day_unvacc) <- cases_per_day_unvacc
output(cases_per_day) <- cases_per_day_vacc + cases_per_day_unvacc

cases_cumulative_vacc <- infections_cumulative_vacc * p_case * (1 - VDB_eff)
cases_cumulative_unvacc <- infections_cumulative_unvacc * p_case
output(cases_cumulative_vacc) <- cases_cumulative_vacc
output(cases_cumulative_unvacc) <- cases_cumulative_unvacc
output(cases_cumulative) <- cases_cumulative_vacc + cases_cumulative_unvacc


initial(vaccines) <- 0
deriv(vaccines) <- Vx_rate * S_h + Vx_rate * I_h


# other outputs
output(R0) <- (m * a^2 * b_h * b_v * p_surv_lat) / (sigma * mu)
output(m_threshold) <- (sigma * mu) / (a^2 * b_h * b_v * p_surv_lat)
output(EIR) <- m * a * I_v * 365
output(N) <- S_h + SV_h + I_h + IV_h
output(N_vacc) <- SV_h + IV_h
output(N_unvacc) <- S_h + I_h
## Vaccine parameters
VDB_eff <- user(0)
VIB_eff <- user(0) #Infections blocking efficacy
VTB_eff <- user(0) #Transmission blocking efficacy
VBS_dur_red <- user(0) #Reduction in duration of infectivity of vaccinated infectious individuals
dur_infection_vacc <- dur_infection * (1 - VBS_dur_red)
sigma_vacc <- 1 / dur_infection_vacc # human recovery rate in vaccinated

## vaccine roll-out parameters
Vx_rate <- if((t >= Vx_start) && t < Vx_end) -log(1 - Vx_cov) / (Vx_end - Vx_start) else 0
Vx_cov <- user(0)
Vx_start <- user(3650)
Vx_end <- user(3680)

# parameter values
I_init_h <- 0.001             # number of infectious humans at start of epidemic
I_init_v <- 0                 # number of infectious mosquitoes at start of epidemic

dur_infection<-50
sigma <- 1/dur_infection        # human recovery rate in unvaccinated

mu <- 0.1                 # mosquito death rate
e <- mu                       # mosquito birth rate
n <- 12                        # extrinsic incubation period
p_surv_lat <- exp(-mu*n)         # probability a mosquito survives the latent period.
m <- user(10)                  # density of female mosquitoes per person. [0.5-40]
a <- 0.3                 # biting rate per female mosquito [0.01-0.5]
b_h <- 0.2                     # probability of infection in susceptible human given bite from infectious mosquito [0.2-0.5]
b_v <- 0.05                    # probability of infection in susceptible mosquito given bite on an infectious human [0.5]
