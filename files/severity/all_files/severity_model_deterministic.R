
beta <- user(0.38)      # Transmission rate
p_sev <- user(0.1)      # Probability of severe disease if infected
sigma_Is <- user(0.08)  # Recovery rate from severe disease
sigma_Ia <- user(0.08)  # Recovery rate from mild disease
mu <- user(0.06)        # Mortality rate from severe disease
N <- user(1e6)          # Total population
I_init <- user(10)      # Initial infected


# Initial state
initial(S) <- N - I_init
initial(Is) <- 0
initial(Ia) <- I_init 
initial(R) <- 0
initial(D) <- 0


# Force of infection; assume Is and Ia are equally infectious
FOI <- beta * (Is + Ia) / N


# update for next time step
deriv(S) <- - S * FOI
deriv(Is) <- S * FOI * p_sev - Is * mu - Is * sigma_Is
deriv(Ia) <- S * FOI * (1 - p_sev) - Ia * sigma_Ia
deriv(R) <- Is * sigma_Is + Ia * sigma_Ia
deriv(D) <- Is * mu


# other outputs of interest
output(new_deaths) <- Is * mu
output(new_hospitalised) <- S * FOI * p_sev
