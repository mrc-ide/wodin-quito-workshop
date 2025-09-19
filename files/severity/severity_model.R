dt <- 0.25              # Step size, in units of time
beta <- user(0.8)       # Transmission rate
p_sev <- user(0.3)      # Probability of severe disease if infected
sigma_Is <- user(0.071) # Recovery rate from severe disease
sigma_Ia <- user(0.071) # Recovery rate from mild disease
mu <- user(0.038)       # Mortality rate from severe disease
N <- user(10000)        # Total population
I_init <- user(10)      # Initial infected


# Derive initial numbers
S_init <- N - I_init
Is_init <- I_init * p_sev
Ia_init <- I_init - Is_init


# Stochastic solution
initial(S) <- S_init
initial(Is) <- Is_init
initial(Ia) <- Ia_init 
initial(R) <- 0
initial(D) <- 0


# Force of infection; note we assume Is and Ia are equally infectious
FOI <- beta * (Is + Ia) / N


# Determine new infections
new_infections <- rbinom(S, FOI * dt)
new_Is <- new_infections * p_sev
new_Ia <- new_infections * (1 - p_sev)


# Two types of events for Is, either death or recovery (competing hazards)
n_deaths_Is <- rbinom(Is, mu * dt)
n_recoveries_Is <- rbinom(Is, sigma_Is * dt)


# All Ia are assumed to recover
n_recoveries_Ia <- rbinom(Ia, sigma_Ia * dt)


# update for next time step
update(S) <- S - new_infections
update(Is) <- Is + new_Is - n_recoveries_Is - n_deaths_Is
update(Ia) <- Ia + new_Ia - n_recoveries_Ia
update(R) <- R + n_recoveries_Is + n_recoveries_Ia
update(D) <- D + n_deaths_Is
