dt <- 0.25              # Step size, in units of time
beta <- user(0.38)      # Transmission rate
p_sev <- user(0.1)      # Probability of severe disease if infected
gamma <- user(0.2)      # Transition rate from exposed to infectious
sigma_Is <- user(0.048) # Recovery rate from severe disease
sigma_Ia <- user(0.141) # Recovery rate from mild disease
mu <- user(0.024)       # Mortality rate from severe disease
N <- user(1e6)          # Total population
I_init <- user(10)      # Initial infected


# Derive initial numbers
S_init <- N - I_init


# Stochastic solution
initial(S) <- S_init
initial(Is) <- 0
initial(Ia) <- I_init 
initial(R) <- 0
initial(D) <- 0


# Force of infection; note we assume Is and Ia are equally infectious
FOI <- beta * (Is + Ia) / N


# Determine new infections
new_infections <- rbinom(S, FOI * dt)

new_I <- rbinom(E, gamma * dt)
new_Is <- new_I * p_sev
new_Ia <- new_I * (1 - p_sev)


# Two types of events for Is, either death or recovery (competing hazards)
n_deaths_Is <- rbinom(Is, mu * dt)
n_recoveries_Is <- rbinom(Is, sigma_Is * dt)


# All Ia are assumed to recover
n_recoveries_Ia <- rbinom(Ia, sigma_Ia * dt)


# update for next time step
update(S) <- S - new_infections
update(E) <- E + new_infections
update(Is) <- Is + new_Is - n_recoveries_Is - n_deaths_Is
update(Ia) <- Ia + new_Ia - n_recoveries_Ia
update(R) <- R + n_recoveries_Is + n_recoveries_Ia
update(D) <- D + n_deaths_Is

# other outputs of interest
initial(inf_incidence) <- 0
initial(hosp_incidence) <- 0
initial(deaths_incidence) <- 0
initial(h_recov_incidence) <- 0

update(inf_incidence) <- new_Ia + new_Is
update(hosp_incidence) <- new_Is
update(deaths_incidence) <- n_deaths_Is
update(h_recov_incidence) <- n_recoveries_Is
