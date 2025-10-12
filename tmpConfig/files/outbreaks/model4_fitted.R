################################################################################
# Model 4 elaborates on Model 2 by explicitly modelling infections with 
# and underlying health condition.
# Keep all other parameters the same but introduce a probability of being 
# having an underlying health condition (p_TRUE). Assume those with underlying 
# health conditions have a higher CFR (estimate from linelist). Assume the 
# probability of having an underlying health condition is the same for 
# hospitalised and community infections. 
# 
#   S = # susceptible 
#   E = # latent (split in two for more realistic distributed latency)
#   I_d_TRUE = # early infectious - not yet hospitalised - will die 
#               # have underlying health conditions
#   I_d_FALSE = # early infectious - not yet hospitalised - will die
#               # no underlying health conditions
#   I_r_TRUE = # early infectious - not yet hospitalised - will recover
#               # have underlying health conditions
#   I_r_FALSE = # early infectious - not yet hospitalised - will recover
#               # no underlying health conditions
#   H_d_TRUE = # hospitalised - will die
#               # have underlying health conditions
#   H_d_FALSE = # hospitalised - will die
#               # no underlying health conditions
#   H_r_TRUE = # hospitalised - will recover
#               # have underlying health conditions
#   H_r_FALSE = # hospitalised - will recover
#               # no underlying health conditions
#   C_d_TRUE = # community (non-hospitalised) - will die
#               # have underlying health conditions
#   C_d_FALSE = # community (non-hospitalised) - will die
#               # no underlying health conditions
#   C_r_TRUE = # community (non-hospitalised) - will recover
#               # have underlying health conditions
#   C_r_FALSE = # community (non-hospitalised) - will recover
#               # no underlying health conditions
#   R = # recovered
#   
# Fit to same parameters as model 2. 
################################################################################

################################################################################
### Underlying health conditions model dynamics
################################################################################

# Susceptible
deriv (S) <- - S/N * (beta_r * ((I_r_FALSE + I_r_TRUE) + (C_r_FALSE + C_r_TRUE)) + 
                        beta_d * ((I_d_FALSE + I_d_TRUE) + (C_d_FALSE + C_d_TRUE)))

# Exposed (but not infectious)
deriv (E) <- S/N * (beta_r * (I_r_FALSE + I_r_TRUE) + 
                      beta_d * (I_d_FALSE + I_d_TRUE)) - 
  gamma * E

# Infected with no underlying health condition
deriv (I_d_FALSE) <- (1-p_TRUE) * cfr_FALSE * gamma * E - sigma_h * I_d_FALSE 
deriv (I_r_FALSE) <- (1-p_TRUE)*(1 - cfr_FALSE) * gamma * E- sigma_h * I_r_FALSE

# Infected with underlying health conditions
deriv (I_d_TRUE) <- (p_TRUE) * cfr_TRUE * gamma * E - sigma_h * I_d_TRUE 
deriv (I_r_TRUE) <- (p_TRUE) * (1 - cfr_TRUE) * gamma * E - sigma_h * I_r_TRUE

# Community infections
deriv(C_r_FALSE) <- (1 - p_hosp) * sigma_h * I_r_FALSE - sigma_r * C_r_FALSE
deriv(C_r_TRUE) <- (1 - p_hosp) * sigma_h * I_r_TRUE - sigma_r * C_r_TRUE

deriv(C_d_FALSE) <- (1 - p_hosp) * sigma_h * I_d_FALSE - sigma_d * C_d_FALSE
deriv(C_d_TRUE) <- (1 - p_hosp) * sigma_h * I_d_TRUE - sigma_d * C_d_TRUE

# Hospital infections
deriv(H_d_FALSE) <- p_hosp * sigma_h * I_d_FALSE - sigma_d * H_d_FALSE
deriv(H_d_TRUE) <- p_hosp * sigma_h * I_d_TRUE - sigma_d * H_d_TRUE

deriv(H_r_FALSE) <- p_hosp * sigma_h * I_r_FALSE - sigma_r * H_r_FALSE
deriv(H_r_TRUE) <- p_hosp * sigma_h * I_r_TRUE - sigma_r * H_r_TRUE

# Recovered
deriv(R) <- sigma_r * (H_r_FALSE + H_r_TRUE + C_r_FALSE + C_r_TRUE)

#Dead
deriv(Dead) <- sigma_d * (H_d_FALSE + H_d_TRUE + C_d_FALSE + C_d_TRUE)

### Create variables that correspond to data - daily incidence & deaths 
## reported incidence:
deriv(cumul_onset) <- p_hosp * gamma * E # cumulative incidence
# create delayed variables in order to compute non cumulative (daily) counts
cumul_onset_delayed <- delay(cumul_onset, 1) # 1 for 1 day  delay
output(daily_onset) <- cumul_onset - cumul_onset_delayed
# new cases today = cumulative count from today - cumulative count from yesterday  

## reported deaths:
deriv(cumul_reported_death_h) <- sigma_d * (H_d_TRUE+ H_d_FALSE) * p_outcome_reported # cumulative deaths
# create delayed variables in order to compute non cumulative (daily) counts
cumul_reported_death_h_delayed <- delay(cumul_reported_death_h, 1) # 1 for daily delay
output(daily_reported_death_h) <- cumul_reported_death_h - cumul_reported_death_h_delayed
# daily deaths = cumulative deaths - cumulative deaths with 1 day delay 

### useful variables to output

Infectious_Community <- I_d_FALSE + I_d_TRUE +
  I_r_FALSE + I_r_TRUE + C_d_FALSE + C_d_TRUE + 
  C_r_FALSE + C_r_TRUE
Infectious_isolated <- H_d_FALSE + H_d_TRUE + H_r_FALSE + H_r_TRUE


################################################################################
### user defined parameters
################################################################################

# Unknown Parameters:
N <- user(5e+5, min = 0) # population size with default value
I0 <- user(9.526, min = 0) # initial number of infected individuals 
R0 <- user(6.339, min = 0) # R0 (assumed the same for those who stay in community and 
# (1) recover or (2) died 
mu_h_before <- user(2.13, min = 0) # mean onset to hosp. delay before interventions

# Parameters given in handout:
p_hosp <- user(0.7, min = 0, max = 1) # proportion hospitalised
t_intervention <- user(170, min = 0) # time of interventions

# Parameters calculated from linelist:
L <- user(9.92, min = 0) # mean latency
mu_d <- user(8.0, min = 0) # time from admission to death in days
mu_r <- user(16.98, min = 0) # mean time from admission to recovery in days
p_outcome_reported <- user(0.775, min = 0, max = 1) # probability outcome of patient is reported 
mu_h_after <- user(1.46, min = 0) # mean onset to hosp. delay after interventions
mu_h <- if (t <= t_intervention) mu_h_before else mu_h_after 
p_TRUE <-  user(0.604, min = 0, max = 1) # probability patient has underlying health issue after intervention
cfr_FALSE <- user(0.453, min = 0, max = 1) # case fatality for those without underlying health conditions
cfr_TRUE <- user(0.675, min = 0, max = 1) # case fatality ratio for those with underlying health conditions

### compute other parameters from the ones above
gamma <- 1/L
sigma_h <- 1 / mu_h
sigma_d <- 1 / (mu_d - mu_h)
sigma_r <- 1 / (mu_r - mu_h)
beta_r <- R0 / mu_r
beta_d <- R0 / mu_d

### additional things to output
output(mu_h) <- TRUE


################################################################################
### initial number of individuals in each compartment
################################################################################

initial(S) <- N
initial(E) <- 0
initial(I_d_FALSE) <- I0 / 4
initial(I_r_FALSE) <- I0 / 4
initial(I_d_TRUE) <- I0 / 4
initial(I_r_TRUE) <- I0 / 4
initial(H_d_FALSE) <- 0
initial(C_d_FALSE) <- 0
initial(H_r_FALSE) <- 0
initial(C_r_FALSE) <- 0
initial(H_d_TRUE) <- 0
initial(C_d_TRUE) <- 0
initial(H_r_TRUE) <- 0
initial(C_r_TRUE) <- 0
initial(R) <- 0 
initial(Dead) <- 0
initial(cumul_onset) <- 0
initial(cumul_reported_death_h)  <- 0


