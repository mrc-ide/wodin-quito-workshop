################################################################################
# Model 3 elaborates on Model 2 by introducing asymptomatic infections.
# Keep all other parameters the same but introduce a probability of being 
#  asymptomatic p_asym. Assume they recover at the same rate as those with
#  symptoms recover (mu_R). Assume they have a separate transmission 
#  coefficient (beta_asym). 
# 
#   S = # susceptible 
#   E = # latent (split in two for more realistic distributed latency)
#   A = # Asymptomatic infections - never hospitalised & always recover  
#   I_d = # early infectious - not yet hospitalised - will die
#   I_r = # early infectious - not yet hospitalised - will recover
#   H_d = # hospitalised - will die
#   H_r = # hospitalised - will recover
#   C_d = # community (non-hospitalised) - will die
#   C_r = # community (non-hospitalised) - will recover
#   R = # recovered
#   
# Fit to same parameters as model 2. 
################################################################################

################################################################################
### model dynamics
################################################################################

# Susceptible
deriv (S) <- - S/N * (beta_r * (I_r + C_r) + 
                        beta_d * (I_d + C_d) + 
                        beta_asym * A)

# Exposed (but not infectious)
deriv (E) <- S/N * (beta_r * (I_r + C_r) + 
                      beta_d * (I_d + C_d) + 
                      beta_asym * A) - gamma * E

deriv(A) <- gamma*E*p_asym - sigma_r*A

# Infected 
deriv (I_d) <- (1 - p_asym) * cfr * gamma * E - sigma_h * I_d 
deriv (I_r) <- (1 - p_asym) * (1 - cfr) * gamma * E - sigma_h * I_r

# Hospital infections
deriv(H_d) <- p_hosp * sigma_h * I_d - sigma_d * H_d
deriv(H_r) <- p_hosp * sigma_h * I_r - sigma_r * H_r

# Community infections
deriv(C_d) <- (1 - p_hosp) * sigma_h * I_d - sigma_d * C_d
deriv(C_r) <- (1 - p_hosp) * sigma_h * I_r - sigma_r * C_r

# Recovered
deriv(R) <- sigma_r * (H_r + C_r + A)

# Died
deriv(Dead) <- sigma_d * (H_d + C_d)

### Create variables that correspond to data - daily incidence & deaths 
## reported incidence:
deriv(cumul_onset) <-  (1 - p_asym) * p_hosp * gamma * E # cumulative incidence
# create delayed variables in order to compute non cumulative (daily) counts
cumul_onset_delayed <- delay(cumul_onset, 1) # 1 for 1 day  delay
output(daily_onset) <- cumul_onset - cumul_onset_delayed 
# new cases today = cumulative count from today - cumulative count from yesterday  

## reported deaths:
deriv(cumul_reported_death_h) <- sigma_d * H_d * p_outcome_reported # cumulative deaths
# create delayed variables in order to compute non cumulative (daily) counts
cumul_reported_death_h_delayed <- delay(cumul_reported_death_h, 1) # 1 for daily delay
output(daily_reported_death_h) <- cumul_reported_death_h - cumul_reported_death_h_delayed
# daily deaths = cumulative deaths - cumulative deaths with 1 day delay 


### useful variables to output

Exposed <- E
Infectious_Community <- I_d + I_r + C_d + C_r + A
output(Infectious_Community) <- TRUE
Infectious_isolated <- H_d + H_r
output(Infectious_isolated) <- TRUE

################################################################################
### user defined parameters
################################################################################

# Unknown parmaeters:
N <- user(5e+5, min = 0) # population size with default value
I0 <- user(11.146, min = 0) # initial number of infected individuals 
R0 <- user(2.026, min = 0) # R0 (assumed the same for those who stay in community and 
# (1) recover or (2) died 
mu_h_before <- user(6.858, min = 0) # mean onset to hosp. delay before interventions

# Parameters from handout:
t_intervention <- user(170, min = 0) # time of interventions
p_hosp <- user(0.7, min = 0, max = 1) # proportion hospitalised
p_asym <- user(0.025, min=0, max=1) # proportion of asymptomatic cases

# Parameter calculated from linelist:
L <- user(9.92, min = 0) # mean latency
mu_d <- user(8.0, min = 0) # time from admission to death in days
mu_r <- user(16.98, min = 0) # mean time from admission to recovery in days
cfr <- user(0.5656, min = 0, max = 1) # case fatality ratio
p_outcome_reported <- user(0.775)
mu_h_after <- user(1.46, min = 0) # mean onset to hosp. delay after interventions

### compute other parameters from the ones above
gamma <- 1/L
sigma_h <- 1 / mu_h
sigma_d <- 1 / (mu_d - mu_h)
sigma_r <- 1 / (mu_r - mu_h)
beta_r <- R0 / mu_r
beta_d <- R0 / mu_d
beta_asym <- R0 / mu_r
# get an Rt (assuming no running out of suceptible, i.e. S~N)
Rt <- cfr * beta_d * 
  (p_hosp * mu_h + (1 - p_hosp) * mu_d) + 
  (1 - cfr) * beta_r * (p_hosp * mu_h + (1 - p_hosp) * mu_r)

mu_h <- if (t <= t_intervention) mu_h_before else mu_h_after 

### additional things to output
output(mu_h) <- TRUE

################################################################################
### initial numbuer of individuals in each compartment
################################################################################
initial(S) <- N
initial(E) <- 0
initial(A) <- 0
initial(I_d) <- I0 / 2
initial(I_r) <- I0 / 2
initial(H_d) <- 0
initial(C_d) <- 0
initial(H_r) <- 0
initial(C_r) <- 0
initial(R) <- 0 
initial(Dead) <- 0
initial(cumul_onset) <- 0
initial(cumul_reported_death_h)  <- 0