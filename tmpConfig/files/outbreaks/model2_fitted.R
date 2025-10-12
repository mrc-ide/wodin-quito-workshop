################################################################################
# Model 2 elaborates on Model 1 by introducing hospitalisation.
# A proportion of newly infectious are allowed to seek care, and hospitalisation occurs
# on average after a given delay. Once hospitalised, individuals are assumed to be isolated and
# therefore do not contribute to onward transmission.
# 
# Rather than changing the Reproduction number, the delay from symptom onset to hospitalisation
# is allowed to decrease before the intervention (on day 170).
# 
# The Exposed compartment has also been divided into 2 to better represent the Gamma distribution
# observed for the incubation period.
# 
#   S = # susceptible 
#   E = # latent (split in two for more realistic distributed latency)
#   I_d = # early infectious - not yet hospitalised - will die
#   I_r = # early infectious - not yet hospitalised - will recover
#   H_d = # hospitalised - will die
#   H_r = # hospitalised - will recover
#   C_d = # community (non-hospitalised) - will die
#   C_r = # community (non-hospitalised) - will recover
#   R = # recovered
#   
#   Using the curve fitting option, the initial number of cases (seeding the infection), the basic reproduction number 
# and the delay from onset to hospitalisation (pre-intervention)can be optimised to fit both the weekly incidence curve of onset of symptoms and the 
# weekly incidence curve of death.
################################################################################

################################################################################
### model dynamics
################################################################################

deriv (S) <- - S/N * (beta_r * (I_r + C_r) + 
                        beta_d * (I_d + C_d))

deriv (E) <- S/N * (beta_r * I_r + 
                      beta_d * I_d) - 
  gamma * E


deriv (I_d) <- cfr * gamma * E - sigma_h * I_d 
deriv (I_r) <- (1 - cfr) * gamma * E - sigma_h * I_r

deriv(H_d) <- p_hosp * sigma_h * I_d - sigma_d * H_d
deriv(C_d) <- (1 - p_hosp) * sigma_h * I_d - sigma_d * C_d

deriv(H_r) <- p_hosp * sigma_h * I_r - sigma_r * H_r
deriv(C_r) <- (1 - p_hosp) * sigma_h * I_r - sigma_r * C_r

deriv(R) <- sigma_r * (H_r + C_r)
deriv(Dead) <- sigma_d * (H_d + C_d)

### Create variables that correspond to data - daily incidence & deaths 
## incidence:
deriv(cumul_onset) <-  p_hosp * gamma * E # cumulative incidence
# create delayed variables in order to compute non cumulative (daily) counts
cumul_onset_delayed <- delay(cumul_onset, 1) # 1 for 1 day  delay
output(daily_onset) <- cumul_onset - cumul_onset_delayed 
# new cases today = cumulative count from today - cumulative count from yesterday  

## deaths:
deriv(cumul_reported_death_h) <- sigma_d * H_d * p_outcome_reported # cumulative deaths
# create delayed variables in order to compute non cumulative (daily) counts
cumul_reported_death_h_delayed <- delay(cumul_reported_death_h, 1) # 1 for daily delay
output(daily_reported_death_h) <- cumul_reported_death_h - cumul_reported_death_h_delayed
# daily deaths = cumulative deaths - cumulative deaths with 1 day delay 

### useful variables to output
Infectious_Community <- I_d + I_r + C_d + C_r
Infectious_isolated <- H_d + H_r


################################################################################
### user defined parameters
################################################################################

# Unknown Parameters:
N <- user(5e+5, min = 0) # population size with default value
I0 <- user(9.284, min = 0) # initial number of infected individuals 
R0 <- user(6.434, min = 0) # R0 (assumed the same for those who stay in community and 
# (1) recover or (2) died 
mu_h_before <- user(2.132, min = 0) # mean onset to hosp. delay before interventions

# Parameters given in handout:
p_hosp <- user(0.7, min = 0, max = 1) # proportion hospitalised
t_intervention <- user(170, min = 0) # time of interventions

# Parameters calculated from linelist:
L <- user(9.92, min = 0) # mean latency
mu_d <- user(8.0, min = 0) # mean time from admission to death in days
mu_r <- user(16.98, min = 0) # mean time from admission to recovery in days
cfr <- user(0.5656, min = 0, max = 1) # case fatality ratio
p_outcome_reported <- user(0.775, min = 0, max = 1) # probability outcome of patient is reported 
mu_h_after <- user(1.46, min = 0) # mean onset to hosp. delay after interventions
mu_h <- if (t <= t_intervention) mu_h_before else mu_h_after 

### compute other parameters from the ones above
gamma <- 1/L # rate at which individuals become infectious
sigma_h <- 1 / mu_h # rate at which individuals are hospitalised 
sigma_d <- 1 / (mu_d - mu_h) # rate at which hospitalised/community individuals die 
sigma_r <- 1 / (mu_r - mu_h) # rate at which hospitalised/community individuals recover 
beta_r <- R0 / mu_r # transmission coefficient from those who recover 
beta_d <- R0 / mu_d # transmission coefficient from those who die

# get an Rt (assuming no running out of susceptible, i.e. S~N)
Rt <- cfr * beta_d * 
  (p_hosp * mu_h + (1 - p_hosp) * mu_d) + 
  (1 - cfr) * beta_r * (p_hosp * mu_h + (1 - p_hosp) * mu_r)

### additional things to output
output(mu_h) <- TRUE


################################################################################
### initial numbuer of individuals in each compartment
################################################################################

initial(S) <- N
initial(E) <- 0
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
