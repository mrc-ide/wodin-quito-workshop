################################################################################
# A basic SEIR model, allowing average time between onset and death and onset and recovery.
# The transmissibility is allowed to change following intervention (day 170).
# 
#   S = # susceptible 
#   E = # latent
#   I_D = # infectious - will die
#   I_R = # infectious - will recover
#   R = # recovered
#   D = # dead
#   
#   Fit the model by calibrating the following parameters:
#     - the initial number of cases (seeding the infection - I0) 
#     - the reproduction number before and after intervention (R0_before & R0_after)
#   Fit both the weekly incidence curve of onset of symptoms and the 
# weekly incidence curve of death.
################################################################################

################################################################################
### model dynamics
################################################################################

deriv (S) <- - S/N * (beta_r * I_r + 
                        beta_d * I_d)

deriv (E) <- S/N * (beta_r * I_r + 
                      beta_d * I_d) - 
  gamma * E

deriv (I_d) <- cfr * gamma * E - sigma_d * I_d 
deriv (I_r) <- (1 - cfr) * gamma * E - sigma_r * I_r

deriv(R) <- sigma_r * I_r
deriv(Dead) <- sigma_d * I_d

### Create variables that correspond to data - daily incidence & deaths 
# incidence:
deriv(cumul_onset) <-  gamma * E # cumulative incidence
## create delayed variables in order to compute non cumulative (daily) counts
cumul_onset_delayed <- delay(cumul_onset, 1) # 1 for 1 day  delay
output(daily_onset) <- cumul_onset - cumul_onset_delayed 
# new cases today = cumulative count from today - cumulative count from yesterday  

# deaths:
cumul_reported_death_h <- Dead * p_outcome_reported # cumulative deaths
## create delayed variables in order to compute non cumulative (daily) counts
cumul_reported_death_h_delayed <- delay(cumul_reported_death_h, 1) # 1 for daily delay
output(daily_reported_death_h) <- cumul_reported_death_h - cumul_reported_death_h_delayed
# daily deaths = cumulative deaths - cumulative deaths with 1 day delay 

### useful variables to output

Exposed <- E
Infectious <- I_d + I_r 

################################################################################
### user defined parameters
################################################################################

# Unknown parameters:
N <- user(5e+5, min = 0) # population size with default value
I0 <- user(1.356, min = 0) # initial number of infected individuals
R0_before <- user(1.972, min = 0) # R0 (assumed the same for those who stay in community and 
# (1) recover or (2) died 
R0_after <- user(0.809, min = 0)
R0 <- if (t <= t_intervention) R0_before else R0_after 

# Parameters given in handout:
t_intervention <- user(170, min = 0) # time of interventions

# Parameters calculated from linelist:
L <- user(9.92, min = 0) # mean latency
mu_d <- user(8.0, min = 0) # mean time from onset to death in days
mu_r <- user(16.98, min = 0) # mean time from onset to recovery in days
cfr <- user(0.5656, min = 0, max = 1) # case fatality ratio
p_outcome_reported <- user(0.775, min = 0, max = 1) # probability patient outcome is reported

### compute other parameters from the ones above
gamma <- 1/L # rate at which individuals become infectious 
sigma_d <- 1 / mu_d # rate at which individuals die
sigma_r <- 1 / mu_r # rate at which individuals recover 
beta_r <- R0 / mu_r # transmission coefficient from individuals that recover 
beta_d <- R0 / mu_d # transmission coefficient from individuals that recover 

### additional things to output
output(R0) <- TRUE

################################################################################
### initial numbuer of individuals in each compartment
################################################################################

initial(S) <- N # assuming no running out of susceptible (S~N)
initial(E) <- 0
initial(I_d) <- I0 / 2
initial(I_r) <- I0 / 2
initial(R) <- 0 
initial(Dead) <- 0
initial(cumul_onset) <- 0



