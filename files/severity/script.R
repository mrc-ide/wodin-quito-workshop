
pacman::p_load(rio, tidyverse, matrixStats, odin.dust, mcstate)
source("files/severity/support.R")

sir <- odin.dust::odin_dust("files/severity/severity_model.R")

pars <- list(
  beta = 0.8,       # Transmission rate
  p_sev = 0.3,      # Probability of severe disease if infected
  sigma_Is = 0.048, # Recovery rate from severe disease
  sigma_Ia = 0.141, # Recovery rate from mild disease
  mu = 0.048,       # Mortality rate from severe disease
  N = 10000,        # Total population
  I_init = 10       # Initial infected
)

model <- sir$new(pars, time = 0, n_particles = 10)
index <- model$info()$index
t <- seq(0, 200, 1)

y <- model$simulate(t)

plot_timeseries(y, index, "hosp_incidence")

