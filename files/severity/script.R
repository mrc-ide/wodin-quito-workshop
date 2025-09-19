source("files/severity/support.R")
pacman::p_load(rio,            # import/export files
               tidyverse,      # data cleaning, wrangling, plotting, etc. tools
               patchwork,      # multi-panel plotting tools
               matrixStats,    # stats on matrix object tools
               odin.dust,      # stochastic state-space models DSL
               mcstate)        # fitting tools for odin models


sir <- odin.dust::odin_dust("files/severity/severity_model.R")

pars <- list(
  beta = 0.38,      # Transmission rate
  p_sev = 0.3,      # Probability of severe disease if infected
  sigma_Is = 0.048, # Recovery rate from severe disease
  sigma_Ia = 0.141, # Recovery rate from mild disease
  mu = 0.034,       # Mortality rate from severe disease
  N = 1e6,          # Total population
  I_init = 10       # Initial infected
)

y <- run_model(sir, pars, n_days = 40)

plot_timeseries(y, index)

plot_timeseries(y, index, "hosp_incidence") /
  plot_timeseries(y, index, "deaths_incidence")


