source("files/severity/support.R")
pacman::p_load(rio,            # import/export files
               tidyverse,      # data cleaning, wrangling, plotting, etc. tools
               patchwork,      # multi-panel plotting tools
               matrixStats,    # stats on matrix object tools
               epitools,       # epi analysis tools
               odin.dust,      # stochastic state-space models DSL
               mcstate)        # fitting tools for odin models


# Basic estimators --------------------------------------------------------

data <- rio::import("files/severity/incidence_data.csv")

plot_data(data)

basic_cfr <- data %>% 
  select(day, deaths, hospitalisations) %>% 
  mutate(deaths = cumsum(deaths),
         hospitalisations = cumsum(hospitalisations)) %>% 
  mutate(CFR = deaths / hospitalisations) 

plot(CFR ~ day, basic_cfr, type = "b", bty = "n")


survey_times <- seq(20, 30, 5)

cfr_conf <- basic_cfr %>% 
  select(day, deaths, hospitalisations) %>% 
  filter(day %in% survey_times) %>% 
  mutate(epitools::binom.exact(x = deaths, n = hospitalisations)) %>% 
  mutate(mean = round(proportion, 3),
         lb = round(lower, 3),
         ub = round(upper, 3)) %>% 
  select(day, mean, lb, ub)

ggplot(cfr_conf) +
  geom_pointrange(aes(x = day, y = mean, ymin = lb, ymax = ub)) +
  labs(x = "Time", y = "CFR") +
  scale_y_continuous(limits = c(0, NA), expand = c(0,0))


closed_cfr <- data %>% 
  select(day, deaths, hospitalisations, recoveries) %>% 
  filter(day %in% survey_times) %>% 
  mutate(deaths = cumsum(deaths),
         hospitalisations = cumsum(hospitalisations),
         recoveries = cumsum(recoveries)) %>% 
  mutate(epitools::binom.exact(x = deaths, 
                               n = recoveries + deaths)) %>% 
  mutate(mean = round(proportion, 3),
         lb = round(lower, 3),
         ub = round(upper, 3)) %>% 
  select(day, mean, lb, ub)


ggplot(closed_cfr) +
  geom_pointrange(aes(x = day, y = mean, ymin = lb, ymax = ub)) +
  labs(x = "Time", y = "CFR") +
  scale_y_continuous(limits = c(0, NA), expand = c(0,0))


compare <- closed_cfr %>% 
  mutate(type = "Deaths / (Deaths + Recoveries)") %>% 
  rbind(., 
        cfr_conf %>% 
          mutate(type = "Deaths / Cases"))


ggplot(compare) +
  geom_pointrange(aes(x = day, y = mean, ymin = lb, ymax = ub, col = type)) +
  labs(x = "Time", y = "CFR") +
  scale_y_continuous(limits = c(0, NA), expand = c(0,0))




#----


# SIR model ---------------------------------------------------------------

sir <- odin.dust::odin_dust("files/severity/severity_model.R")

pars <- list(
  beta = 0.38,      # Transmission rate
  N = 1e6           # Total population
)

res <- run_model(sir, pars, n_days = 100)


# generate_data(res)

plot_timeseries(res)

plot_timeseries(res, "hosp_incidence") /
  plot_timeseries(res, "deaths_incidence")

