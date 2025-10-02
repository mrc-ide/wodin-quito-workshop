
pacman::p_load(simulist, tidyverse, patchwork)

set.seed(1)
outbreak_start_date <- as.Date("2025-08-19")
true_cfr <- 0.03

simul_linelist <- sim_linelist(anonymise = TRUE,
                  outbreak_start_date = outbreak_start_date,
                  outbreak_size = c(1e3, 5e3),
                  hosp_death_risk = 0.33,
                  hosp_risk = 0.1,
                  infectious_period = function(x) stats::rexp(n = x, 0.1317),
                  onset_to_death = function(x) stats::rexp(n = x, 0.024),
                  onset_to_recovery = function(x) stats::rexp(n = x, 0.1317)) %>%
  mutate(delay_outcome = ceiling(date_outcome - date_onset),
         delay_admission = ceiling(date_admission - date_onset),
         day = as.numeric(floor(date_onset - outbreak_start_date) + 1))


simul_linelist <- simul_linelist %>% 
  select(date_onset, date_admission, date_outcome, outcome) %>% 
  mutate(across(.cols = where(is.Date), .fns = lubridate::ymd)) %>% 
  mutate(delay_outcome = as.numeric(date_outcome - date_onset),
         delay_admission = as.numeric(date_admission - date_onset),
         day = as.numeric(date_onset - outbreak_start_date) + 1)


nrow(simul_linelist[!is.na(simul_linelist$date_admission), ]) / nrow(simul_linelist)


ggplot(simul_linelist) +
  geom_histogram(aes(date_admission)) + 
  facet_wrap(~outcome)


ggplot(simul_linelist) +
  geom_histogram(aes(delay_outcome)) +
  facet_wrap(~outcome, scales = "free_y")

mean(simul_linelist[simul_linelist$outcome == "recovered", "delay_outcome"])

mean(simul_linelist[simul_linelist$outcome == "recovered" & 
                      !is.na(simul_linelist$date_admission), "delay_outcome"])

mean(simul_linelist[simul_linelist$outcome == "recovered" & 
                      is.na(simul_linelist$date_admission), "delay_outcome"])

mean(simul_linelist[simul_linelist$outcome == "died", "delay_outcome"])



make_timeseries <- function(llist, cutoff = 50) {
  
  llist <- llist %>% 
    filter(day <= cutoff)
  
  dates <- data.frame(
    date_onset = seq.Date(min(llist$date_onset), max(llist$date_onset), 1))
    
  
  llist %>% 
    mutate(
      date_admission = ifelse(delay_admission > cutoff, NA_real_, date_admission),
      outcome = ifelse(delay_outcome > cutoff, NA_real_, outcome),
      date_outcome = ifelse(delay_outcome > cutoff, NA_real_, date_outcome)
    ) %>% 
    right_join(., dates, by = "date_onset") %>% 
    group_by(date_onset) %>% 
    summarise(infections = n(),
              hospitalisations = sum(!is.na(date_admission)),
              recoveries = sum(!is.na(date_admission) & outcome == "recovered"),
              deaths = sum(!is.na(date_admission) & outcome == "died")) %>% 
    replace(is.na(.), 0)
}


plot_cfr_trajectory <- function(ts, true_cfr = 0.03,
                                min_date = outbreak_start_date,
                                max_date = as.Date("2026-10-24")) {
  
  tmp <- ts %>% 
    select(date = date_onset, deaths, infections) %>% 
    mutate(deaths = cumsum(deaths),
           infections = cumsum(infections)) %>% 
    mutate(HFR = deaths / infections) 
  
  known_cfr <- round(tail(tmp$HFR, 1), 2)
  
  x_loc <- min(tmp$date) + 100
  
  p <- ggplot(tmp, aes(date, HFR)) +
    geom_line() +
    geom_hline(yintercept = known_cfr, col = "blue", linetype = 3) +
    geom_hline(yintercept = true_cfr, col = "purple", linetype = 2) +
    # annotate("text", label = paste0("Known CFR = ", known_cfr), 
    #          x = x_loc, y = known_cfr + 0.03) +
    geom_vline(xintercept = as.Date("2025-10-08"), linetype = 2, col = "red") +
    annotate("text", label = "Present day", 
             x = as.Date("2025-10-18") + 10, y = 0.75) +
    labs(x = "", y = "CFR") +
    scale_y_continuous(limits = c(0, 0.1), expand = c(0, 0), 
                       labels = scales::percent) +
    scale_x_date(date_breaks = "2 month", date_labels = "%b-%y",
                 limits = c(min_date, max_date)) +
    theme_minimal() +
    theme(axis.line = element_line())
  
  # if(!is.null(true_cfr)) {
  #   true_cfr <- round(true_cfr, 2)
  #   p <- p +
  #     geom_hline(yintercept = true_cfr, col = "purple", linetype = 2) 
  #     # annotate("text", label = paste0("True CFR = ", true_cfr), 
  #     #          x = x_loc, y = true_cfr + 0.03)
  # }
  p
}


ts_500 <- make_timeseries(simul_linelist, 500)
ts_150 <- make_timeseries(simul_linelist, 150)
ts_50 <- make_timeseries(simul_linelist, 50)


plot_cfr_trajectory(ts_50) /
  plot_cfr_trajectory(ts_500)



# Simple estimates --------------------------------------------------------

ts_cumul_50 <- ts_50 %>% 
  select(date = date_onset, everything()) %>% 
  mutate(across(.cols = where(is.numeric), .fns = cumsum))
  
survey_times <- seq.Date(as.Date("2025-09-17"), as.Date("2025-10-07"), 5)

cfr_conf <- ts_cumul_50 %>% 
  filter(date %in% survey_times) %>% 
  mutate(epitools::binom.exact(x = deaths, n = infections)) %>% 
  mutate(mean = round(proportion, 3),
         lb = round(lower, 3),
         ub = round(upper, 3)) %>% 
  select(date, mean, lb, ub)


ggplot(cfr_conf) +
  geom_pointrange(aes(x = date, y = mean, ymin = lb, ymax = ub)) +
  geom_hline(yintercept = true_cfr, col = "purple", linetype = 2) +
  labs(x = "Time", y = "CFR") +
  scale_y_continuous(limits = c(0, 0.1), expand = c(0, 0),
                     labels = scales::percent) +
  theme_minimal() +
  theme(axis.line = element_line())


# Closed cases ------------------------------------------------------------

closed_cfr <- ts_cumul_50 %>% 
  filter(date %in% survey_times) %>% 
  mutate(epitools::binom.exact(x = deaths, n = recoveries + deaths)) %>% 
  mutate(mean = round(proportion, 3),
         lb = round(lower, 3),
         ub = round(upper, 3)) %>% 
  select(date, mean, lb, ub)


compare <- closed_cfr %>% 
  mutate(type = "Deaths / (Deaths + Recoveries)") %>% 
  rbind(., 
        cfr_conf %>% 
          mutate(type = "Deaths / Hospitalisations"))


ggplot(compare) +
  geom_pointrange(aes(x = date, y = mean, ymin = lb, ymax = ub, col = type)) +
  geom_hline(yintercept = true_cfr, col = "red", linetype = 2) +
  labs(x = "Time", y = "CFR") +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_minimal() +
  theme(axis.line = element_line(),
        legend.position = c(0.7, 0.85),
        legend.title = element_blank())


# drjacoby ----------------------------------------------------------------


case_convolve <- function(x, delay) {
  ret <- rev(convolve(x = rev(x), y = delay, type = "open"))[1:length(x)]
  ret[ret < 0] <- 0
  return(ret)
}

# Note, this code uses the following specific version of drjacoby
#devtools::install_github("mrc-ide/drjacoby@v1.5.4", force = TRUE)
library(drjacoby)

# define parameters data.frame
df_params <- define_params(name = "HFR", min = 0, max = 1, init = 0.1,
                           name = "delay_death_mean", min = 0, max = Inf, init = 10,
                           name = "delay_death_sd", min = 0, max = Inf, init = 2)

# define log-likelihood function
loglike <- function(params, data, misc) {
  
  # extract parameter values
  HFR <- params["HFR"]
  delay_death_mean <- params["delay_death_mean"]
  delay_death_sd <- params["delay_death_sd"]
  
  # get incidence of death by convolution
  n <- nrow(data)
  inc_death <- 
    HFR * case_convolve(x = data$cases,
                        delay = discrete_gamma(0:100, 
                                               mean = delay_death_mean, 
                                               sd = delay_death_sd))
  
  # calculate likelihood
  ret <- sum(dpois(data$deaths, lambda = inc_death[1:n], log = TRUE))
  
  # return
  return(ret)
}

# define log-prior function
logprior <- function(params, misc) {
  
  # extract parameter values
  HFR <- params["HFR"]
  delay_death_mean <- params["delay_death_mean"]
  delay_death_sd <- params["delay_death_sd"]
  
  # calculate log-prior
  ret <- dunif(HFR, min = 0, max = 1, log = TRUE) +
    dexp(delay_death_mean, rate = 1 / 16, log = TRUE) +
    dexp(delay_death_sd, rate = 1 / 8.5, log = TRUE)
  
  # return
  return(ret)
}

discrete_gamma <- function(x, mean, sd) {
  pgamma(x, shape = mean^2 / sd^2, rate = mean / sd^2) -
    pgamma(x - 1, shape = mean^2 / sd^2, rate = mean / sd^2)
}

# run MCMC
# set.seed(1)

inc_df <- ts_150 %>% 
  select(date = date_onset,
         cases = hospitalisations,
         deaths)

mcmc <- run_mcmc(data = inc_df,
                 df_params = df_params,
                 loglike = loglike,
                 logprior = logprior,
                 burnin = 1e2,
                 samples = 1e3,
                 chains = 5,
                 silent = TRUE)

#mcmc$diagnostics
#plot_trace(mcmc, phase = "both")

# get 95% CrIs
mcmc_draws <- mcmc$output |>
  filter(phase == "sampling") |>
  select(-chain, -phase, -iteration, -logprior, -loglikelihood)

mcmc_CrI <- apply(mcmc_draws, 2, function(x) quantile(x, probs = c(0.025, 0.5, 0.975))) |>
  t() |>
  as.data.frame() |>
  rename(lower_CrI = '2.5%',
         upper_CrI = '97.5%',
         median = '50%')


mcmc_CrI %>% 
  round(digits = 3)

