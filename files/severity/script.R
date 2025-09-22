source("files/severity/support.R")
pacman::p_load(rio,            # import/export files
               tidyverse,      # data cleaning, wrangling, plotting, etc. tools
               patchwork,      # multi-panel plotting tools
               matrixStats,    # stats on matrix object tools
               epitools,       # epi analysis tools
               odin.dust,      # stochastic state-space models DSL
               mcstate)        # fitting tools for odin models


# Basic estimator with all data -------------------------------------------

data <- rio::import("files/severity/incidence_data.csv")

plot_data(data, all_data = TRUE)

basic_cfr <- data %>% 
  select(day, deaths, hospitalisations) %>% 
  mutate(deaths = cumsum(deaths),
         hospitalisations = cumsum(hospitalisations)) %>% 
  mutate(CFR = deaths / hospitalisations) 

true_cfr <- tail(basic_cfr$CFR, 1)
true_cfr

plot(CFR ~ day, basic_cfr, type = "b", bty = "n", ylim = c(0, 0.5))
abline(h = true_cfr, col = "red", lty = 3)

#----


# Basic estimator, day 30 data --------------------------------------------

data <- rio::import("files/severity/incidence_data_day30.csv")

head(data, 15)

n_hosp <- sum(data$hospitalisations)
n_death <- sum(data$deaths)
n_recov <- sum(data$recoveries)
n_open <- n_hosp - n_recov - n_death

n_open / n_hosp


basic_cfr <- data %>% 
  select(day, deaths, hospitalisations) %>% 
  mutate(deaths = cumsum(deaths),
         hospitalisations = cumsum(hospitalisations)) %>% 
  mutate(CFR = deaths / hospitalisations) 

plot(CFR ~ day, basic_cfr, type = "b", bty = "n", ylim = c(0, 0.5))
abline(h = true_cfr, col = "red", lty = 3)

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
  geom_hline(yintercept = true_cfr, col = "red", linetype = 2) +
  labs(x = "Time", y = "CFR") +
  scale_y_continuous(limits = c(0, 0.5), expand = c(0,0)) +
  theme_minimal() +
  theme(axis.line = element_line())

#----


# Closed cases basic estimator --------------------------------------------

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
  geom_hline(yintercept = true_cfr, col = "red", linetype = 2) +
  labs(x = "Time", y = "CFR") +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_minimal() +
  theme(axis.line = element_line())


compare <- closed_cfr %>% 
  mutate(type = "Deaths / (Deaths + Recoveries)") %>% 
  rbind(., 
        cfr_conf %>% 
          mutate(type = "Deaths / Cases"))


ggplot(compare) +
  geom_pointrange(aes(x = day, y = mean, ymin = lb, ymax = ub, col = type)) +
  geom_hline(yintercept = true_cfr, col = "red", linetype = 2) +
  labs(x = "Time", y = "CFR") +
  scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
  theme_minimal() +
  theme(axis.line = element_line(),
        legend.position = c(0.7, 0.85),
        legend.title = element_blank())

#----


# Kaplan-Meier method, full linelist ----------------------------------------

linelist <- rio::import("files/severity/simulated_linelist.csv")
head(arrange(linelist, id), 20)

sum(linelist$outcome == "")
sum(linelist$outcome == "") / nrow(linelist)


hist(linelist$delay_recovery)
abline(v = mean(linelist$delay_recovery, na.rm = TRUE), col = "red")

hist(linelist$delay_death)
abline(v = mean(linelist$delay_death, na.rm = TRUE), col = "red")


df_km <- get_km_df(linelist, xlim = 350)


plot_km(df_km)

#----


# Kaplan-Meier method, day 30 linelist ------------------------------------

linelist <- rio::import("files/severity/simulated_linelist_day30.csv")
head(arrange(linelist, id), 20)

sum(linelist$outcome == "")
sum(linelist$outcome == "") / nrow(linelist)


hist(linelist$delay_recovery)
abline(v = mean(linelist$delay_recovery, na.rm = TRUE), col = "red")

hist(linelist$delay_death)
abline(v = mean(linelist$delay_death, na.rm = TRUE), col = "red")


df_km <- get_km_df(linelist, xlim = 30)

plot_km(df_km)

#----


# Delays with time-series data --------------------------------------------

library(cfr)

ts_data <- rio::import("files/severity/incidence_data.csv") %>% 
  select(day, hospitalisations, deaths)

plot_data(ts_data, all_data = TRUE)

discrete_gamma <- function(x, mean, sd) {
  pgamma(x, shape = mean^2 / sd^2, rate = mean / sd^2) -
    pgamma(x - 1, shape = mean^2 / sd^2, rate = mean / sd^2)
}


mean_delay <- mean(c(linelist$delay_recovery, 
                     linelist$delay_death), na.rm = TRUE)

sd_delay <- sd(c(linelist$delay_recovery, 
                 linelist$delay_death), na.rm = TRUE)

data.frame(x = 1:50) |>
  mutate(y = discrete_gamma(x, mean = mean_delay, sd = sd_delay)) |>
  ggplot() + theme_bw() +
  geom_bar(aes(x = x, y = y), stat = "identity") +
  xlab("Time (days)") + ylab("Probability of death")



# reformat data for cfr package
inc_df <- ts_data %>% 
  select(date = day,
         cases = hospitalisations,
         deaths) %>% 
  mutate(date = as.Date("2024-12-31") + date)

# estimate CFR accounting for delays
cfr_static(data = inc_df,
           delay_density = function(x) 
             discrete_gamma(x, mean = 11.52, sd = 7.69))
true_cfr

#----


# Compare cfr_static to crude estimate ------------------------------------

# estimate CFR using cfr_static() from day 20 onwards
df_cfr <- mapply(function(i) {
  cfr_static(data = inc_df[1:i,],
             delay_density = function(x) discrete_gamma(x, 
                                                        mean = mean_delay, 
                                                        sd = sd_delay)) %>% 
    suppressWarnings() %>% 
    suppressMessages()}, 
  20:nrow(inc_df), SIMPLIFY = FALSE) %>% 
  bind_rows() %>% 
  mutate(date = inc_df$date[20:nrow(inc_df)], .before = 1)

# crude estimate
CFR_df <- inc_df %>% 
  mutate(cfr_naive = cumsum(deaths) / cumsum(cases)) %>% 
  select(date, cfr_naive)

df_cfr %>% 
  left_join(CFR_df) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = cfr_naive, col = "Crude estimate")) +
  geom_line(aes(y = severity_estimate, col = "cfr_static()")) +
  geom_hline(yintercept = true_cfr, col = "red", linetype = 2) +
  geom_ribbon(aes(ymin = severity_low, ymax = severity_high), 
              fill = "green4", alpha = 0.2) +
  scale_color_manual(labels = c("cfr_static()", "Crude estimate"),
                     values = c("green4", "blue4")) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = "Date", y = "CFR") +
  theme_minimal() +
  theme(axis.line = element_line(),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.85))

#----


# SIR model ---------------------------------------------------------------

sir <- odin.dust::odin_dust("files/severity/severity_model.R")

res <- run_model(sir, list(), n_days = 150)

# generate_data(res)

plot_timeseries(res)

plot_timeseries(res, "hosp_incidence") /
  plot_timeseries(res, "deaths_incidence")


#----



# SIR deterministic -------------------------------------------------------

sir_det <- odin::odin("files/severity/severity_model_deterministic.R")

mod <- sir_det()
y <- as.data.frame(mod$run(seq(0, 150, 1)))


