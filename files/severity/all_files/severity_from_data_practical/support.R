
apply_theme <- function(p, xlim = NA, ylim = NA) {
  
  p +
    scale_x_continuous(expand = c(0, 0), limits = c(0, xlim)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, ylim)) +
    theme_minimal() +
    theme(axis.line = element_line())
}


plot_data <- function(data, all_data = FALSE) {

  xlim <- ifelse(all_data, max(data$day), 30)
  
  data <- data %>% 
    pivot_longer(!day) %>% 
    filter(day <= xlim) %>% 
    mutate(name = str_to_title(name))
  
  ylim <- max(data$value) * 1.10
  
  data$name <- factor(data$name, 
                      levels = c("Infections", "Hospitalisations", 
                                 "Recoveries", "Deaths"))
  
  p <- ggplot(data, aes(day, value, col = name)) +
    geom_point() 
  
  p <- apply_theme(p, xlim, ylim)
  
  suppressWarnings(
    suppressMessages(
      p +
        labs(x = "Time", y = "Incidence data") +
        geom_vline(xintercept = 20, linetype = 2, col = "red") +
        annotate("text", label = "Outbreak detected", x = 20, y = 6000) +
        theme(legend.title = element_blank(),
              legend.position = c(0.15, 0.85))
    )
  )
}


run_model <- function(mod, pars, n_days = 100, n_pars = 100) {
  
  n_steps <- n_days * 4
  
  model <- mod$new(pars, time = 0, n_particles = n_pars)
  index <- model$info()$index
  t <- seq(0, n_steps, 1)
  
  y <- model$simulate(t)
  
  list(
    state = y,
    index = index,
    steps = t,
    time = t / 4
  )
}


summarise_state <- function(y) {
  data.frame(
    day = seq(1, nrow(y)) / 4,
    mean = rowMeans(y),
    lb = rowQuantiles(y, probs = 0.025),
    ub = rowQuantiles(y, probs = 0.975)
  )
}


plot_timeseries <- function(res, what = NULL, data = NULL) {
  
  y <- res$state
  index <- res$index
  
  stopifnot(what %in% names(index))
  
  ylab <- if (is.null(what)) {
    NULL
  } else if (what == "hosp_incidence") {
    "Hospital admissions"
  } else if (what == "deaths_incidence") {
    "Daily deaths"
  }

  if (is.null(ylab)) {
    
    breaks <- c("Model hospitalisations", "Model Deaths", "Data")
    values <- c("green3", "blue", "black")
    
    hosp <- t(y[index[["hosp_incidence"]], , ])
    deaths <- t(y[index[["deaths_incidence"]], , ])
    
    hosp <- summarise_state(hosp) %>% 
      mutate(type = "Model hospitalisations")
    deaths <- summarise_state(deaths) %>% 
      mutate(type = "Model Deaths")
    
    ret <- rbind(hosp, deaths)
    
    if(!is.null(data)) {
      ret <- data %>%
        rename(mean = data) %>% 
        mutate(day = as.double(day),
               lb = NA_real_,
               ub = NA_real_,
               type = "Data") %>% 
        rbind(ret)
    }
    
    p <- ggplot(ret, aes(x = day)) +
      geom_line(aes(y = mean, col = type)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = type), 
                  alpha = 0.2, show.legend = FALSE) +
      labs(x = "Time", y = "Incidence") +
      scale_color_manual(breaks = breaks, values = values) +
      scale_fill_manual(breaks = breaks, values = values) +
      annotate("text", label = "Outbreak detected", x = 20, y = 1000)
    
    
  } else {
    
    breaks <- c("Model", "Data")
    values <- c("green3", "black")
    
    tmp <- t(y[index[[what]], , ])
    ret <- summarise_state(tmp)
    
    p <- ggplot(ret, aes(x = day)) +
      geom_line(aes(y = mean, col = "Model")) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = "Model"), 
                  alpha = 0.2, show.legend = FALSE) +
      labs(x = "Time", y = ylab) +
      scale_color_manual(breaks = breaks, values = values) +
      scale_fill_manual(breaks = c("Model", "Data"),
                        values = c("green4", "black"))
  }
  
  p <- apply_theme(p)
  
  p +
    geom_vline(xintercept = 20, linetype = 2, col = "red") +
    theme(legend.position = c(0.85, 0.85),
          legend.title = element_blank())
}


generate_data <- function(res) {
  
  i_inf <- res$index$inf_incidence
  i_hosp <- res$index$hosp_incidence
  i_deaths <- res$index$deaths_incidence
  i_h_recov <- res$index$h_recov_incidence
  
  df <- data.frame(
    day = res$time,
    infections = round(rowMeans(t(res$state[i_inf, , ]))),
    hospitalisations = round(rowMeans(t(res$state[i_hosp, , ]))),
    recoveries = round(rowMeans(t(res$state[i_h_recov, , ]))),
    deaths = round(rowMeans(t(res$state[i_deaths, , ])))
  ) %>%
    filter(day %in% seq(1, max(res$time)))
  
  df_day30 <- df %>% 
    filter(day <= 30)
  
  rio::export(df_day30, "files/severity/incidence_data_day30.csv")
  
  rio::export(df, "files/severity/incidence_data.csv")
}


generate_linelist <- function(data) {
  
  n_hosp <- sum(data$hospitalisations)
  n_death <- sum(data$deaths)
  n_recov <- sum(data$recoveries)
  n_open <- n_hosp - n_recov - n_death
  
  delay_recov <- rexp(n_recov, 0.048)
  delay_death <- rexp(n_death, 0.024)
  
  sim_linelist <- 
    data.frame(
      id = paste0("id_", sample(1:n_hosp, replace = FALSE)),
      outcome = c(rep("death", n_death), 
                  rep("recovery", n_recov),
                  rep(NA_real_, n_hosp - n_recov - n_death))
    ) %>% 
    mutate(
      delay_recovery = c(rep(NA_real_, n_death), 
                         delay_recov,
                         rep(NA_real_, n_open)),
      delay_death = c(delay_death,
                      rep(NA_real_, n_open + n_recov))
    )
  
  rio::export(sim_linelist, "files/severity_simulated_linelist.csv")
}


generate_linelist_day30 <- function(...) {
  
  data <- rio::import("files/severity/incidence_data_day30.csv")
  df <- rio::import("files/severity/simulated_linelist.csv")
  
  n_hosp <- sum(data$hospitalisations)
  n_death <- sum(data$deaths)
  n_recov <- sum(data$recoveries)
  n_open <- n_hosp - n_recov - n_death
  
  
  x <- df[df$outcome == "recovery" & df$delay_recovery <= 30, ]
  x1 <- x[1:n_recov, ] %>% 
    rbind(.,
          (x[(n_recov + 1):(n_recov + floor(n_open / 2)), ] %>% 
             mutate(
               outcome = "",
               delay_recovery = NA_real_))
    )
  
  y <- df[df$outcome == "death" & df$delay_death <= 30, ]
  y1 <- y[1:n_death, ] %>% 
    rbind(.,
          (y[(n_death + 1):(n_death + ceiling(n_open / 2)), ] %>% 
             mutate(
               outcome = "",
               delay_death = NA_real_))
    )
  
  
  out <-  rbind(x1, y1)
  
  rio::export(out, "files/severity/simulated_linelist_day30.csv")
}


get_km_df <- function(linelist, xlim = 350){
  
  # get a vector of delays for closed recoveries and deaths
  closed_recovery <- linelist %>% 
    filter(!is.na(delay_recovery)) %>% 
    pull(delay_recovery)
  
  closed_death <- linelist %>% 
    filter(!is.na(delay_death)) %>% 
    pull(delay_death)
  
  
  n <- nrow(linelist)
  
  data.frame(day = 0:xlim) %>%
    mutate(
      n_recovered = 
        mapply(function(x) sum(closed_recovery <= x), day),
      n_dead = 
        mapply(function(x) sum(closed_death <= x), day),
      prop1 = 1 - n_recovered / n,
      prop2 = n_dead / n
    )
}


plot_km <- function(df_km, true_cfr = 0.3331771) {
  
  df_km %>% 
    select(day, prop1, prop2) %>% 
    pivot_longer(cols = -day) %>% 
    ggplot() + 
    geom_step(aes(x = day, y = value, col = name)) +
    geom_hline(yintercept = true_cfr, linetype = 2, col = "red") +
    labs(x = "Time from admission to outcome", y = "HFR") +
    scale_y_continuous(limits = c(0, 1), 
                       breaks = seq(0, 1, 0.1),
                       expand = c(0, 0)) +
    scale_color_manual(values = c("chartreuse3", "purple"),
                       name = "", 
                       labels = c("Survivorship to\nrecovery", 
                                  "\nOne minus\nsurvivorship to\ndeath")) +
    theme_minimal() +
    theme(axis.line = element_line(),
          legend.position = c(0.85, 0.5))
}
