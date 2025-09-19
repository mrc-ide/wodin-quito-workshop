
run_model <- function(mod, pars, n_days = 100, n_pars = 100) {
  
  n_steps <- n_days * 4
  
  model <- mod$new(pars, time = 0, n_particles = n_pars)
  index <- model$info()$index
  t <- seq(0, n_steps, 1)
  
  y <- model$simulate(t)
  
  y
}


summarise_state <- function(y) {
  data.frame(
    day = seq(1, nrow(y)) / 4,
    mean = rowMeans(y),
    lb = rowQuantiles(y, probs = 0.025),
    ub = rowQuantiles(y, probs = 0.975)
  )
}


plot_timeseries <- function(y, index, what = NULL) {
  
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
    
    p <- ggplot(ret, aes(x = day)) +
      geom_line(aes(y = mean, col = type)) +
      geom_ribbon(aes(ymin = lb, ymax = ub, fill = type), 
                  alpha = 0.2, show.legend = FALSE) +
      labs(x = "Time", y = "Incidence") +
      scale_color_manual(breaks = breaks, values = values) +
      scale_fill_manual(breaks = breaks, values = values) +
      annotate("text", label = "Outbreak detected", x = 24, y = 900)
    
    
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
  
  p +
    geom_vline(xintercept = 30, linetype = 2, col = "red") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(axis.line = element_line(),
          legend.position = c(0.25, 0.85),
          legend.title = element_blank())
}
