
plot_timeseries <- function(y, what = "hosp_incidence") {
  
  ylab <- if (what == "hosp_incidence") {
    "Hospital admissions"
  } else if (what == "deaths_incidence") {
    "Daily deaths"
  }
  
  y %>% 
    select(time, what) %>% 
    pivot_longer(!time) %>% 
    ggplot(aes(time, value, col = name)) +
    geom_line() +
    labs(x = "Time", y = ylab) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(axis.line = element_line(),
          legend.position = "none")
  
}
