
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
  
  ylab <- if (what == "hosp_incidence") {
    "Hospital admissions"
  } else if (what == "deaths_incidence") {
    "Daily deaths"
  }

  tmp <- t(y[index[[what]], , ])
  
  ret <- summarise_state(tmp)
    
  ggplot(ret, aes(x = day)) +
    geom_line(aes(y = mean, col = "Model")) +
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = "Model"), 
                alpha = 0.2, show.legend = FALSE) +
    labs(x = "Time", y = ylab) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_color_manual(breaks = c("Model", "Data"),
                       values = c("green4", "red")) +
    scale_fill_manual(breaks = c("Model", "Data"),
                      values = c("green4", "red")) +
    theme_minimal() +
    theme(axis.line = element_line(),
          legend.position = c(0.85, 0.85),
          legend.title = element_blank())
  
}
