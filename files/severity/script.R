
pacman::p_load(rio, tidyverse, odin)

sir <- odin::odin("files/severity/severity_model.R")

model <- sir$new()
t <- seq(0, 365, 1)

y <- as.data.frame(model$run(t)) %>% 
  pivot_longer(!step)

ggplot(y, aes(step, value, col = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y")
