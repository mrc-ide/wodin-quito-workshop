
pacman::p_load(rio, tidyverse, odin)

sir <- odin::odin("files/severity/severity_model.R")

model <- sir$new()
t <- seq(0, 400, 1)

y <- as.data.frame(model$run(t)) %>% 
  mutate(step = step / 4) %>% 
  pivot_longer(!step)

ggplot(y, aes(step, value, col = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y")
