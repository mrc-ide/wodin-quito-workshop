
pacman::p_load(rio, tidyverse, odin)
source("support.R")

sir <- odin::odin("files/severity/severity_model.R")

model <- sir$new()
t <- seq(0, 200, 1)

y <- as.data.frame(model$run(t)) %>% 
  mutate(step = step / 4) %>% 
  rename(time = step)

plot_timeseries(y)

y %>% 
  pivot_longer(!time) %>% 
  ggplot(aes(time, value, col = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y")



y %>% 
  select(!step) %>% 
  rowSums() %>% 
  plot()
