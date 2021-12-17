library(tidyverse)

simulation_results <- readRDS("./simulation-results.rds")


simulation_results %>%
  filter(`Number of UAVs` == "low", `Solution method` == "GA:TOT", `Arrival rate variance` == "high") %>%
  select(instance, utilization) %>%
  unnest(cols = utilization) %>%
  unnest(cols = utilization) %>%
  filter(time %% 60 == 0) %>%
  ggplot(aes(x = time/(60*60), y = inUse/time)) +
    geom_line(aes(group = instance), alpha = .5) +
    # tidyquant::geom_ma(n=30) +
    geom_vline(xintercept = 3600/(60*60), linetype="dashed") +
    theme_bw() + labs(x = "Runtime in hours", y = "Average UAV utilization")

ggsave('./plots_for_report/uav_utilization.pdf', width = 9, height = 3)

# bind_rows(
#   sim_result_free$log[1][[1]] %>% mutate(flight = "free"),
#   sim_result_zoned$log[1][[1]] %>% mutate(flight = "zoned"),
# ) %>%
#   select(id, status, time, flight) %>%
#   mutate(inUse = ifelse(status != "IDLE", 1, 0)) %>%
#   group_by(flight, time) %>%
#   summarise(inUse = mean(inUse)) %>%
#   mutate(inUse = cumsum(inUse)) %>%
#   ggplot(aes(x = time, y = inUse/time, color = flight)) +
#   geom_line() +
#   # tidyquant::geom_ma(n=30) +
#   geom_hline(yintercept = 1, linetype="dashed") +
#   theme_bw()