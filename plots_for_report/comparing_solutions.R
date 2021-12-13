library(tidyverse)

solution_results <- readRDS('./solution-results.rds') %>%
  filter(grid_dimension == 8)

## Not using all centroids ##
readRDS('./no-demand-points.rds') %>%
  filter(`Grid dimension` == 8, `Solution method` %in% c("GA:ARV", "GA:SAFE")) %>%
  select(`Solution method`, `no_of_centers`, `number_of_centroids`, `Number of UAVs`) %>%
  mutate(`Base locations with no demand points` = as.numeric(no_of_centers) - number_of_centroids) %>%
  select(-c(no_of_centers, number_of_centroids)) %>%
  ggplot() +
    geom_histogram(aes(x=`Base locations with no demand points`)) +
    facet_grid(`Solution method`~`Number of UAVs`, labeller = label_both) +
    scale_y_continuous(limits = c(0,90), n.breaks = 9) +
    theme_bw()

ggsave('./plots_for_report/base_locations_with_no_demand_points.pdf', width = 10, height = 4)
### mean/variance plot ###
# Each tile show an objective function, the color show the
# objective function used for solving and shape shows method
# Except for SAFE objective we want to be left and down, but
# for SAFE we want to be right and down
# NOTE: Would probably make sense to make one for both 3 and 6
# no_of_centers. Maybe compare them individually.
solution_results %>%
  group_by(`Solution method`,`Number of UAVs`, `Arrival rate variance`, Objective) %>%
  summarise(across(`Objective value`, 
                   list(mean = mean, sd = sd), 
                   .names = "{.fn}")) %>%
  ggplot(aes(x = mean, y = sd, color = `Solution method`, shape = `Arrival rate variance`)) +
  geom_point() +
  facet_wrap(~Objective, scales = "free") +
  theme_bw()

### Violin plot showing the distribution of objectives for ARV and SAFE ###
solution_results %>%
  # filter(`Arrival rate variance` == "high", `Number of UAVs` == "low") %>%
  filter(Objective %in% c("ARV"), `Solution method` != "KM:WCSS") %>%
  group_by(`Solution method`,`Number of UAVs`, `Arrival rate variance`, Objective) %>%
  ggplot(aes(x = `Solution method`, 
             y = `Objective value`)) +
  geom_violin(fill = "grey") +
  facet_wrap(~Objective, scales = "free") +
  scale_y_log10() +
  theme_bw() + theme(legend.position = "none")

ggsave('./plots_for_report/comparing_solutions_arv.pdf', width = 4, height = 3)

solution_results %>%
  # filter(`Arrival rate variance` == "high", `Number of UAVs` == "low") %>%
  filter(Objective %in% c("SAFE"), `Solution method` != "KM:WCSS") %>%
  group_by(`Solution method`,`Number of UAVs`, `Arrival rate variance`, Objective) %>%
  ggplot(aes(x = `Solution method`, 
             y = `Objective value`)) +
  geom_violin(fill = "grey") +
  facet_wrap(~Objective, scales = "free") +
  # scale_y_log10() +
  theme_bw() + theme(legend.position = "none")

ggsave('./plots_for_report/comparing_solutions_safe.pdf', width = 4, height = 3)

# Comparing GA:TOT and KMeans for TOT objective across ar_var
# We expect that GA:TOT outperforms KMeans for TOT objective
# in the high variance case
solution_results %>%
  filter(`Solution method` %in% c("GA:TOT", "KM:WCSS", "WKM:WWCSS")) %>%
  filter(Objective %in% c("TOT"), `Arrival rate variance` %in% c("low", "medium")) %>%
  ggplot(aes(y = `Number of UAVs`, 
             x = `Objective value`,
             fill = `Solution method`)) +
  geom_boxplot() +
  facet_wrap(~`Arrival rate variance`, scales = "free", nrow = 3, labeller = label_both) +
  theme_bw() + labs(x = "TOT Objective") + theme(legend.position = "top")

ggsave('./plots_for_report/comparing_GATOT_KMWCSS_TOT.pdf', width = 9, height = 4)
  

# Comparing GA:TOT and KMeans for TOT objective across ar_var
# We expect that GA:TOT outperforms KMeans for TOT objective
# in the high variance case
readRDS('./solution-results.rds') %>%
  filter(`Grid dimension` %in% c(8, 15)) %>%
  filter(`Solution method` %in% c("GA:TOT", "KM:WCSS", "WKM:WWCSS")) %>%
  filter(Objective %in% c("TOT"), `Arrival rate variance` == "high") %>%
  mutate(`Solution method` = factor(
    paste0(`Solution method`,":",`Grid dimension`),
    levels = c("GA:TOT:15", "GA:TOT:8", "WKM:WWCSS:8", "KM:WCSS:8"), labels = c("GA:TOT 15x15 grid", "GA:TOT 8x8 grid", "WKM:WWCSS", "KM:WCSS")
  )) %>%
  ggplot(aes(y = `Number of UAVs`, 
             x = `Objective value`,
             fill = `Solution method`)) +
  geom_boxplot() +
  facet_wrap(~`Arrival rate variance`, scales = "free", nrow = 1, labeller = label_both) +
  theme_bw() + labs(x = "TOT Objective") + theme(legend.position = "top") +
  scale_fill_manual(values = c("#00BA42", "#F8766D", "#00BFC4", "#C77CFF"))

ggsave('./plots_for_report/comparing_GATOT_KMWCSS_TOT_high_dimension.pdf', width = 9, height = 4)

# readRDS('./solution-results.rds') %>%
#   filter(`Solution method` %in% c("GA:TOT", "KM:WCSS"), `Number of UAVs` == "high") %>%
#   filter(Objective %in% c("WCSS")) %>%
#   mutate(`Solution method` = paste0(`Solution method`,":",`Grid dimension`)) %>%
#   group_by(`Arrival rate variance`, `Solution method`) %>%
#   summarise(`Mean objective` = mean(`Objective value`),
#             `Sd objective` = sd(`Objective value`)) %>%
#   ggplot(aes(x = `Arrival rate variance`, y = `Mean objective`,group = `Solution method`)) +
#     geom_ribbon(aes(ymin=`Mean objective`-`Sd objective`, ymax=`Mean objective`+`Sd objective`, fill = `Solution method`), alpha = .3) +
#     geom_point(aes(color = `Solution method`)) +
#     geom_line(aes(color = `Solution method`)) +
#     theme_bw()
# 
# ggsave('./plots_for_report/mean_objective_ga_km.pdf', width = 10, height = 2)

### TESTING SIGNIFICANCE WITH HIGH DIMENSION (not significant  p=.15)

ga_high <- readRDS('./solution-results.rds') %>%
  filter(`Grid dimension` == 15, `Solution method` == "GA:TOT")

km_high <- readRDS('./solution-results.rds') %>% 
  filter(instance %in% ga_high$instance, `Solution method` == "WKM:WWCSS")

lm_data <- bind_rows(ga_high, km_high) %>%
  filter(`Solution method` %in% c("GA:TOT", "WKM:WWCSS")) %>%
  filter(Objective %in% c("TOT")) %>%
  pivot_wider(id_cols = c(instance, `Number of UAVs`, `Arrival rate variance`, `Solution method`),
              names_from = Objective, values_from = `Objective value`)

lm.fit <- lm(TOT ~ `Solution method`,
   data = lm_data %>% filter(`Arrival rate variance` == "high", `Number of UAVs` == "high"))

summary(lm.fit)

### TESTING SIGNIFICANCE WITH LOW DIMENSION (not significant p=.51)

lm_data <- readRDS('./solution-results.rds') %>%
  filter(`Grid dimension` == 8) %>%
  filter(`Solution method` %in% c("GA:TOT", "WKM:WWCSS")) %>%
  filter(Objective %in% c("TOT")) %>%
  pivot_wider(id_cols = c(instance, `Number of UAVs`, `Arrival rate variance`, `Solution method`),
              names_from = Objective, values_from = `Objective value`)

lm.fit <- lm(TOT ~ `Solution method`,
             data = lm_data %>% filter(`Arrival rate variance` == "high", `Number of UAVs` == "high"))

summary(lm.fit)

### TESTING SIGNIFICANCE WITH LOW DIMENSION and low number of UAVs (significant p=.02)

lm_data <- readRDS('./solution-results.rds') %>%
  filter(`Grid dimension` == 8) %>%
  filter(`Solution method` %in% c("GA:TOT", "WKM:WWCSS")) %>%
  filter(Objective %in% c("TOT")) %>%
  pivot_wider(id_cols = c(instance, `Number of UAVs`, `Arrival rate variance`, `Solution method`),
              names_from = Objective, values_from = `Objective value`)

lm.fit <- lm(TOT ~ `Solution method`,
             data = lm_data %>% filter(`Arrival rate variance` == "high", `Number of UAVs` == "low"))

summary(lm.fit)

### COMPARISON OF WKM AND KM

# solution_results %>%
  
