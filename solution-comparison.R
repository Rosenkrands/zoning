source("2d-instance.R")
source("solution-functions.R")

calc_obj2 <- function(file) {
  clean_name <- substring(file, 1, nchar(file) - 4)
  specification <- str_split(clean_name,'_')
  
  instance_id <- paste0(specification[[1]][1],'_',specification[[1]][2])
  method <- specification[[1]][3]
  obj <- specification[[1]][4]
  no_of_centers <- specification[[1]][5]
  
  solution <- readRDS(paste0('./solution_for_simulation/',file))
  inst <- readRDS(paste0('./instances/',instance_id,'.rds'))
  
  tibble(
    file = file,
    instance = instance_id,
    method,
    obj,
    no_of_centers = no_of_centers,
    ar_var = inst$arv['max'] %>% as.numeric() - inst$arv['min'] %>% as.numeric(),
    ARV = ARV(solution),
    SAFE = SAFE(solution),
    TOT = TOT(solution),
    WCSS = WCSS(solution),
    number_of_centroids = number_of_centroids(solution)
  )
}

sol_files <- list.files('./solution_for_simulation')

result <- do.call(
  bind_rows, 
  pbapply::pblapply(sol_files %>% as.list(), calc_obj2)
) 

result <- result %>% mutate(
  `Solution method` = factor(paste0(method,':',obj),
                             levels = c("GA:ARV", "GA:SAFE", "GA:TOT", "KM:WCSS")),
  `Number of UAVs` = factor(as.numeric(no_of_centers),
                            levels = c(5, 10, 15, 20)),
  `Arrival rate variance` = factor(ar_var, 
                                   levels = c(20,50,80),
                                   labels = c("low", "medium", "high")),
) %>%
  select(-c(method, obj, no_of_centers, ar_var)) %>%
  pivot_longer(cols = c(ARV, TOT, SAFE, WCSS), 
               names_to = "Objective", values_to = "Objective value")
  
### mean/variance plot ###
# Each tile show an objective function, the color show the
# objective function used for solving and shape shows method
# Except for SAFE objective we want to be left and down, but
# for SAFE we want to be right and down
# NOTE: Would probably make sense to make one for both 3 and 6
# no_of_centers. Maybe compare them individually.
result %>%
  group_by(`Solution method`,`Number of UAVs`, `Arrival rate variance`, Objective) %>%
  summarise(across(`Objective value`, 
                   list(mean = mean, sd = sd), 
                   .names = "{.fn}")) %>%
  ggplot(aes(x = mean, y = sd, color = `Solution method`, shape = `Arrival rate variance`)) +
  geom_point() +
  facet_wrap(~Objective, scales = "free") +
  theme_bw()

### boxplot for the different objectives ###
# NOTE: Would probably make sense to make one for both 3 and 6
# no_of_centers. Maybe compare them individually.
result %>%
  filter(`Arrival rate variance` == "high", `Number of UAVs` == "low") %>%
  group_by(`Solution method`,`Number of UAVs`, `Arrival rate variance`, Objective) %>%
  ggplot(aes(x = `Solution method`, 
             y = `Objective value`,
             color = `Solution method`)) +
  geom_boxplot() +
  facet_wrap(~Objective, scales = "free") +
  theme_bw()

# Comparing GA:TOT and KMeans for TOT objective across ar_var
# We expect that GA:TOT outperforms KMeans for TOT objective
# in the high variance case
result %>%
  filter(`Solution method` %in% c("GA:TOT", "KM:WCSS")) %>%
  filter(Objective %in% c("TOT", "WCSS")) %>%
  ggplot(aes(x = `Arrival rate variance`, 
             y = `Objective value`,
             color = `Solution method`)) +
  geom_boxplot() +
  facet_wrap(`Number of UAVs`~Objective, scales = "free", nrow = 4) +
  theme_bw()

### OLD

# checking instances where ga is better than kmeans, might need to increase nstart
# now increased nstart to 1000 and still see instances where GA performs better
# per_instance <- 
#   result %>%
#   group_by(instance, method) %>%
#   slice_min(WCSS, n = 1) %>%
#   filter(row_number() == 1)
# 
# per_instance %>%
#   select(instance, method, WCSS) %>%
#   pivot_wider(id_cols = c(-WCSS), values_from = WCSS, names_from = method) %>%
#   mutate(gap = GA - KM)
#   
# ggplot(per_instance) +
#   geom_point(aes(x = instance, y = WCSS, color = method))
# 
# km_tot <- data %>% 
#   filter(method == "KM") %>%
#   ungroup() %>%
#   select(WCSS) %>%
#   as.numeric()
# 
# ggplot() +
#   geom_point(data = data %>% filter(method == "GA"),
#              aes(x = dimension, y = WCSS)) +
#   geom_line(data = data %>% filter(method == "GA"),
#              aes(x = dimension, y = WCSS)) +
#   geom_hline(yintercept = km_tot, linetype = "dashed") +
#   scale_x_continuous(breaks = seq(3,10,1), labels = seq(3,10,1)) +
#   annotate("text", x = 4.5, y = km_tot+1, size = 3.5,
#            label = "Dotted line show KMeans WCSS") +
#   theme_bw()
# 
# km_data <- result %>% filter(method == "KM")
# 
# ga_data <- result %>%
#   filter(method != "KM", miter == "run500", obj == "TOT") %>%
#   left_join(km_data %>% select(instance, TOT), 
#             by = "instance", suffix = c(".ga", ".km")) %>%
#   mutate(TOT_gap = (TOT.ga - TOT.km)*100/TOT.ga) %>%
#   group_by(method, obj, dimension, miter) %>%
#   summarise(TOT_gap = mean(TOT_gap)) %>%
#   pivot_wider(id_cols = c(method, obj, miter, dimension), 
#               names_from = dimension, values_from = TOT_gap) %>%
#   select(method, obj, miter, `3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`) %>%
#   arrange(miter)
# 
# print(xtable::xtable(ga_data, digits = c(2)), include.rownames = F)
