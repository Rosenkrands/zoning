source("2d-instance.R")
source("solution-functions.R")

sol_files <- list.files('./dimension_tuning')

result <- do.call(
  bind_rows, 
  pbapply::pblapply(sol_files %>% as.list(), calc_obj)
)

# Clean results to reflect correct number of iterations
result_clean <- result %>%
  mutate(miter = replace_na(miter, 100))

data <- result_clean %>%
  group_by(method,obj,dimension,miter) %>%
  summarise(across(c(ARV, TOT, WCSS), mean)) %>%
  mutate(dimension = as.numeric(dimension))

# checking instances where ga is better than kmeans, might need to increase nstart
per_instance <- 
  result_clean %>%
  group_by(instance, method) %>%
  slice_min(TOT, n = 1) %>%
  filter(row_number() == 1)

per_instance %>%
  select(instance, method, TOT) %>%
  pivot_wider(id_cols = c(-TOT), values_from = TOT, names_from = method) %>%
  mutate(gap = GA - KM)
  
ggplot(per_instance) +
  geom_point(aes(x = instance, y = TOT, color = method), position = "dodge")

km_tot <- data %>% 
  filter(method == "KM") %>%
  ungroup() %>%
  select(TOT) %>%
  as.numeric()

ggplot() +
  geom_point(data = data %>% filter(method == "GA"),
             aes(x = dimension, y = TOT, color=miter)) +
  geom_line(data = data %>% filter(method == "GA"),
             aes(x = dimension, y = TOT, color=miter)) +
  geom_hline(yintercept = km_tot, linetype = "dashed") +
  scale_x_continuous(breaks = seq(3,10,1), labels = seq(3,10,1)) +
  annotate("text", x = 3.8, y = km_tot+.05, size = 3.5,
           label = "Dotted line show KMeans TOT") +
  theme_bw()

km_data <- result_clean %>% filter(method == "KM")

ga_data <- result_clean %>%
  filter(method != "KM") %>%
  left_join(km_data %>% select(instance, TOT), 
            by = "instance", suffix = c(".ga", ".km")) %>%
  mutate(TOT_gap = (TOT.km - TOT.ga)) %>%
  group_by(method, obj, dimension, miter) %>%
  summarise(TOT_gap = mean(TOT_gap)) %>%
  pivot_wider(id_cols = c(method, obj, miter, dimension), 
              names_from = dimension, values_from = TOT_gap) %>%
  select(method, obj, miter, `3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`) %>%
  arrange(miter)

print(xtable::xtable(ga_data, digits = c(3)), include.rownames = F)
