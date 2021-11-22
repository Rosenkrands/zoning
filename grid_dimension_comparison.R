source("2d-instance.R")
source("solution-functions.R")

sol_files <- list.files('./solutions')

result <- do.call(
  bind_rows, 
  pbapply::pblapply(sol_files %>% as.list(), calc_obj)
)

data <- result %>%
  filter(miter == 'run500') %>%
  filter((obj == "TOT") | (method == "KM")) %>%
  group_by(method,obj,dimension,miter) %>%
  summarise(across(c(ARV, TOT, WCSS), mean)) %>%
  mutate(dimension = as.numeric(dimension))

# checking instances where ga is better than kmeans, might need to increase nstart
# now increased nstart to 1000 and still see instances where GA performs better
per_instance <- 
  result %>%
  group_by(instance, method) %>%
  slice_min(WCSS, n = 1) %>%
  filter(row_number() == 1)

per_instance %>%
  select(instance, method, WCSS) %>%
  pivot_wider(id_cols = c(-WCSS), values_from = WCSS, names_from = method) %>%
  mutate(gap = GA - KM)
  
ggplot(per_instance) +
  geom_point(aes(x = instance, y = WCSS, color = method))

km_tot <- data %>% 
  filter(method == "KM") %>%
  ungroup() %>%
  select(TOT) %>%
  as.numeric()

ggplot() +
  geom_point(data = data %>% filter(method == "GA"),
             aes(x = dimension, y = TOT)) +
  geom_line(data = data %>% filter(method == "GA"),
             aes(x = dimension, y = TOT)) +
  geom_hline(yintercept = km_tot, linetype = "dashed") +
  scale_x_continuous(breaks = seq(3,10,1), labels = seq(3,10,1)) +
  # annotate("text", x = 4.5, y = km_tot+.001, size = 3.5,
  #          label = "Dotted line show KMeans TOT") +
  theme_bw()

ggsave('./plots_for_report/grid_dimension_comparison.pdf', width = 8, height = 3)

km_data <- result %>% filter(method == "KM")

ga_data <- result %>%
  filter(method != "KM", miter == "run500", obj == "TOT") %>%
  left_join(km_data %>% select(instance, TOT), 
            by = "instance", suffix = c(".ga", ".km")) %>%
  mutate(TOT_gap = (TOT.ga - TOT.km)*100/TOT.ga) %>%
  group_by(method, obj, dimension, miter) %>%
  summarise(TOT_gap = mean(TOT_gap)) %>%
  pivot_wider(id_cols = c(method, obj, miter, dimension), 
              names_from = dimension, values_from = TOT_gap) %>%
  select(method, obj, miter, `3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`) %>%
  arrange(miter)

print(xtable::xtable(ga_data, digits = c(2)), include.rownames = F)
