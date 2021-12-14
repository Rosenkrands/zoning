library(tidyverse)

regression_data <- readRDS("./regression-data.rds") %>%
  filter(`Solution method` %in% c('GA:TOT','WKM:WWCSS'))

regression_data2 <- regression_data %>%
  group_by(`Solution method`, `Arrival rate variance`) %>%
  summarise(y_se = psych::describe(`Mean response`)$se,
            y_mean = mean(`Mean response`))

regression_data2 %>%
  ggplot(aes(x = `Arrival rate variance`,
             y = y_mean,
             color = `Solution method`)) +
  geom_line(aes(group = `Solution method`)) +
  geom_point() +
  geom_errorbar(aes(ymin = y_mean-1.96*y_se,
                    ymax = y_mean+1.96*y_se),
                width = .1) +
  # facet_wrap(~`Number of UAVs`, scales = 'free', labeller = label_both) +
  theme_bw() + labs(y = "Mean response")

ggsave('./plots_for_report/arrival_rate_variance_main_effect.pdf', width = 5, height = 4)

regression_data2 <- regression_data %>%
  group_by(`Solution method`, `Arrival rate variance`) %>%
  summarise(y_se = psych::describe(`Fulfillment ratio`)$se,
            y_mean = mean(`Fulfillment ratio`))

regression_data2 %>%
  ggplot(aes(x = `Arrival rate variance`,
             y = y_mean,
             color = `Solution method`)) +
  geom_line(aes(group = `Solution method`)) +
  geom_point() +
  geom_errorbar(aes(ymin = y_mean-1.96*y_se,
                    ymax = y_mean+1.96*y_se),
                width = .1) +
  # facet_wrap(~`Number of UAVs`, scales = 'free', labeller = label_both) +
  theme_bw() + labs(y = "Fulfilment ratio")

ggsave('./plots_for_report/number_of_uavs_main_effect.pdf', width = 5, height = 4)

