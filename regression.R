library(tidyverse)
library(cowplot)
library(xtable)

regression_data <- readRDS("./regression-data.rds") %>%
    filter(`Solution method` != 'KM:WCSS')

# Comparison between queue and no queue
regression_data_TOT <- regression_data %>% 
  filter(`Solution method` == 'GA:TOT') %>%
  mutate(queue = 0)
  
ggplot(regression_data_TOT, aes(x = queue, y = `Mean response`)) +
  geom_boxplot()

### Regressions
# Response time
# Mean_response_regression <- lm(`Mean response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)
# Ninety_response_regression <- lm(`90th percentile response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)
# # Fulfillment ratio
# Fulfillment_ration_regression <- lm(`Fulfillment ratio` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)

### Descriptive Statistics
# plot(`Mean response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)
# plot(`90th percentile response` ~ `Solution method`, data = regression_data)
# plot(`Fulfillment ratio` ~ `Solution method`, data = regression_data)

# ggplot
solmet_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `Mean response`) ) +
    geom_boxplot() 

regression_data2 <- regression_data %>%
  group_by(`Arrival rate variance`) %>%
  summarise(y_se = psych::describe(`Mean response`)$se,
            y_mean = mean(`Mean response`))

arvar_gg <- regression_data2 %>%
  ggplot(aes(x = `Arrival rate variance`,
             y = y_mean)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = y_mean-1.96*y_se,
                    ymax = y_mean+1.96*y_se),
                width = .1) + labs(y = "")

# arvar_gg <- ggplot(regression_data, aes(x = `Arrival rate variance`, y = `Mean response`) ) +
#     geom_boxplot() +
#     labs(y = "")

regression_data2 <- regression_data %>%
  group_by(`Number of UAVs`) %>%
  summarise(y_se = psych::describe(`Mean response`)$se,
            y_mean = mean(`Mean response`))

nuav_gg <- regression_data2 %>%
  ggplot(aes(x = `Number of UAVs`,
             y = y_mean)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = y_mean-1.96*y_se,
                    ymax = y_mean+1.96*y_se),
                width = .1) + labs(y = "")

# nuav_gg <- ggplot(regression_data, aes(x = `Number of UAVs`, y = `Mean response`) ) +
#     geom_boxplot() +
#     labs(y = "")
plot_grid(solmet_gg, arvar_gg, nuav_gg, ncol = 3, axis = 'l', rel_widths = c(2, 1, 1))  

ggsave('./plots_for_report/Mean_resp_box.pdf', width = 9, height = 3)

solmet_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `distance min`) ) +
  geom_boxplot() +
  labs(y = "Minimum distance")

regression_data2 <- regression_data %>%
  group_by(`Arrival rate variance`) %>%
  summarise(y_se = psych::describe(`distance min`)$se,
            y_mean = mean(`distance min`))

arvar_gg <- regression_data2 %>%
  ggplot(aes(x = `Arrival rate variance`,
             y = y_mean)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = y_mean-1.96*y_se,
                    ymax = y_mean+1.96*y_se),
                width = .1) + labs(y = "")

# arvar_gg <- ggplot(regression_data, aes(x = `Arrival rate variance`, y = `90th percentile response`) ) +
#   geom_boxplot() +
#   labs(y = "")

regression_data2 <- regression_data %>%
  group_by(`Number of UAVs`) %>%
  summarise(y_se = psych::describe(`distance min`)$se,
            y_mean = mean(`distance min`))

nuav_gg <- regression_data2 %>%
  ggplot(aes(x = `Number of UAVs`,
             y = y_mean)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = y_mean-1.96*y_se,
                    ymax = y_mean+1.96*y_se),
                width = .1) + labs(y = "")

# nuav_gg <- ggplot(regression_data, aes(x = `Number of UAVs`, y = `90th percentile response`) ) +
#   geom_boxplot() +
#   labs(y = "")
plot_grid(solmet_gg, arvar_gg, nuav_gg, ncol = 3, axis = 'l', rel_widths = c(2, 1, 1)) 

ggsave('./plots_for_report/min_dist_all_box_plot.pdf', width = 9, height = 3)

solmet_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `Fulfillment ratio`) ) +
  geom_boxplot() 
arvar_gg <- ggplot(regression_data, aes(x = `Arrival rate variance`, y = `Fulfillment ratio`) ) +
  geom_boxplot() +
  labs(y = "")
nuav_gg <- ggplot(regression_data, aes(x = `Number of UAVs`, y = `Fulfillment ratio`) ) +
  geom_boxplot() +
  labs(y = "")
plot_grid(solmet_gg, arvar_gg, nuav_gg, ncol = 3, axis = 'l', rel_widths = c(2, 1, 1))  

perresp_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `90th percentile response`) ) +
    geom_boxplot()
fulrat_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `Fulfillment ratio`) ) +
    geom_boxplot()
plot_grid(perresp_gg, fulrat_gg, ncol = 2, axis = 'l')

# Appendix
# solmet_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `Fulfillment ratio`) ) +
#   geom_boxplot() 
# arvar_gg <- ggplot(regression_data, aes(x = `Arrival rate variance`, y = `Fulfillment ratio`) ) +
#   geom_boxplot() 
# nuav_gg <- ggplot(regression_data, aes(x = `Number of UAVs`, y = `Fulfillment ratio`) ) +
#   geom_boxplot() 
# plot_grid(solmet_gg, arvar_gg, nuav_gg, ncol = 3, axis = 'l', rel_widths = c(1.4, 1, 1))

### Statistical tests
summary(Mean_response_regression)
print(xtable(summary(Mean_response_regression)))
summary(Ninety_response_regression)
print(xtable(summary(Ninety_response_regression)))
summary(Fulfillment_ration_regression)
print(xtable(summary(Fulfillment_ration_regression)))

### Interactions
# Compare TOT and Kmeans

regression_data_TOTK <-  regression_data %>% filter(`Solution method` %in% c('GA:TOT', 'WKM:WWCSS'))
Mean_response_regression_TOTK <- lm(`Mean response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs` + `Solution method`:`Arrival rate variance`:`Number of UAVs`, data = regression_data_TOTK)
summary(Mean_response_regression_TOTK)
print(xtable(summary(Mean_response_regression_TOTK)))

Mean_response_regression_TOTK_ninety <- lm(`90th percentile response` ~  `Solution method` + `Arrival rate variance` + `Number of UAVs` + `Solution method`:`Arrival rate variance`, data = regression_data_TOTK)
summary(Mean_response_regression_TOTK_ninety)
print(xtable(summary(Mean_response_regression_TOTK_ninety)))

Mean_response_regression_TOTK_fulfill <- lm(`Fulfillment ratio` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs` + `Solution method`:`Arrival rate variance`, data = regression_data_TOTK)
summary(Mean_response_regression_TOTK_fulfill)
print(xtable(summary(Mean_response_regression_TOTK_fulfill)))

#Number of UAVs
Mean_response_regression_TOTK_UAV <- lm(`Mean response` ~ `Arrival rate variance` + `Number of UAVs`*`Solution method` , data = regression_data_TOTK)
summary(Mean_response_regression_TOTK_UAV)
print(xtable(summary(Mean_response_regression_TOTK_UAV)))

Mean_response_regression_TOTK_UAV <- lm(`90th percentile response` ~ `Arrival rate variance` + `Number of UAVs`*`Solution method` , data = regression_data_TOTK)
summary(Mean_response_regression_TOTK_UAV)
print(xtable(summary(Mean_response_regression_TOTK_UAV)))

Mean_response_regression_TOTK_UAV <- lm(`Fulfillment ratio` ~ `Arrival rate variance` + `Number of UAVs`*`Solution method` , data = regression_data_TOTK)
summary(Mean_response_regression_TOTK_UAV)
print(xtable(summary(Mean_response_regression_TOTK_UAV)))


#Compare ARV and TOT
regression_data_TOTARV <-  regression_data %>% filter(`Solution method` %in% c('GA:ARV', 'GA:TOT'))
Mean_response_regression_TOTARV <- lm(`Mean response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs` + `Solution method`:`Arrival rate variance`, data = regression_data_TOTARV)
summary(Mean_response_regression_TOTARV)



### Safety measures Overview
solmet_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `distance min`) ) +
  geom_boxplot() 
arvar_gg <- ggplot(regression_data, aes(x = `Arrival rate variance`, y = `distance min`) ) +
  geom_boxplot() +
    labs(y = "")
nuav_gg <- ggplot(regression_data, aes(x = `Number of UAVs`, y = `distance min`) ) +
  geom_boxplot() +
    labs(y = "")
plot_grid(solmet_gg, arvar_gg, nuav_gg, ncol = 3, axis = 'l', rel_widths = c(2, 1, 1))

solmet_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `distance 5th percentile`) ) +
  geom_boxplot() 
arvar_gg <- ggplot(regression_data, aes(x = `Arrival rate variance`, y = `distance 5th percentile`) ) +
  geom_boxplot() +
  labs(y = "")
nuav_gg <- ggplot(regression_data, aes(x = `Number of UAVs`, y = `distance 5th percentile`) ) +
  geom_boxplot() +
  labs(y = "")
plot_grid(solmet_gg, arvar_gg, nuav_gg, ncol = 3, axis = 'l', rel_widths = c(2, 1, 1))

solmet_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `distance mean`) ) +
  geom_boxplot() 
arvar_gg <- ggplot(regression_data, aes(x = `Arrival rate variance`, y = `distance mean`) ) +
  geom_boxplot() +
  labs(y = "")
nuav_gg <- ggplot(regression_data, aes(x = `Number of UAVs`, y = `distance mean`) ) +
  geom_boxplot() +
  labs(y = "")
plot_grid(solmet_gg, arvar_gg, nuav_gg, ncol = 3, axis = 'l', rel_widths = c(2, 1, 1))

perresp_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `distance 5th percentile`) ) +
  geom_boxplot()
fulrat_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `distance mean`) ) +
  geom_boxplot()
plot_grid(perresp_gg, fulrat_gg, ncol = 2, axis = 'l')


### Safety measure inference
# Basic regression with different measures
# Safety measures replace
min_dist_regression <- lm(`distance min` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)
fifth_regression <- lm(`distance 5th percentile` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)
mean_dist_regression <- lm(`distance mean` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)

summary(min_dist_regression)
print(xtable(summary(min_dist_regression)))
summary(fifth_regression)
print(xtable(summary(fifth_regression)))
summary(mean_dist_regression)
print(xtable(summary(mean_dist_regression)))

#Interaction between arrival rate variance and all sol methods for some safety measures.
min_dist_regression_TOTK <- lm(`distance min` ~ `Arrival rate variance`*`Solution method` + `Number of UAVs`, data = regression_data)
summary(min_dist_regression_TOTK)
print(xtable(summary(min_dist_regression_TOTK)))

fifth_dist_regression_TOTK <- lm(`distance 5th percentile` ~ `Arrival rate variance`*`Solution method` + `Number of UAVs`, data = regression_data)
summary(fifth_dist_regression_TOTK)
print(xtable(summary(fifth_dist_regression_TOTK)))

mean_dist_regression_TOTK <- lm(`distance mean` ~ `Arrival rate variance`*`Solution method` + `Number of UAVs`, data = regression_data)
summary(mean_dist_regression_TOTK)
print(xtable(summary(mean_dist_regression_TOTK)))


