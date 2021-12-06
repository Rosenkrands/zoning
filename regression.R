library(tidyverse)
library(cowplot)

regression_data <- readRDS("./regression-data.rds")

### Regressions
# Response time
Mean_response_regression <- lm(`Mean response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)
Ninety_response_regression <- lm(`90th percentile response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)
# Fulfillment ratio
Fulfillment_ration_regression <- lm(`Fulfillment ratio` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)

### Descriptive Statistics
plot(`Mean response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs`, data = regression_data)
plot(`90th percentile response` ~ `Solution method`, data = regression_data)
plot(`Fulfillment ratio` ~ `Solution method`, data = regression_data)

# ggplot
solmet_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `Mean response`) ) +
    geom_boxplot() 
arvar_gg <- ggplot(regression_data, aes(x = `Arrival rate variance`, y = `Mean response`) ) +
    geom_boxplot() 
nuav_gg <- ggplot(regression_data, aes(x = `Number of UAVs`, y = `Mean response`) ) +
    geom_boxplot() 
plot_grid(solmet_gg, arvar_gg, nuav_gg, ncol = 3, axis = 'l', rel_widths = c(1.4, 1, 1))

perresp_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `90th percentile response`) ) +
    geom_boxplot()
fulrat_gg <- ggplot(regression_data, aes(x = `Solution method`, y = `Fulfillment ratio`) ) +
    geom_boxplot()
plot_grid(perresp_gg, fulrat_gg, ncol = 2, axis = 'l')

### Statistical tests
summary(Mean_response_regression)
summary(Ninety_response_regression)
summary(Fulfillment_ration_regression)

### Interactions
# Compare TOT and Kmeans

regression_data_TOTK <-  regression_data %>% filter(`Solution method` %in% c('GA:TOT', 'KM:WCSS'))
Mean_response_regression_TOTK <- lm(`Mean response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs` + `Solution method`:`Arrival rate variance`, data = regression_data_TOTK)
summary(Mean_response_regression_TOTK)

Mean_response_regression_TOTK_ninety <- lm(`90th percentile response` ~  `Solution method` + `Arrival rate variance` + `Number of UAVs` + `Solution method`:`Arrival rate variance`, data = regression_data_TOTK)
summary(Mean_response_regression_TOTK_ninety)

Mean_response_regression_TOTK_fulfill <- lm(`Fulfillment ratio` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs` + `Solution method`:`Arrival rate variance`, data = regression_data_TOTK)
summary(Mean_response_regression_TOTK_fulfill)


#Compare ARV and TOT
regression_data_TOTARV <-  regression_data %>% filter(`Solution method` %in% c('GA:ARV', 'GA:TOT'))
Mean_response_regression_TOTARV <- lm(`Mean response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs` + `Solution method`:`Arrival rate variance`, data = regression_data_TOTARV)
summary(Mean_response_regression_TOTARV)














