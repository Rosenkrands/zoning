library(tidyverse)

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

### Statistical tests
summary(Mean_response_regression)
summary(Ninety_response_regression)
summary(Fulfillment_ration_regression)

### Interactions
# Compare TOT and Kmeans
regression_data_TOTK <-  regression_data %>% filter(`Solution method` %in% c('GA:TOT', 'KM:WCSS'))
Mean_response_regression_TOTK <- lm(`Mean response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs` + `Solution method`:`Arrival rate variance`:`Number of UAVs`, data = regression_data_TOTK)
summary(Mean_response_regression_TOTK)

#Compare ARV and TOT
regression_data_TOTARV <-  regression_data %>% filter(`Solution method` %in% c('GA:ARV', 'GA:TOT'))
Mean_response_regression_TOTARV <- lm(`Mean response` ~ `Solution method` + `Arrival rate variance` + `Number of UAVs` + `Solution method`:`Number of UAVs`, data = regression_data_TOTARV)
summary(Mean_response_regression_TOTARV)














