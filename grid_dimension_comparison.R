source("2d-instance.R")

ARV <- function(solution) {
  solution$instance %>%
    select(`Arrival rate`, `Centroid id`) %>%
    group_by(`Centroid id`) %>%
    summarise(`AR` = sum(`Arrival rate`)) %>%
    summarise(`ARV` = var(`AR`)) %>%
    as.numeric()
}

TOT <- function(solution) {
  tau <- 1
  solution$instance %>%
    rowwise() %>%
    mutate(Distance = euclid_norm(c(x-x.centroid, y-y.centroid))) %>%
    group_by(`Centroid id`) %>%
    summarise(OT = sum(`Arrival rate`*(Distance + tau))) %>%
    summarise(TOT = sum(OT)) %>%
    as.numeric()
}

# SAFE <- function(solution) {
#   dist_temp <- vector(length = nrow(solution$instance))
#   solution$Distance <- dist_temp
#   for (i in 1:nrow(solution$instance)) {
#     for (j in 1:nrow(solution$instance)) {
#       if (solution$instance$`Centroid id`[i] == solution$instance$`Centroid id`[j]) {dist_temp[j] <- Inf}
#       else {
#         dist_temp[j] <- euclid_norm(
#           c(solution$instance$x[i],solution$instance$y[i])-
#             c(solution$instance$x[j],solution$instance$y[j])
#         )
#       }
#     }
#     solution$instance$Distance[i] <- min(dist_temp)
#   }
#   return(min(solution$instance$Distance))
# }

WCSS <- function(solution) {
  solution$instance %>%
    rowwise() %>%
    mutate(Distance = euclid_norm(c(x-x.centroid, y-y.centroid))) %>%
    ungroup() %>%
    summarise(WCSS = sum(Distance)) %>%
    as.numeric()
}

number_of_centroids <- function(solution) {
  length(unique(solution$instance$`Centroid id`))
}

#' For each solution load the object and calculate all objective functions
#' then save as a row in a table. Can be easily parallelized to save time.
#' Get instance id, solution method and objective function from file name.

sol_files <- list.files('./dimension_tuning')

calc_obj <- function(file) {
  clean_name <- substring(file, 1, nchar(file) - 4)
  specification <- str_split(clean_name,'_')
  
  instance_id <- specification[[1]][1]
  method <- specification[[1]][2]
  obj <- specification[[1]][3]
  dimension <- specification[[1]][4]
  miter <- specification[[1]][5]
  
  solution <- readRDS(paste0('./dimension_tuning/',file))
  
  tibble(
    instance = instance_id,
    method,
    obj,
    dimension = dimension,
    miter = miter,
    ARV = ARV(solution),
    # SAFE = SAFE(solution),
    TOT = TOT(solution),
    WCSS = WCSS(solution),
    number_of_centroids = number_of_centroids(solution)
  )
}

result <- do.call(bind_rows, pbapply::pblapply(sol_files %>% as.list(), calc_obj))

# Clean results to reflect correct number of iterations
result_clean <- result %>%
  mutate(miter = replace_na(miter, 100))

data <- result_clean %>%
  group_by(method,obj,dimension,miter) %>%
  summarise(across(c(ARV, TOT, WCSS), mean)) %>%
  mutate(dimension = as.numeric(dimension))

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

