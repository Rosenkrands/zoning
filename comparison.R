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

SAFE <- function(solution) {
  dist_temp <- vector(length = nrow(solution$instance))
  solution$Distance <- dist_temp
  for (i in 1:nrow(solution$instance)) {
    for (j in 1:nrow(solution$instance)) {
      if (solution$instance$`Centroid id`[i] == solution$instance$`Centroid id`[j]) {dist_temp[j] <- Inf}
      else {
        dist_temp[j] <- euclid_norm(
          c(solution$instance$x[i],solution$instance$y[i])-
          c(solution$instance$x[j],solution$instance$y[j])
        )
      }
    }
    solution$instance$Distance[i] <- min(dist_temp)
  }
  return(min(solution$instance$Distance))
}

WCSS <- function(solution) {
  solution$instance %>%
    rowwise() %>%
    mutate(Distance = euclid_norm(c(x-x.centroid, y-y.centroid))) %>%
    ungroup() %>%
    summarise(WCSS = sum(Distance)) %>%
    as.numeric()
}

#' For each solution load the object and calculate all objective functions
#' then save as a row in a table. Can be easily parallelized to save time.
#' Get instance id, solution method and objective function from file name.

sol_files <- list.files('./solutions')

calc_obj <- function(file) {
  clean_name <- substring(file, 1, nchar(file) - 4)
  specification <- str_split(clean_name,'_')
  
  instance_id <- specification[[1]][1]
  method <- specification[[1]][2]
  obj <- specification[[1]][3]
  
  solution <- readRDS(paste0('./solutions/',file))
  
  tibble(
    instance = instance_id,
    method,
    obj,
    ARV = ARV(solution),
    TOT = TOT(solution),
    SAFE = SAFE(solution),
    WCSS = WCSS(solution)
  )
}

result <- do.call(bind_rows, lapply(sol_files %>% as.list(), calc_obj))

result_mean <- result %>%
  group_by(method, obj) %>%
  summarise(ARV = mean(ARV),
            TOT = mean(TOT),
            SAFE = mean(SAFE),
            WCSS = mean(WCSS))
xtable
