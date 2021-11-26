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
  distanceFunctions::distC(
    solution$instance %>%
      mutate(Distance = 99999) %>%
      select(`Demand point id`, `Centroid id`, Distance, x, y) %>%
      data.matrix()
  )
}

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

calc_obj <- function(file) {
  clean_name <- substring(file, 1, nchar(file) - 4)
  specification <- str_split(clean_name,'_')
  
  instance_id <- specification[[1]][1]
  method <- specification[[1]][2]
  obj <- specification[[1]][3]
  dimension <- specification[[1]][4]
  miter <- specification[[1]][5]
  
  solution <- readRDS(paste0('./solutions/',file))
  
  tibble(
    file = file,
    instance = instance_id,
    method,
    obj,
    dimension = dimension,
    miter = miter,
    ARV = ARV(solution),
    SAFE = SAFE(solution),
    TOT = TOT(solution),
    WCSS = WCSS(solution),
    number_of_centroids = number_of_centroids(solution)
  )
}

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

