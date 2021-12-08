source('2d-instance.R')

instances <- tibble(filename = list.files('./instances')) %>%
  rowwise() %>%
  mutate(location_id = str_split(filename, '_')[[1]][1],
         arv_id = str_replace(str_split(filename,'_')[[1]][2], '.rds', ''),
         `Arrival rate variance` = diff(readRDS(paste0('./instances/',filename))$arv)) %>%
  mutate(`Arrival rate variance` = factor(`Arrival rate variance`, levels = c(20, 50, 80), labels = c("low", "medium", "high")))

plot_points <- function(layout_id = 1, arv = NA) {
  if (is.na(arv)) {
    path <- paste0('./instances/',(instances %>% filter(location_id == layout_id))[1,1])
  } else {
    path <- paste0('./instances/',(instances %>% filter(location_id == layout_id, `Arrival rate variance` == arv))[1,1])
  }
  cat('path is: ', path, '\n')
  
  limits <- readRDS(paste0('./instances/',(instances %>% filter(location_id == layout_id, `Arrival rate variance` == "high"))[1,1]))$data %>%
    select(`Arrival rate`) %>% summarise(min = min(`Arrival rate`), max = max(`Arrival rate`))
  
  plot_data <-  readRDS(path)$data %>% 
    mutate(`Arrival rate group` = factor(cut(log(`Arrival rate`), breaks = seq(log(0.0001),log(0.08),length.out = 10))))
  
  base_plot <- ggplot(
   plot_data
  ) +
    geom_path(data = tibble(x = c(-10,10,10,-10,-10), 
                            y = c(-10,-10,10,10,-10)) %>%
                mutate(across(c(x,y), ~.x*1.1)),
              aes(x,y)) + 
    theme_void()  
  
  if (is.na(arv)) {
    base_plot + geom_point(aes(x,y))
  } else {
      base_plot + geom_point(aes(x,y, color = `Arrival rate group`)) + 
      scale_color_discrete(drop = TRUE, limits = levels(plot_data$`Arrival rate group`)) +
      theme_void() + theme(legend.position = "none")
  }
}

layout_id <- 1

plot_points(layout_id = layout_id)
ggsave('./plots_for_report/demand_point_locations.pdf', width = 4, height = 4)
plot_points(layout_id = layout_id, arv = "low")
ggsave('./plots_for_report/demand_point_locations_low_variance.pdf', width = 4, height = 4)
# plot_points(layout_id = layout_id, arv = "medium")
plot_points(layout_id = layout_id, arv = "high")
ggsave('./plots_for_report/demand_point_locations_high_variance.pdf', width = 4, height = 4)


