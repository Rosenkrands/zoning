# rm(list = ls())
source('2d-instance.R')

simulation <- function(
  solution, method = c("GA","KMeans"), flight = c("zoned", "free"), log = F
) {
  set.seed(110520)
  # Setting parameters for later use
  nReplications = 1
  LOS = 14400 # Length of simulation
  
  nDemands = nrow(solution$instance)
  totaldemandrate = sum(solution$instance$`Arrival rate`)
  
  if (method == "GA") {
    nAgents = nrow(solution$centroids)
    agentBaseInfo = data.frame(
      id = c(1:nAgents), 
      X = solution$centroids$x, 
      Y = solution$centroids$y,
      `Centroid id` = solution$centroids$`Centroid id`
    )
  } else if (method == "KMeans") {
    nAgents = solution$no_of_centers
    agentBaseInfo = data.frame(
      id = c(1:nAgents), 
      X = solution$clusters$x, 
      Y = solution$clusters$y
    )
  }
  
  # Helper functions
  getDemandpointID <- function(df){
    prob = runif(1)
    for (i in 1:length(df)){
      if(prob < df[i]){
        return(i)
      }
    }
    return(0)
  }
  
  getDistance <- function (x1, y1, x2, y2){
    return(((x1-x2)^2 +(y1-y2)^2)^0.5)
  }
  
  if (flight == "zoned") {
    getNearestAgent <- function(demandid, agentList){
      x = as.numeric(solution$instance[
        solution$instance$`Demand point id` == demandid, "x"
      ])
      y = as.numeric(solution$instance[
        solution$instance$`Demand point id` == demandid, "y"
      ])
      df_temp <- data.frame(id = agentList$id, dist = rep(0, nrow(agentList)))
      demandid_zone <- solution$instance[
        solution$instance$`Demand point id` == demandid, 'Centroid id'
      ] %>% as.character()
      for (i in 1:nrow(agentList)){
        if (
          if (method == "GA"){
            demandid_zone == agentBaseInfo[
              agentBaseInfo$id == agentList$id[i], "Centroid.id"
            ]  
          } else if (method == "KMeans") {
            demandid_zone == agentBaseInfo[agentBaseInfo$id == agentList$id[i], "id"]
          }
        ) {
          df_temp$dist[i] = if_else(
            agentList$status[i] == "IDLE", 
            getDistance(x,y, agentList$Xnow[i], agentList$Ynow[i]), 
            1000000
          )
        }
        else {
          df_temp$dist[i] = 1000000
        }
      }
      df_temp <- df_temp[sample(nrow(df_temp)), ] # to avoid selection with lowest id. 
      df_temp <- df_temp[order(df_temp$dist), ]  # sort the list by distance
      if (df_temp[1,2] < 1000000){
        return (df_temp[1,1]) # return agent id
      } 
      else {# all agents are busy
        return(0)
      }
    }
  } else if (flight == "free") {
    getNearestAgent <- function(demandid, agentList){
      x = as.numeric(solution$instance[
        solution$instance$`Demand point id` == demandid, "x"
      ])
      y = as.numeric(solution$instance[
        solution$instance$`Demand point id` == demandid, "y"
      ])
      df_temp <- data.frame(id = agentList$id, dist = rep(0, nrow(agentList)))
      for (i in 1:nrow(agentList)){
        df_temp$dist[i] = if_else(
          agentList$status[i] == "IDLE", 
          getDistance(x,y, agentList$Xnow[i], agentList$Ynow[i]), 
          1000000
        )
      }
      df_temp <- df_temp[sample(nrow(df_temp)), ] # to avoid selection with lowest id. 
      df_temp <- df_temp[order(df_temp$dist), ]  # sort the list by distance
      if (df_temp[1,2] < 1000000){
        return (df_temp[1,1]) # return agent id
      } 
      else {# all agents are busy
        return(0)
      }
    }
  }
  
  # For now
  df_demandpoints = solution$instance %>%
    rename(X = x, Y = y)
  
  metric_list <- list()
  agentLog_list <- list()
  utilization_list <- list()
  
  for (n in 1:nReplications){
    # cat(sprintf("Replication = : %s\n", n))
    # Initialize an agent list (assume they are at 0,0 at the beginning)
    agentList = data.frame(id = agentBaseInfo$id, 
                           Xnow = agentBaseInfo$X, 
                           Ynow = agentBaseInfo$Y, 
                           goalX= agentBaseInfo$X, 
                           goalY= agentBaseInfo$Y,
                           demand_id_handling = rep(0, nAgents),
                           tDeployed = rep(0, nAgents),
                           status  = rep("IDLE", nAgents), stringsAsFactors=FALSE)
    
    agentLog = agentList %>% mutate(time = -1)
    
    # Initialize a simulation result
    demandPerformance = data.frame(nGenerated = rep(0, nDemands), nCovered = rep(0, nDemands), totalResponseTime = rep(0, nDemands))
    agentPerformance = data.frame(nDispatched= rep(0, nAgents), totalUsage= rep(0,nAgents))
    responseTimePerformance = data.frame(demand_id_handling = integer(), responseTime = integer()) 
    # Initialize the event list
    eventList = data.frame(event = character(), time = numeric(), agentid = numeric(), demandid = numeric())
    tNext = round(rexp(1, totaldemandrate)) # Sample next demand arrival time
    demand_id = getDemandpointID(df_demandpoints$prob) # assign demend points based on their demand rates
    eventList <-bind_rows(eventList, data.frame(event = c("Call"), time = c(tNext), demandid = c(demand_id))) # put the first event into the eventlist
    eventList <-bind_rows(eventList, data.frame(event = c("Move"), time = c(tNext), demandid = c(0))) # put the "move" event into the eventlist
    tNow = tNext
    reset = F # used in relation to excluding the warm up period
    while(nrow(eventList)>0 && tNow <LOS){
      # Extract the current event from the list and remove the event from the list
      eventNow <- eventList[1,]      
      eventList <- eventList[-c(1),]
      tNow = eventNow$time
      # cat(sprintf("EVENT = : %s\t", eventNow$event), sprintf("Time = : %s\n", tNow))
      
      # Exclude the warmup period (i.e. the first hour of the simulation)
      if ((tNow >= 3600) & (reset == F)) {
        demandPerformance = data.frame(nGenerated = rep(0, nDemands), nCovered = rep(0, nDemands), totalResponseTime = rep(0, nDemands))
        agentPerformance = data.frame(nDispatched= rep(0, nAgents), totalUsage= rep(0,nAgents))
        responseTimePerformance = data.frame(demand_id_handling = integer(), responseTime = integer()) 
        reset = T
      }
      
      switch(as.character(eventNow$event), 
             "Call"={
               # update call performance data
               demandPerformance$nGenerated[eventNow$demandid] <-  demandPerformance$nGenerated[eventNow$demandid] +1
               # Find the nearest agent
               agent_id = getNearestAgent(eventNow$demandid, agentList)  
               if (agent_id>0){
                 # Update status of the agent assigned to the call
                 agentList$status[agent_id] <- "BUSY"
                 agentList$goalX[agent_id] <- df_demandpoints$X[eventNow$demandid]
                 agentList$goalY[agent_id] <- df_demandpoints$Y[eventNow$demandid]
                 agentList$demand_id_handling[agent_id] <- eventNow$demandid
                 agentList$tDeployed[agent_id] <- tNow
                 agentPerformance$nDispatched[agent_id] <- agentPerformance$nDispatched[agent_id] +1
                 demandPerformance$nCovered[eventNow$demandid] <-  demandPerformance$nCovered[eventNow$demandid] +1
               }
               else{
                 # No agent is available.
                 # Assume that this call is discharged in this example. 
               }
               
               # Generate next call
               tNext = tNow + round(rexp(1, totaldemandrate)) 
               demand_id = getDemandpointID(df_demandpoints$prob)
               eventList <-bind_rows(eventList, data.frame(event = c("Call"), time = c(tNext), demandid = c(demand_id)))
             },
             
             "Move"={
               movingAgents = agentList[(agentList$status == "BUSY" | agentList$status == "BACK"), ]
               speedAgent=.025 # movement per time unit (kilometers per second)
               # i.e. 25 meter per second is equiv to 90 km/h (25 m/s times 3.6)
               if(nrow(movingAgents)>0){
                 for(i in 1:nrow(movingAgents)){
                   agent_id = movingAgents$id[i]
                   # position update
                   xIncrement = (agentList$goalX[agent_id]- agentList$Xnow[agent_id])
                   yIncrement = (agentList$goalY[agent_id]- agentList$Ynow[agent_id])
                   lengthToMove = (xIncrement^2 + yIncrement^2)^0.5
                   if(lengthToMove > speedAgent){
                     angle = atan2(yIncrement, xIncrement)
                     xUpdate = speedAgent*cos(angle)
                     yUpdate = speedAgent*sin(angle)
                     agentList$Xnow[agent_id] = agentList$Xnow[agent_id] +  xUpdate
                     agentList$Ynow[agent_id] = agentList$Ynow[agent_id] +  yUpdate  
                   }
                   else{ # Agent arrived at the destination
                     agentList$Xnow[agent_id]  = agentList$goalX[agent_id]
                     agentList$Ynow[agent_id]  = agentList$goalY[agent_id]
                     if (agentList$status[agent_id] == "BUSY"){ # arrived at the demand point
                       # cat(sprintf("Agent %s\t", agent_id), sprintf(" arrived at demand point %s ", agentList$demand_id_handling[agent_id]), sprintf("at time %s\n", tNow))
                       # Record demand performance
                       demandPerformance$totalResponseTime[agentList$demand_id_handling[agent_id]] <-  demandPerformance$totalResponseTime[agentList$demand_id_handling[agent_id]]+ (tNow-agentList$tDeployed[agent_id])
                       responseTimePerformance <- bind_rows(
                         responseTimePerformance,
                         data.frame(demand_id_handling = agentList$demand_id_handling[agent_id],
                                    responseTime = tNow-agentList$tDeployed[agent_id])
                       )
                       
                       
                       # Assign the return trip
                       agentList$status[agent_id] = "BACK"
                       agentList$goalX[agent_id] = agentBaseInfo$X[agent_id]
                       agentList$goalY[agent_id] = agentBaseInfo$Y[agent_id]
                     }
                     else { # Agent returned to its base
                       # cat(sprintf("Agent %s\t", agent_id), sprintf(" returns its home base at time %s\n", tNow))
                       # update agent usage data
                       agentPerformance$totalUsage[agent_id] <- agentPerformance$totalUsage[agent_id]  + (tNow - agentList$tDeployed[agent_id])
                       # update agent status
                       agentList$status[agent_id] = "IDLE"
                       # We may add set-up time for a next deployment
                     }
                   }
                 }
               }
               # Generate next "Move" event
               eventList <-bind_rows(eventList, data.frame(event = c("Move"), time = c(tNow+1), demandid = c(0)))
               agentLog <- bind_rows(agentLog, agentList %>% mutate(time = tNow))
             },
             {
               print('A wrong event generated')
             }
      )
      # SORT the events in the list by their time
      eventList <- eventList[order(eventList$time),]
    }
    
    first_demand <- agentLog %>%
      select(time) %>%
      filter(time != -1) %>% unique() %>%
      filter(time == min(time)) %>% as.numeric()

    first_row <- agentLog %>% filter(time == -1)
    missing <- first_row[0,]
    for (i in 1:first_demand) {
      missing <- bind_rows(missing, first_row %>% mutate(time = i - 1))
    }
    
    agentLog <- bind_rows(
      agentLog %>% filter(time == -1),
      missing,
      agentLog %>% filter(time >= first_demand)
    )
    
    # Calculate distance between agents at any given time
    locations <- agentLog %>%
      select(id, x = Xnow, y = Ynow, time) %>%
      mutate(idt = paste0(time,'_',id))

    combinations <- combn(unique(locations$id), 2) %>% t()

    dist_calc = function(points, t) {
      locations_temp <- locations %>% filter(time == t)
      # print(locations_temp)
      euclid_norm(
        c(
          locations_temp$x[points[1]] - locations_temp$x[points[2]],
          locations_temp$y[points[1]] - locations_temp$y[points[2]]
        )
      )
    }

    # distances1 <- tibble(id1 = rep(combinations[,1],length(unique(locations$time))),
    #                     id2 = rep(combinations[,2],length(unique(locations$time))),
    #                     time = sort(rep(seq(-1, length(unique(locations$time)) - 2), length(combinations[,1])))) %>%
    #   rowwise() %>%
    #   mutate(distance = dist_calc(points = c(id1, id2), t = time))
    
    distances2 <- tibble(id1 = rep(combinations[,1],length(unique(locations$time))),
                        id2 = rep(combinations[,2],length(unique(locations$time))),
                        time = sort(rep(seq(-1, length(unique(locations$time)) - 2), length(combinations[,1])))) %>%
      mutate(idt1 = paste0(time, '_',id1), idt2 = paste0(time, '_',id2)) %>%
      inner_join(locations %>% select(idt,x,y), by=c("idt1" = "idt")) %>%
      inner_join(locations %>% select(idt,x,y), by=c("idt2" = "idt"), suffix = c(".1",".2")) %>%
      select(-c(idt1, idt2))
    
    distances2 <- distances2 %>% mutate(
      distance = distanceFunctions::simDistC(
        distances2 %>% select(x.1, y.1, x.2, y.2) %>% data.matrix()
      )
    )
    
    metric_list[[n]] <- list("demandPerformance" = demandPerformance, 
                             "agentPerformance" = agentPerformance,
                             "responseTimePerformance" = responseTimePerformance
                             #,"distances" = distances2
                             )
    agentLog_list[[n]] <- agentLog
    utilization_list[[n]] <- agentLog %>% 
      select(id, status, time) %>%
      mutate(inUse = ifelse(status != "IDLE", 1, 0)) %>%
      group_by(time) %>%
      summarise(inUse = mean(inUse)) %>%
      mutate(inUse = cumsum(inUse))
  }
  set.seed(NULL)
  if (log == T) {
    return(list("metrics" = metric_list, "log" = agentLog_list))
  } else {
    return(list("metrics" = metric_list, "utilization" = utilization_list))
  }
}

# TEST
# instance = generate_2d_instance(
#   no_of_points = 100,
#   interval = c("min" = -10, "max" = 10)
# )

# centroids = grid_centroids(instance, dimension = 4)

# solution = solve_ga(instance, centroids, miter = 5)
# solution = solve_kmeans(instance, no_of_centers = 5)

# sim_result <- simulation(solution = solution, method = "GA")
# sim_result_zoned <- simulation(solution = solution,
#                          method = "KMeans", flight = "zoned")
# sim_result_free <- simulation(solution = solution,
#                                method = "KMeans", flight = "free")

# histogram of safety distances
# bind_rows(
#   sim_result_free$metrics[[1]]$responseTime %>% mutate(flight = "free"),
#   sim_result_zoned$metrics[[1]]$responseTime %>% mutate(flight = "zoned")
# ) %>%
#   ggplot(aes(x = responseTime, fill = flight)) +
#   geom_histogram(bins = 50, color = "black") +
#   facet_wrap(~flight, nrow = 2) +
#   theme_bw() +
#   theme(legend.position = "none")

# utilization
# bind_rows(
#   sim_result_free$log[1][[1]] %>% mutate(flight = "free"),
#   sim_result_zoned$log[1][[1]] %>% mutate(flight = "zoned"),
# ) %>%
#   select(id, status, time, flight) %>%
#   mutate(inUse = ifelse(status != "IDLE", 1, 0)) %>%
#   group_by(flight, time) %>%
#   summarise(inUse = mean(inUse)) %>%
#   mutate(inUse = cumsum(inUse)) %>%
#   ggplot(aes(x = time, y = inUse/time, color = flight)) +
#   geom_line() +
#   # tidyquant::geom_ma(n=30) +
#   geom_hline(yintercept = 1, linetype="dashed") +
#   theme_bw()

# effectiveness
# bind_rows(
#   sim_result_free$log[1][[1]] %>% mutate(flight = "free"),
#   sim_result_zoned$log[1][[1]] %>% mutate(flight = "zoned"),
# ) %>%
#   select(id, status, time, flight) %>%
#   mutate(inUse = ifelse(status != "IDLE", 1, 0)) %>%
#   group_by(flight, time) %>%
#   summarise(inUse = mean(inUse)) %>%
#   mutate(inUse = cumsum(inUse)) %>%
#   ggplot(aes(x = time, y = inUse/time, color = flight)) +
#   geom_line() +
#   # tidyquant::geom_ma(n=30) +
#   geom_hline(yintercept = 1, linetype="dashed") +
#   theme_bw()

# effectiveness
# bind_rows(
#   sim_result_free$metrics[[1]]$demandPerformance  %>%
#     summarise(across(c(nGenerated,nCovered), sum)) %>%
#     mutate(flight = "free"),
#   sim_result_zoned$metrics[[1]]$demandPerformance  %>%
#     summarise(across(c(nGenerated,nCovered), sum)) %>%
#     mutate(flight = "zoned")
# ) %>%
#   mutate(`nC/nG` = nCovered / nGenerated) %>%
#   xtable::xtable()
