rm(list = ls())
source('2d-instance.R')

simulation <- function(
  instance, centroids = NULL, solution, method = c("GA","KMeans")
) {
  # Setting parameters for later use, maybe unneccesary
  nReplications = 3 
  LOS = 100 # Length of simulation
  
  nDemands = nrow(instance$data)
  totaldemandrate = sum(instance$data$`Arrival rate`)
  
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
  
  if (method == "GA") {
    # Update instance data to include zone for each demand point
    assignment <- centroids$distances %>%
      inner_join(solution$centroids, by = "Centroid id") %>%
      group_by(`Demand point id`) %>%
      filter(Distance == min(Distance)) %>%
      ungroup()
    
    instance$data <- instance$data %>% left_join(
      assignment %>% select(
        `Demand point id`, `Centroid id`
      ), by = "Demand point id"
    )  
  } else if (method == "KMeans") {
    instance$data$`Centroid id` <- solution$clustering_vector
  }
  
  getNearestAgent <- function(demandid, agentList){
    x = as.numeric(instance$data[
      instance$data$`Demand point id` == demandid, "x"
    ])
    y = as.numeric(instance$data[
      instance$data$`Demand point id` == demandid, "y"
    ])
    df_temp <- data.frame(id = agentList$id, dist = rep(0, nrow(agentList)))
    demandid_zone <- instance$data[
      instance$data$`Demand point id` == demandid, 'Centroid id'
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
  
  # For now
  df_demandpoints = instance$data %>%
    rename(X = x, Y = y)
  
  metric_list <- list()
  agentLog_list <- list()
  
  for (n in 1:nReplications){
    cat(sprintf("Replication = : %s\n", n))
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
    # Initialize the event list
    eventList = data.frame(event = character(), time = numeric(), agentid = numeric(), demandid = numeric())
    tNext = round(rexp(1, totaldemandrate)) # Sample next demand arrival time
    demand_id = getDemandpointID(df_demandpoints$prob) # assign demend points based on their demand rates
    eventList <-rbind(eventList, data.frame(event = c("Call"), time = c(tNext), demandid = c(demand_id))) # put the first event into the eventlist
    eventList <-rbind(eventList, data.frame(event = c("Move"), time = c(tNext), demandid = c(0))) # put the "move" event into the eventlist
    tNow = tNext
    while(nrow(eventList)>0 && tNow <LOS){
      # Extract the current event from the list and remove the event from the list
      eventNow <- eventList[1,]      
      eventList <- eventList[-c(1),]
      tNow = eventNow$time
      cat(sprintf("EVENT = : %s\t", eventNow$event), sprintf("Time = : %s\n", tNow))
      
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
               eventList <-rbind(eventList, data.frame(event = c("Call"), time = c(tNext), demandid = c(demand_id)))
             },
             
             "Move"={
               movingAgents = agentList[(agentList$status == "BUSY" | agentList$status == "BACK"), ]
               speedAgent= .5 # movement per time unit
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
                       cat(sprintf("Agent %s\t", agent_id), sprintf(" arrived at demand point %s ", agentList$demand_id_handling[agent_id]), sprintf("at time %s\n", tNow))
                       # Record demand performance
                       demandPerformance$totalResponseTime[agentList$demand_id_handling[agent_id]] <-  demandPerformance$totalResponseTime[agentList$demand_id_handling[agent_id]]+ (tNow-agentList$tDeployed[agent_id]) 
                       
                       # Assign the return trip
                       agentList$status[agent_id] = "BACK"
                       agentList$goalX[agent_id] = agentBaseInfo$X[agent_id]
                       agentList$goalY[agent_id] = agentBaseInfo$Y[agent_id]
                     }
                     else { # return to its base
                       cat(sprintf("Agent %s\t", agent_id), sprintf(" returns its home base at time %s\n", tNow))
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
               eventList <-rbind(eventList, data.frame(event = c("Move"), time = c(tNow+1), demandid = c(0)))
               agentLog <- rbind(agentLog, agentList %>% mutate(time = tNow))
             },
             {
               print('A wrong event generated')
             }
      )
      # SORT the events in the list by their time
      eventList <- eventList[order(eventList$time),]
    }
    
    # Calculate distance between agents at any given time
    locations <- agentLog %>%
      select(id, x = Xnow, y = Ynow, time)
    
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
    
    distances <- tibble(id1 = rep(combinations[,1],length(unique(locations$time))), 
                        id2 = rep(combinations[,2],length(unique(locations$time))),
                        time = sort(rep(seq(-1, length(unique(locations$time)) - 2), length(combinations[,1])))) %>%
      rowwise() %>%
      mutate(distance = dist_calc(points = c(id1, id2), t = time))
    
    metric_list[[n]] <- list("demandPerformance" = demandPerformance, 
                             "agentPerformance" = agentPerformance, 
                             "distances" = distances)
    agentLog_list[[n]] <- agentLog
  }
  return(list("metrics" = metric_list, "log" = agentLog_list))
}

# # TEST
# instance = generate_2d_instance(
#   no_of_points = 100,
#   interval = c("min" = -10, "max" = 10)
# )
# 
# centroids = grid_centroids(instance, dimension = 4)
# 
# solution = solve_ga(instance, centroids)
# # solution = solve_kmeans(instance, no_of_centers = 5)
# 
# sim_result <- simulation(instance, centroids, solution, method = "GA")
# # sim_result <- simulation(instance, solution = solution, method = "KMeans")
