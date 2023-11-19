#' @title Calculation of benefit of reducing the duration of one step in a simple process
#' @concept This function calculates the gains/losses to throughput time by changing the duration of the second of four steps in a process. Each step takes (by default) on average 10 minutes and work-items arrive at the process at an average interarrival time of 10 minutes. All distributions are exponential.
#' @param numberWorkers ... number of resources available to process work-items [integer]
#' @param durationSecondStep ... duration of the second step of process [minutes]
#' @return Vector with three numbers: average throughput time [minutes], median throughput time [minutes], change to throughput time compared to default situation [percent]
simpleSimulator <- function(averageInterarrivalTime = 2.5,
                            numberOfRepetitions = 40,
                            numberPharmacists = 4,
                            numberLabellers = 4,
                            numberDispensers = 4,
                            numberFinCheckers = 4,
                            durationVerif = 10,
                            durationLabel = 10,
                            durationDisp = 10,
                            durationFinCheck = 10){

  simProcess <- trajectory(name = "simulated process", verbose = T) %>%
    set_attribute(keys = "waitingForVerif", values = function(){1}) %>% # 1 ... waiting for step 1
    seize(resource = "pharmacist", amount = 1) %>%
    set_attribute(keys = "startVerif", values = function(){2}) %>% # 2 ... processed by step 1
    timeout(function(){rexp(n = 1, rate = 1/durationVerif)}) %>%
    release(resource = "pharmacist", amount = 1) %>%
    set_attribute(keys = "waitingForLabel", values = function(){3}) %>% # 3 ... waiting for step 2
    seize(resource = "labeller", amount = 1) %>%
    set_attribute(keys = "startLabel", values = function(){4}) %>% # 4 ... processed by step 2
    timeout(function(){rexp(n = 1, rate = (1/durationLabel))}) %>% #function parameter determines duration of second step
    release(resource = "labeller", amount = 1) %>%
    set_attribute(keys = "waitingForDisp", values = function(){5}) %>% # 5 ... waiting for step 3
    seize(resource = "dispenser", amount = 1) %>%
    set_attribute(keys = "startDisp", values = function(){6}) %>% # 6 ... processed by step 3
    timeout(function(){rexp(n = 1, rate = 1/durationDisp)}) %>%
    release(resource = "dispenser", amount = 1) %>%
    set_attribute(keys = "waitingForFinCheck", values = function(){7}) %>% # 7 ... waiting for step 4
    seize(resource = "finChecker", amount = 1) %>%
    set_attribute(keys = "startFinCheck", values = function(){8}) %>% # 8 ... processed by step 4
    timeout(function(){rexp(n = 1, rate = 1/durationFinCheck)}) %>%
    release(resource = "finChecker", amount = 1) %>%
    set_attribute(keys = "finishFinCheck", values = function(){9}) # 9 ... process finished

  #numberOfRepetitions <- 20
  envs <- mclapply(1:numberOfRepetitions, function(i){
    simmer(name = "theSimulation", verbose = F) %>%
      add_resource(name = "pharmacist", capacity = numberPharmacists, mon = T) %>%
      add_resource(name = "labeller", capacity = numberLabellers, mon = T) %>%
      add_resource(name = "dispenser", capacity = numberDispensers, mon = T) %>%
      add_resource(name = "finChecker", capacity = numberFinCheckers, mon = T) %>%
      add_generator(name_prefix = "prescription",
                    trajectory = simProcess,
                    at(cumsum(rexp(n = 192, rate = 1/2.5))), #this creates exactly 192 arrivals for each run, which is the number of arrivals in 8 hours at an average interarrival time of 2.5 minutes
                    mon = 2) %>%
      run(until = Inf) %>% #I guess this means the simulator runs until all arrivals are processed
      wrap()
  })

  progressDF <- envs %>%
    get_mon_attributes() %>%
    pivot_wider(id_cols = c(name, replication), names_from = key, values_from = time) %>%
    mutate(queueVerif = startVerif - waitingForVerif, 
           queueLabel = startLabel - waitingForLabel, 
           queueDisp = startDisp - waitingForDisp, 
           queueFinCheck = startFinCheck - waitingForFinCheck, 
           overallQueue = queueVerif + queueLabel + queueDisp + queueFinCheck, 
           throughPutTime = finishFinCheck - waitingForVerif)
  
  summaryDF <- progressDF %>% 
    summarise(.by = replication, 
                         numberItems = length(waitingForVerif), 
                         numberCompleted = sum(!is.na(throughPutTime)), 
                         meanQueue = mean(overallQueue, na.rm = TRUE), 
                         meanThroughput = mean(throughPutTime, na.rm = TRUE)) %>% 
    mutate(percentageCompleted = numberCompleted/numberItems) %>% 
    dplyr::select(replication, numberItems, numberCompleted, 
                  percentageCompleted, meanQueue, meanThroughput) %>% 
    as.data.frame()

  resultsVector <- c(averageInterarrivalTime = averageInterarrivalTime,
                     numberPharmacists = numberPharmacists,
                     numberLabellers = numberLabellers,
                     numberDispensers = numberDispensers,
                     numberFinCheckers = numberFinCheckers,
                     durationVerif = durationVerif,
                     durationLabel = durationLabel,
                     durationDisp = durationDisp,
                     durationFinCheck = durationFinCheck,
                     numberItems = weighted.mean(summaryDF$numberItems, summaryDF$numberItems),
                     numberCompleted = weighted.mean(summaryDF$numberCompleted, summaryDF$numberItems),
                     proportionCompleted = weighted.mean(summaryDF$percentageCompleted, summaryDF$numberItems),
                     meanQueue = weighted.mean(summaryDF$meanQueue, summaryDF$numberItems),
                     meanThroughput = weighted.mean(summaryDF$meanThroughput, summaryDF$numberItems))
  
  return(resultsVector)
    
    
  # resultsDF <- envs %>% # given progressDF, resultsDF does not offer any new information
  #   get_mon_arrivals(ongoing = T) %>%
  #   .[order(.$start_time),] %>%
  #   mutate(throughputTime = (end_time - start_time))
  # resourcesDF <- envs %>% # not sure what to do with this data-frame, it offers information on the timing of resources used and queues at the resources
  #   get_mon_resources()

  # meanThrouputTime <- progressDF %>% filter(!(is.na(totThroughput))) %>%
  #   pull(totThroughput) %>% mean() %>% round(digits = 2)
  # standDevThroughputTime <- progressDF %>% filter(!(is.na(totThroughput))) %>%
  #   pull(totThroughput) %>% sd()
  # errorThroughputTime <- qt(0.975,df=20-1)*standDevThroughputTime/sqrt(20) %>%
  #   round(digits = 2) #95% confidence interval; degrees of freedom is 19 (number of samples minus 1)

  # returnDF <- data.frame(
  #   column = c(
  #     "number of workers" = numberWorkers, # %>% as.character(),
  #     "number of runs" = numberOfRepetitions, # %>% as.character(),
  #     "duration of 2nd step" = durationSecondStep, # %>% as.character(),
  #     "number of items generated per run" = progressDF %>%
  #       nrow() %>% "/" (numberOfRepetitions) %>% round(digits = 0), # %>% as.character(),
  #     "number of items completed per run" = progressDF %>%
  #       filter(!(is.na(totThroughput))) %>%
  #       nrow() %>% "/" (numberOfRepetitions) %>%
  #       round(digits = 2), # %>% as.character(),
  #     "mean throughput time" = meanThrouputTime,# %>% as.character(),
  #     "error throughput time" = errorThroughputTime,
  #     "median throughput time" = progressDF %>% filter(!(is.na(totThroughput))) %>%
  #       pull(totThroughput) %>% median() %>% round(digits = 2), # %>% as.character(),
  #     "mean queue time" = progressDF %>% filter(!(is.na(totThroughput))) %>%
  #       pull(totQueueDur) %>% mean() %>% round(digits = 2), # %>% as.character(),
  #     "median queue time" = progressDF %>% filter(!(is.na(totThroughput))) %>%
  #       pull(totQueueDur) %>% median() %>% round(digits = 2), # %>% as.character(),
  #     "mean activity duration" = progressDF %>% filter(!(is.na(totThroughput))) %>%
  #       pull(totActivityDur) %>% mean() %>% round(digits = 2), # %>% as.character(),
  #     "median activity duration" = progressDF %>% filter(!(is.na(totThroughput))) %>%
  #       pull(totActivityDur) %>% median() %>% round(digits = 2) # %>% as.character()
  #   )
  # )#  %>% t() %>% #transposing the data-frame (i.e. rows become columns and vice versa)
  #  # as.data.frame()

  # colnames(returnDF) <- runif(1, min = 0, max = 999999) %>% round(digits = 0) %>%
  #   as.character() #giving a random column name

  # return(
  #   returnDF
  #   # list(
  #   #   returnDF,
  #   #   progressDF,
  #   #   resultsDF,
  #   #   resourcesDF
  #   # )
  # )
}


