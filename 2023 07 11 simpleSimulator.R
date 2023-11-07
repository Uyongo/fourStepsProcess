#' @title Calculation of benefit of reducing the duration of one step in a simple process
#' @concept This function calculates the gains/losses to throughput time by changing the duration of the second of four steps in a process. Each step takes (by default) on average 10 minutes and work-items arrive at the process at an average interarrival time of 10 minutes. All distributions are exponential.
#' @param numberWorkers ... number of resources available to process work-items [integer]
#' @param durationSecondStep ... duration of the second step of process [minutes]
#' @return Vector with three numbers: average throughput time [minutes], median throughput time [minutes], change to throughput time compared to default situation [percent]
simpleSimulator <- function(numberWorkers, durationSecondStep){

  simProcess <- trajectory(name = "simulated process", verbose = T) %>%
    set_attribute(keys = "progress", values = function(){1}) %>% # 1 ... waiting for step 1
    seize(resource = "worker", amount = 1) %>%
    set_attribute(keys = "progress", values = function(){2}) %>% # 2 ... processed by step 1
    timeout(function(){rexp(n = 1, rate = 1/10)}) %>%
    release(resource = "worker", amount = 1) %>%
    set_attribute(keys = "progress", values = function(){3}) %>% # 3 ... waiting for step 2
    seize(resource = "worker", amount = 1) %>%
    set_attribute(keys = "progress", values = function(){4}) %>% # 4 ... processed by step 2
    timeout(function(){rexp(n = 1, rate = (1/durationSecondStep))}) %>% #function parameter determines duration of second step
    release(resource = "worker", amount = 1) %>%
    set_attribute(keys = "progress", values = function(){5}) %>% # 5 ... waiting for step 3
    seize(resource = "worker", amount = 1) %>%
    set_attribute(keys = "progress", values = function(){6}) %>% # 6 ... processed by step 3
    timeout(function(){rexp(n = 1, rate = 1/10)}) %>%
    release(resource = "worker", amount = 1) %>%
    set_attribute(keys = "progress", values = function(){7}) %>% # 7 ... waiting for step 4
    seize(resource = "worker", amount = 1) %>%
    set_attribute(keys = "progress", values = function(){8}) %>% # 8 ... processed by step 4
    timeout(function(){rexp(n = 1, rate = 1/10)}) %>%
    release(resource = "worker", amount = 1) %>%
    set_attribute(keys = "progress", values = function(){9}) # 9 ... process finished

  numberOfRepetitions <- 20
  envs <- mclapply(1:numberOfRepetitions, function(i){
    simmer(name = "theSimulation", verbose = F) %>%
      add_resource(name = "worker", capacity = numberWorkers, mon = T) %>%
      add_generator(name_prefix = "item",
                    trajectory = simProcess,
                    at(cumsum(rexp(n = 192, rate = 1/2.5))), #this creates exactly 192 arrivals for each run
                    mon = 2) %>%
      run(until = Inf) %>% #I guess this means the simulator runs until all arrivals are processed
      wrap()
  })

  progressDF <- envs %>%
    get_mon_attributes() %>%
    mutate(activity = recode(value, "queue1", "activity1", "queue2", "activity2", "queue3", "activity3", "queue4", "activity4", "finished")) %>%
    pivot_wider(id_cols = c(name, replication), names_from = activity, values_from = time) %>%
    mutate(queue1Dur = activity1 - queue1, activity1Dur = queue2 - activity1, queue2Dur = activity2 - queue2, activity2Dur = queue3 - activity2, queue3Dur = activity3 - queue3, activity3Dur = queue4 - activity3, queue4Dur = activity4 - queue4, activity4Dur = finished - activity4) %>%
    mutate(totThroughput = finished - queue1) %>%
    mutate(totQueueDur = queue1Dur + queue2Dur + queue3Dur + queue4Dur) %>%
    mutate(totActivityDur = totThroughput - totQueueDur)
  resultsDF <- envs %>%
    get_mon_arrivals(ongoing = T) %>%
    .[order(.$start_time),] %>%
    mutate(throughputTime = (end_time - start_time))
  resourcesDF <- envs %>%
    get_mon_resources()

  meanThrouputTime <- progressDF %>% filter(!(is.na(totThroughput))) %>%
    pull(totThroughput) %>% mean() %>% round(digits = 2)
  standDevThroughputTime <- progressDF %>% filter(!(is.na(totThroughput))) %>%
    pull(totThroughput) %>% sd()
  errorThroughputTime <- qt(0.975,df=20-1)*standDevThroughputTime/sqrt(20) %>%
    round(digits = 2) #95% confidence interval; degrees of freedom is 19 (number of samples minus 1)

  returnDF <- data.frame(
    column = c(
      "number of workers" = numberWorkers, # %>% as.character(),
      "number of runs" = numberOfRepetitions, # %>% as.character(),
      "duration of 2nd step" = durationSecondStep, # %>% as.character(),
      "number of items generated per run" = progressDF %>%
        nrow() %>% "/" (numberOfRepetitions) %>% round(digits = 0), # %>% as.character(),
      "number of items completed per run" = progressDF %>%
        filter(!(is.na(totThroughput))) %>%
        nrow() %>% "/" (numberOfRepetitions) %>%
        round(digits = 2), # %>% as.character(),
      "mean throughput time" = meanThrouputTime,# %>% as.character(),
      "error throughput time" = errorThroughputTime,
      "median throughput time" = progressDF %>% filter(!(is.na(totThroughput))) %>%
        pull(totThroughput) %>% median() %>% round(digits = 2), # %>% as.character(),
      "mean queue time" = progressDF %>% filter(!(is.na(totThroughput))) %>%
        pull(totQueueDur) %>% mean() %>% round(digits = 2), # %>% as.character(),
      "median queue time" = progressDF %>% filter(!(is.na(totThroughput))) %>%
        pull(totQueueDur) %>% median() %>% round(digits = 2), # %>% as.character(),
      "mean activity duration" = progressDF %>% filter(!(is.na(totThroughput))) %>%
        pull(totActivityDur) %>% mean() %>% round(digits = 2), # %>% as.character(),
      "median activity duration" = progressDF %>% filter(!(is.na(totThroughput))) %>%
        pull(totActivityDur) %>% median() %>% round(digits = 2) # %>% as.character()
    )
  )#  %>% t() %>% #transposing the data-frame (i.e. rows become columns and vice versa)
   # as.data.frame()

  colnames(returnDF) <- runif(1, min = 0, max = 999999) %>% round(digits = 0) %>%
    as.character() #giving a random column name

  return(
    returnDF
    # list(
    #   returnDF,
    #   progressDF,
    #   resultsDF,
    #   resourcesDF
    # )
  )
}
