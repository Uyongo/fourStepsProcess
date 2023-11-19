#This algorithm only produces the results for the simpleSimulator function. The
#first data-frame is for the system with 4 pharmacists, 4 labellers, 4
#dispensers and 4 final checkers. The second data-frame is for the system with
#3 pharmacists, 3 labellers, 3 dispensers and 3 final checkers. The third
#data-frame is for the system with 2 pharmacists, 2 labellers, 2 dispensers and
#2 final checkers.

rDf <- lapply(1:20, function(durationLabel) simpleSimulator(durationLabel = durationLabel)) %>% 
  bind_rows()
nDf <- lapply(1:20, 
              function(durationLabel) simpleSimulator(durationLabel = durationLabel, 
                                                      numberPharmacists = 3, 
                                                      numberLabellers = 3, 
                                                      numberDispensers = 3, 
                                                      numberFinCheckers = 3)) %>% 
  bind_rows()
nnDf <- lapply(1:20, 
               function(durationLabel) simpleSimulator(durationLabel = durationLabel, 
                                                       numberPharmacists = 2, 
                                                       numberLabellers = 2, 
                                                       numberDispensers = 2, 
                                                       numberFinCheckers = 2)) %>% 
  bind_rows()