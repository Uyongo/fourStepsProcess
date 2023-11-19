#This algorithm is only meant to plot the results obtained with the 
#simpleSimulator function. It requires the resultsDataframes algorithm
#to run first. 

lineWidth <- 2
degreesFreedom <- 5
method = 2

#Plotting results of rDf, highlighting a smoothed line in colour.
#Mean throughput times of the system with 4 pharmacists, 4 labellers, 
#4 dispensers and 4 final checkers:
plot(rDf$durationLabel, rDf$meanThroughput, 
     col = 'grey', type = 'l', ylim = c(0, 600), xlim = c(5, 15), 
     main = 'mean throughput and waiting times of a process', 
     xlab = 'duration of the second step in the process (minutes)',
     ylab = 'mean throughput and waiting times (minutes)')
smoothLineR <- smooth.Pspline(rDf$durationLabel, 
                              rDf$meanThroughput, 
                              df = degreesFreedom, 
                              method = method)
smoothLineR0 <- predict(smoothLineR, 
                        rDf$durationLabel, 
                        nderiv = 0)
lines(smoothLineR, col = '#f5334a', lwd = lineWidth, lty = 'solid')
#Mean waiting times of the system with 4 pharmacists, 4 labellers, 
#4 dispensers and 4 final checkers.
lines(rDf$durationLabel, rDf$meanQueue, col = 'grey', lty = 'dashed')
smoothLineRwait <- smooth.Pspline(rDf$durationLabel, 
                                  rDf$meanQueue, 
                                  df = degreesFreedom, 
                                  method = method)
smoothLineRWait0 <- predict(smoothLineRwait, 
                            rDf$durationLabel, 
                            nderiv = 0)
lines(smoothLineRwait, col = '#f5334a', lwd = lineWidth, lty = 'dashed')
#Mean throughput times of the system with 3 pharmacists, 3 labellers, 
#3 dispensers and 3 final checkers:
lines(nDf$durationLabel, nDf$meanThroughput, col = 'grey', lty = 'solid')
smoothLineN <- smooth.Pspline(nDf$durationLabel, 
                              nDf$meanThroughput, 
                              df = degreesFreedom, 
                              method = method)
smoothLineN0 <- predict(smoothLineN,
                        nDf$durationLabel, 
                        nderiv = 0)
lines(smoothLineN, col = '#c133f5', lwd = lineWidth, lty = 'solid')
#Mean waiting times of the system with 3 pharmacists, 3 labellers, 
#3 dispensers and 3 final checkers:
lines(nDf$durationLabel, nDf$meanQueue, col = 'grey', lty = 'dashed')
smoothLineNwait <- smooth.Pspline(nDf$durationLabel, 
                                  nDf$meanQueue, 
                                  df = degreesFreedom, 
                                  method = method)
smoothLineNWait0 <- predict(smoothLineNwait,
                            nDf$durationLabel, 
                            nderiv = 0)
lines(smoothLineNwait, col = '#c133f5', lwd = lineWidth, lty = 'dashed')
#Mean throughput times of the system with 2 pharmacists, 2 labellers, 
#2 dispensers and 2 final checkers:
lines(nnDf$durationLabel, nnDf$meanThroughput, col = 'grey', lty = 'solid')
smoothLineNN <- smooth.Pspline(nnDf$durationLabel, 
                               nnDf$meanThroughput, 
                               df = degreesFreedom, 
                               method = method)
smoothLineNN0 <- predict(smoothLineNN,
                         nnDf$durationLabel, 
                         nderiv = 0)
lines(smoothLineNN, col = '#6d33f5', lwd = lineWidth, lty = 'solid')
#Mean waiting times of the system with 2 pharmacists, 2 labellers,
#2 dispensers and 2 final checkers:
lines(nnDf$durationLabel, nnDf$meanQueue, col = 'grey', lty = 'dashed')
smoothLineNNwait <- smooth.Pspline(nnDf$durationLabel, 
                                   nnDf$meanQueue, 
                                   df = degreesFreedom, 
                                   method = method)
smoothLineNNWait0 <- predict(smoothLineNNwait,
                             nnDf$durationLabel, 
                             nderiv = 0)
lines(smoothLineNNwait, col = '#6d33f5', lwd = lineWidth, lty = 'dashed')
#Creating vertical lines at the x-axis at 10 and 11 minutes:
abline(v = 10, col = 'grey', lwd = 1)
abline(v = 11, col = 'grey', lwd = 1)
#Creating horizontal lines at the intersections of the smooth lines for 
#throughput times with the x-axis at 10 and 11 minutes:
abline(h = smoothLineR0[10], col = 'grey', lwd = 1, lty = 'solid')
abline(h = smoothLineN0[10], col = 'grey', lwd = 1, lty = 'solid')
abline(h = smoothLineNN0[10], col = 'grey', lwd = 1, lty = 'solid')
abline(h = smoothLineR0[11], col = 'grey', lwd = 1, lty = 'solid')
abline(h = smoothLineN0[11], col = 'grey', lwd = 1, lty = 'solid')
abline(h = smoothLineNN0[11], col = 'grey', lwd = 1, lty = 'solid')

#Print increase to throughput times due to increase of duration of the second
#step in the process by one minute: 
tR <- round(smoothLineR0[11] - smoothLineR0[10], 0)
wR <- round(smoothLineRWait0[11] - smoothLineRWait0[10], 0)
tN <- round(smoothLineN0[11] - smoothLineN0[10], 0)
wN <- round(smoothLineNWait0[11] - smoothLineNWait0[10], 0)
tNN <- round(smoothLineNN0[11] - smoothLineNN0[10], 0)
wNN <- round(smoothLineNNWait0[11] - smoothLineNNWait0[10], 0)

resDf <- data.frame('results' = c(tR, wR, tN, wN, tNN, wNN))
row.names(resDf) <- c('increase in throughput times (red)', 
                      'increase in waiting times (red)', 
                      'increase in throughput times (purple)', 
                      'increase in waiting times (purple)', 
                      'increase in throughput times (blue)', 
                      'increase in waiting times (blue)')
print(resDf)

                        
