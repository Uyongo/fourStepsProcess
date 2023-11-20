# Process with four steps
The (latest version of the) _simpleSimulator.R_ (with the including _simpleSimulator_ function) defines a simple workflow of four consecutive steps (using the package _Simmer_ for R). 

![image](https://github.com/Uyongo/fourStepsProcess/assets/53852545/0d8f5740-a933-471c-926b-bc8f4d8f29a1)

Work-items arrive at a chosen rate and undergo processing in steps 1, 2, 3, and 4, after which the workflow ends. Each step requires a distinct resource (i.e. staff group or profession) for completion. The _simpleSimulator_ function's arguments define the average interarrival time of work-items (default: 2.5 min.), the average durations of each step (default: 10 min.), and the numbers of each available staff-group/profession (default: 4 for each staff-group) for the process. Another argument defines how many runs are to be completed (default: 40) before relevant metrics, including throughput and waiting times, are calculated. These are returned by the _simpleSimulator_ function within a vector. All durations (incl. interarrival times) are exponentially distributed. Work-items only arrive within a period of eight hours of simulation time and are each finished completely, even if some require processing beyond the eight hours' period, as above.  

The algorithm in _resultsDataframes.R_ compiles the data (in data-frames) gathered from three sets of simulation runs using the _simpleSimulator_ function. These sets differ in their availablility of staff resources (i.e. 4, 3, and 2 for each staff-group, respectively). Each set includes runs for a range of values for the second step in the workflow. All other arguments of _simpleSimulator_ are default values. 

The algorithm in _resultsPlotter.R_ draws a graph depicting the dependency of the average throughput and waiting times from the duration of the second step in the workflow. Given the variation in resulting metrics, smoothed lines are used in the graph and for the calculation of changes to average throughput and waiting times for the incremental change of the second step in the workflow by 1 (minute). 

![image](https://github.com/Uyongo/fourStepsProcess/assets/53852545/9197373b-d56f-470d-9521-6465c9d485b0)

This simulation experiment highlights the dependence of the effect of changes to the duration of one step (in this case the second step) in a workflow on throughput and waiting times. In more resource constrained settings (purple and blue graphs) average throughput (solid lines) and waiting times (dashed lines) increase stronger than in less resource constrained situations (red graph). The table below displays the change to metrics by increasing the duration of the second step from 10 to 11 minutes.

                                              results
        increase in throughput times (red)          9
        increase in waiting times (red)             8
        increase in throughput times (purple)      14
        increase in waiting times (purple)         13
        increase in throughput times (blue)        22
        increase in waiting times (blue)           21
