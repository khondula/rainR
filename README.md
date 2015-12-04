# Purpose

The `rainR` package was developed to provide R code for the calculation of metrics associated with individual rainstorms from a timestamped record of precipitation. 

The current version takes as input a data frame with at least columns for time and amount of rainfall, and returns a summary data frame with metrics associated with each event, and a list of data frames for each event. You specify the minimum inter-event time (defaults to 6 hours[1]) to use. 

### Event metrics

1. total depth
2. maximum intensity
3. maximum one hour intensity
4. average intensity
5. duration (in minutes, hours, and days)
6. antecedent dry period (in hours)
7. proportion of event without rain[2]
8. length of longest intra-event time without rain[2]
9. proportion of event that is longest dry period 
10. starting and ending time of event
11. amount of rainfall in previous 1-14 days before event

# References and Notes

[1] International BMP Stormwater Database 

[2] Metrics 7-9 are measurements of intermittency as defined by Dunkerley 2015 [10.1002/hyp.10454](http://onlinelibrary.wiley.com/doi/10.1002/hyp.10454/abstract)

# R packages dependencies

- stringr
- dplyr

# How to install

```
install.packages("devtools")  # if necessary
devtools::install_github("khondula/rainR")
```

# Acknowledgements

This work was supported by the National Socio-Environmental Synthesis Center (SESYNC) under funding received from the National Science Foundation DBI-1052875.

# Potential improvements

- This package could be modified to be compatible with tipping bucket data (i.e. irregularly spaced timestamps of tips). 
- Add a function to create events from other precipitation data (e.g. from `‘raincpc` package) and a user-defined start and end date
- Ensure consistency of time and date data between input and output/have more flexible options for date and time input column(s)
