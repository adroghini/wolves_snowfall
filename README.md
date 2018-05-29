# How does snowfall affect wolf movements?
This project investigated the effect of snowfall events on the movement behaviour of grey wolves. We quantified wolf movement using GPS telemetry data and identified snowfall events using remote cameras. The study took place in northeastern Alberta, Canada, near the town of Fort McMurray over the course of two winters (Jan. to Mar. 2013 and 2014).

The manuscript for which these scripts were built is currently under review: Droghini, A., and S. Boutin. 2018. The calm during the storm: Snowfall events decrease the movement of grey wolves (*Canis lupus*). In review.

# Software requirements
- R version 3.5.0 (https://www.r-project.org/)
- RStudio version 1.1.453 (https://www.rstudio.com/)

## Package requirements
The following R packages must be installed: *plyr, dplyr, tidyr, data.table, mixtools, suncalc, MuMIn, lme4, nlme, ggplot2*

# File structure
The following folder structure must be in place for the scripts to run properly:

..\data [contains telemetry and remote camera data]

..\data\outputs [for model and dataframe outputs]

..\figures [for output of final plots]

..\scripts [can be in any directory; no dependencies]

## Access to raw data
We are still conducting analyses with these datasets. We will upload our telemetry data to Movebank (https://www.movebank.org/) once all analyses have been published. If you are interested in using our data or collaborating with us, please contact Amanda Droghini: droghini (at) ualberta.ca.

# Authors
Amanda Droghini
