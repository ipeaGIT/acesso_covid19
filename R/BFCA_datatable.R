library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(dplyr)
library(gdistance)
library(kableExtra)

library(data.table)



####### load data ----------------------

# Load geography:
 load("Simulated Example.RData")

 
# Create simple features objects for population centers and clinics:
  # Clinics
  sim_clinics <- st_sf(Clinics_data, geometry_clinics, stringsAsFactors = TRUE)
  
  # Population centers
  sim_population <- st_sf(Population_data, geometry_population, stringsAsFactors = TRUE)


# Calculate the distance between these points as a measure of cost:
distance_matrix <- round(pointDistance(st_coordinates(sim_population), 
                                       st_coordinates(sim_clinics), 
                                       type='Euclidean', lonlat = FALSE))


head(Clinics_data)
head(Population_data)
head(distance_matrix)



####### threshold -------------------------


# Quantiles according to distance
quantiles_d <- quantile(as.vector(distance_matrix), probs = c(0, 1/10, 3/10, 5/10, 7/10, 8/10, 9/10))

dist_threshold <- quantile(as.vector(distance_matrix), probs = c(0, 1/10, 3/10, 5/10, 7/10, 8/10, 9/10))[[5]]





####### prepare travel matrix in long format  ----------------------


# convert matrix to data.frame
distance_matrix_long <- as.data.table(distance_matrix)

# rename columns
names(distance_matrix_long) <- c('clinic_1', 'clinic_2', 'clinic_3')
distance_matrix_long$id <- Population_data$id

# melt
distance_matrix_long <- data.table::melt(distance_matrix_long, id.vars='id', variable.name = "dest", value.name = "dist")

# add pop info
distance_matrix_long <- left_join(distance_matrix_long, Population_data)

# add  doctors info
Clinics_data$id <- c('clinic_1', 'clinic_2', 'clinic_3')
distance_matrix_long$dest <- as.character(distance_matrix_long$dest)
long_matrix <- left_join(distance_matrix_long, Clinics_data, by=c('dest'='id'))
setDT(long_matrix)
head(long_matrix)



####### calculate accessibility [data.table syntax with no comments, see commented version below]----------------------

# calculate impedance (binary)
long_matrix[, impedance := fifelse(dist <= dist_threshold, 1, 0) ]

# calculate weights i (normalized impedance by origin id)
long_matrix[, wi := impedance/sum(impedance), by=id]

# calculate weights j (normalized impedance by destination)
long_matrix[, wj := impedance/sum(impedance), by=dest]

## Step 1 - reaportion the demand to each each hospital proportionally to weight i
long_matrix[, pop_served := Population * wi, by= id ]

## Step 2 - calculate provider-to-population ration (ppr) at each destination
long_matrix[ , ppr := sum(Doctors[which(impedance>0)] / pop_served[which(impedance>0)]), by= dest]

## Step 3 - reaportion ppr at each origin proportionally to weight j
bfca <- long_matrix[, .(BFCA = sum(ppr * wj)), by= id]














####### calculate accessibility [data.table syntax]----------------------

# calculate impedance (binary)
long_matrix[, impedance := fifelse(dist <= dist_threshold, 1, 0) ]
head(long_matrix)

long_matrix[, sum(impedance), by=id]

# simple Cummulative Opportunity Metric
long_matrix[, sum(Population * impedance ), by=dest ]


# calculate weights i (normalized impedance by origin id)
long_matrix[, wi := impedance/sum(impedance), by=id]

# calculate weights j (normalized impedance by destination)
long_matrix[, wj := impedance/sum(impedance), by=dest]

# check if they sum one (keep overall population and service levels)
long_matrix[, sum(wi), by=id]
long_matrix[, sum( wj), by=dest]

long_matrix[, .(pop_served=sum(Population * wi, na.rm = TRUE)), by=id][,sum(pop_served)]
long_matrix[, .(service_level=sum(Doctors * wj, na.rm = TRUE)), by=id][,sum(service_level)]


## Step 1 - reaportion the demand to each each hospital proportionally to weight i
long_matrix[, pop_served := Population * wi, by= id ]
long_matrix[, sum(pop_served), by=id][,sum(V1)]
summary(long_matrix$pop_served)


## Step 2 - calculate provider-to-population ration (ppr) at each destination
long_matrix[ , ppr := sum(Doctors[which(impedance>0)] / pop_served[which(impedance>0)]), by= dest]
long_matrix[, .(ppr= mean(ppr, na.rm=T)), by=dest]


## Step 3 - reaportion ppr at each origin proportionally to weight j
bfca <- long_matrix[, .(BFCA = sum(ppr * wj)), by= id]



# Check that the sum of the accessibility is equal to the available level of service:
bfca[, sum(BFCA)]
long_matrix[, .(ppr= mean(ppr, na.rm=T)), by=dest][,sum(ppr)]

