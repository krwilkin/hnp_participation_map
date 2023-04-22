# r script to calculate distance of hnp participants to renown/umc med centers

# NOTES: objective is to create a variable representing the distance (euclidean)
#        from hnp participants to the renown and umc med centers. this variable
#        may have predictive power in explaining whether or not a participant has
#        a medical record. predicted probability of having a medical record could
#        be used as a weighting variable (specifically, the inverse of the prob)
#        to adjust estimates for potential non-random selection into having a
#        medical record.
#
#        i used the google maps geocoder to manually get coordinates for both the
#        renown and umc medical centers. i use the centroids of census zctas to
#        to calculate the straight-line distance for each participant to each med
#        center. zctas were matched on zip code from the participants consents; for
#        those that did not match, most had a med record and were geocoded to the
#        zcta. zcta5 are based on census 2020 definitions.

# import libraries
library(dplyr)
library(tibble)
library(readr)
library(stringr)
library(purrr)
library(rlang)
library(ggplot2)
library(tidyr)
library(glue)
#library(tidyverse)
library(sf)
library(units)


# set root (working) directory
setwd('//cgm-srv-1/DataShare/Kelly')
#setwd('/Users/kwilkin/Documents/hnp')


# define other directory paths
hnpDir = 'E://hnp'
outDir = 'E://hnp_fluff'
jimDir = file.path('From_Jim')


# load user-defined functions
source(file.path('hnp_fluff', 'functions.R'))


# import monolith data
monodat = readRDS(file.path(hnpDir, 'monolith_20220920_ren_umc_clean_geo.rds'))

# create tibble of med center coordinates
medcoords = tibble('name' = c('Renown', 'UMC'),
                   'lat' = c(39.525629, 36.159713),
                   'lon' = c(-119.796568, -115.167332))

saveRDS(medcoords, file.path(hnpDir, 'ren_umc_coordintates.rds'))
#medcoords = readRDS(file.path(hnpDir, 'ren_umc_coordintates.rds'))
#medcoords = readRDS('ren_umc_coordintates.rds')

# pre-process zcta centroids
# NOTE: convert lat/lon to numeric from character, keep unique cases
hnpZips = monodat %>%
  filter(!is.na(ZCTA5CE20)) %>%
  select(ZCTA5CE20, INTPTLAT20, INTPTLON20) %>%
  distinct() %>%
  mutate(INTPTLAT20 = str_replace_all(INTPTLAT20, '\\+', '')) %>%
  mutate_at(vars(INTPTLAT20, INTPTLON20),
            .funs = list(~ as.numeric(.)))

saveRDS(hnpZips, file.path(hnpDir, 'hnp_zcta5_centroids.rds'))

#hnpZips = readRDS(file.path(hnpDir, 'hnp_zcta5_centroids.rds'))
#hnpZips = readRDS('hnp_zcta5_centroids.rds')


# SPATIAL OBJECTS
# create spatial object from hospital coordinates
medSp = st_as_sf(medcoords,
                 coords = c('lon', 'lat'),
                 crs = 4326)


# create spatial object from zcta5 centroids
zipSp = st_as_sf(hnpZips,
                 coords = c('INTPTLON20', 'INTPTLAT20'),
                 crs = 4269)


# DISTANCE CALCULATIONS
# NOTE: distances are from centroids of zip codes. default returns in meters.
#       convert medSp points crs to same as zipSp on the fly using st_transform
# distance to renown
ren_dist = st_distance(zipSp,
                       st_transform(slice(medSp, 1),
                                    crs = st_crs(zipSp)))

#set_units(ren_dist, 'km')

# distance to umc
umc_dist = st_distance(zipSp,
                       st_transform(slice(medSp, 2),
                                    crs = st_crs(zipSp)))

# convert to tibble and join with zip code references
med_dist = bind_cols(hnpZips %>%
                       select(ZCTA5CE20),
                     ren_dist %>%
                       as_tibble() %>%
                       rename('dist_ren_m' = 'value'),
                     umc_dist %>%
                       as_tibble() %>%
                       rename('dist_umc_m' = 'value'))


# save to rds
saveRDS(med_dist, file.path(hnpDir, 'hnp_zcta_med_distance.rds'))
#saveRDS(med_dist, 'hnp_zcta_med_distance.rds')

