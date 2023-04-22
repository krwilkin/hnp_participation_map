
# load libraries
library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(glue)
library(mapview)
library(spdep)


# define directories
acsdir = '/Users/kwilkin/Documents/acs'
acs5dir = file.path(acsdir, 'api_data', 'acs5_data')
mapdir = '/Users/kwilkin/Documents/maps'
hnpdir = file.path(mapdir, '..', 'hnp')


# load hnp frequency distribution by geoid
hnpgeo = readRDS(file.path(hnpdir, 'hnp_freq_by_tract_aug2021.rds'))


# load acs5 tract-level summary data
acs5dat = do.call(rbind,
                  map(dir(acs5dir)[grep('\\.dat$', dir(acs5dir))] %>% as.list(),
                      ~ read_delim(file.path(acs5dir, .x),
                                   delim = '\t',
                                   col_names = TRUE) %>%
                          select(-c(state, county, tract))))

saveRDS(acs5dat, file.path(acsdir, 'acs5_tract_data.rds'))
#acs5dat = readRDS(file.path(acsdir, 'acs5_tract_data.rds'))


# load index of cbsas (2020 definitions)
cbsaref = readxl::read_xls(file.path(mapdir, 'list1_2020.xls'),
                           skip = 2) %>%
  rename_all(.funs = list(~ str_replace_all(., '\\s', '')))

saveRDS(cbsaref, file.path(mapdir, 'cbsa_2020_delineation_file.rds'))
#cbsaref = readRDS(file.path(mapdir, 'cbsa_2020_delineation_file.rds'))


# load nevada and california shapefiles
nv_ca_tracts = 
  do.call(bind_rows,
          map(c('32', '06') %>% as.list(),
              ~ read_sf(file.path(mapdir,
                                  'tracts',
                                  glue('tl_2020_{.x}_tract'),
                                  glue('tl_2020_{.x}_tract.shp')))))

# get hnp tracts
hnp_tracts = nv_ca_tracts %>%
  filter(GEOID %in% hnpgeo$geoid)


# reno csa tracts (plot to check)
reno_csa_counties = cbsaref %>%
  filter(CSACode == '456') %>%
  mutate(fips = paste0(FIPSStateCode, FIPSCountyCode)) %>%
  pluck('fips')

# subset tracts shapefile to reno-carson-fernley csa
reno_csa_tracts = nv_ca_tracts %>%
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
  filter(fips %in% reno_csa_counties) %>%
  select(-fips)


# plot
ggplot() +
  geom_sf(data = reno_csa_tracts, fill = 'white', color = 'grey') +
  theme_void()

mapview(
  reno_csa_tracts, 
  col.regions = 'red',
  legend = FALSE
)

# erase water features from reno csa tracts and plot
reno_csa_nowater = reno_csa_tracts %>%
  filter(ALAND > 0)

mapview(
  reno_csa_nowater,
  col.regions = 'red',
  alpha.regions = 0.4,
  legend = FALSE
)

ggplot(data = reno_csa_tracts %>%
         left_join(.,
                   hnpgeo %>%
                     select(geoid, freq),
                   by = c('GEOID' = 'geoid'))) +
  geom_sf(aes(fill = freq), color = NA) +
  scale_fill_viridis_c() +
  theme_void()


hnp_tract_freq = reno_csa_tracts %>%
  left_join(.,
            hnpgeo %>%
              select(geoid, freq),
            by = c('GEOID' = 'geoid')) %>%
  filter(!is.na(freq))

# mapview of the frequency of participants by tract (heat map)
mapview(
  hnp_tract_freq,
  zcol = 'freq',
  alpha.regions = 0.7,
  legend = TRUE,
  layer.name = 'HNP Participants'
)


# poverty clusters
# NOTES: define areas of high poverty (high incidence of family income < .5 of
#        the poverty level) and poverty (family income >=.5 - <1.0 percnet of
#        poverty). poverty of family based on income in last 12 months
reno_csa_poverty = reno_csa_nowater %>%
  left_join(.,
            acs5dat %>%
              rowwise() %>%
              mutate(geoid = str_extract_all(GEO_ID, '(?<=US)\\d+')[[1]],
                     poverty = (B17026_003E + B17026_004E),
                     highpov = B17026_002E,
                     pov1 = (B17026_003E + B17026_004E + B17026_002E),
                     totfams = B17026_001E) %>%
              ungroup() %>%
              select(geoid, poverty, highpov, pov1, totfams) %>%
              mutate(povrate = poverty/totfams,
                     hpvrate = highpov/totfams,
                     pov1rate = pov1/totfams),
            by = c('GEOID' = 'geoid'))



# (a) get neighbors
neighbors = poly2nb(reno_csa_poverty, queen = TRUE)

summary(neighbors)

neighbors[[1]]

# (b) spatial weights matrix
weights = nb2listw(neighbors, style = "W")

weights$weights[[1]]

# (c) spatial lag
#reno_csa_poverty$hpov_lag_est = lag.listw(weights, reno_csa_poverty$highpov)
#reno_csa_poverty$pov_lag_est = lag.listw(weights, reno_csa_poverty$poverty)
reno_csa_poverty$pov1_lag_est = lag.listw(weights, reno_csa_poverty$pov1)

# plot
ggplot(reno_csa_poverty, aes(x = highpov, y = hpov_lag_est)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(color = "red") + 
  theme_minimal() + 
  labs(title = "Families in High Poverty by Census Tract, Reno-Carson-Fernley, NV CSA",
       x = "Families in High Poverty (<.5 of Poverty Level)",
       y = "Spatial lag, high poverty", 
       caption = "Data source: 2016-2020 ACS 5-year estimates.\nSpatial relationships based on queens-case polygon contiguity.")

ggplot(reno_csa_poverty, aes(x = highpov, y = hpov_lag_est)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(color = "red") + 
  theme_minimal() + 
  labs(title = "Families in Poverty by Census Tract, Reno-Carson-Fernley, NV CSA",
       x = "Families in Poverty (.5-.99 of Poverty Level)",
       y = "Spatial lag, poverty", 
       caption = "Data source: 2016-2020 ACS 5-year estimates.\nSpatial relationships based on queens-case polygon contiguity.")

ggplot(reno_csa_poverty, aes(x = pov1, y = pov1_lag_est)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(color = "red") + 
  theme_minimal() + 
  labs(title = "Families in Poverty by Census Tract, Reno-Carson-Fernley, NV CSA",
       x = "Families in Poverty (<1.0 of Poverty Level)",
       y = "Spatial lag, poverty", 
       caption = "Data source: 2016-2020 ACS 5-year estimates.\nSpatial relationships based on queens-case polygon contiguity.")

# (d) moran's test of H0: spatial randomness of poverty
#moran.test(reno_csa_poverty$highpov, weights)
#moran.test(reno_csa_poverty$poverty, weights)
moran.test(reno_csa_poverty$pov1, weights) # p-value suggest reject H0


# (e) local spatial autocorrelation: used to identify hot spots
# NOTE: Getis-Ord local G statistic
# For Gi*, re-compute the weights with `include.self()`
localg_weights = nb2listw(include.self(neighbors))

reno_csa_poverty$localG <- localG(reno_csa_poverty$pov1, localg_weights)

ggplot(reno_csa_poverty) + 
  geom_sf(aes(fill = localG), color = NA) + 
  scale_fill_distiller(palette = "RdYlBu") + 
  theme_void() + 
  labs(fill = "Local Gi* statistic")

mapview(reno_csa_poverty,
        zcol = 'localG')

# (f) choose cutoffs for local G statistic and plot
# NOTE: cutoffs determine 'hot spots'
ggplot(reno_csa_poverty %>%
         mutate(hotspot = case_when(
           localG >= quantile(reno_csa_poverty$localG, .97) ~ "Poverty cluster",
           localG <= quantile(reno_csa_poverty$localG, 0.01) ~ "Wealth cluster",
           TRUE ~ "Not significant"
         ))) + 
  geom_sf(aes(fill = hotspot), color = "grey90", size = 0.1) + 
  scale_fill_manual(values = c("grey", "red", "blue")) + 
  theme_void()

povertyTracts = reno_csa_poverty %>%
  filter(localG >= quantile(reno_csa_poverty$localG, .97))

mapview(povertyTracts)


# --------------------
# SUMMARY CALCULATIONS
# --------------------
# average tract size
summary(hnp_tract_freq$freq)



# get a count of how many hnp participants the reno-carson-fernley csa covers
summarise(hnpgeo %>%
            mutate(fips = paste0(state, county)) %>%
            filter(fips %in% reno_csa_counties),
          tot_hnp_csa = sum(freq)) %>%
  mutate(pct_hnp_csa = 100 * tot_hnp_csa / sum(hnpgeo$freq))





# plot tracts represented by hnp
ggplot() + 
  geom_sf(data = nv_ca_tracts, fill = 'white', color = 'grey') +
  geom_sf(data = hnp_tracts, fill = NA, color = 'red') +
  theme_void()
