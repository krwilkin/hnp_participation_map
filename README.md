# hnp_participation_map
Map of HNP participation by Census tract within the Reno-Carson City-Fernley Consolidated Statistical Area. Made in R and output to HTML using the MapView plugin.

This repository contains working R code (hnp_tract_spatial_work.R) to produce the map, as well as R code to identify clusters of Census tracts with relatively high poverty rates. Input data are from the American Community Survey 5-year files for 2020; later updates to this code will use 2021 vintage 5-year files.

In addition, the file "hnp_zip_distance_med_centers.R" presents R code to calculate the distance (Euclidean, in meters) from the Renown Hospital Center in downtown Reno, NV to all five-digit ZIP code tabulation area (ZCTA) centroids.
