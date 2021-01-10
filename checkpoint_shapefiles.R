

# Attach Packages
library(tidyverse)
library(janitor)
library(gridExtra)
library(raster)
library(ggforce)

# Load Data
checkpoint_data <- read_csv(here::here("context_capture_report_data.csv")) %>%
  janitor::clean_names() 
checkpoint_locations <- read_csv(here::here("cp_locations.csv"), col_names = TRUE, skip = 1) %>%
  janitor::clean_names()  %>%
  dplyr::select(1:4) %>%
  drop_na()


for(treatment_current in unique(checkpoint_data$treatment))
{
  # Subset Checkpoints by Treatment
  checkpoints_with_locations <- checkpoint_data
  checkpoints_with_locations$northing <- rep(0, nrow(checkpoints_with_locations))
  checkpoints_with_locations$easting <- rep(0, nrow(checkpoints_with_locations))
  for (row in 1:nrow(checkpoints_with_locations))
  {
    point <- as.numeric(checkpoints_with_locations[row,]$point_id)
    checkpoints_with_locations[row,]$easting <- as.numeric(checkpoint_locations[checkpoint_locations$pt == point,]$easting_x)
    checkpoints_with_locations[row,]$northing <- as.numeric(checkpoint_locations[checkpoint_locations$pt == point,]$northing_y)
  }
  checkpoints_with_locations <- checkpoints_with_locations %>%
    filter(treatment == treatment_current)

  # Print Shapefile with These Checkpoints
  #   GCPs
  gcps_sp <- checkpoints_with_locations %>% filter(type == "GCP")
  if(nrow(gcps_sp) > 0)
  {
    print(nrow(gcps_sp))
    print(treatment_current)
    coordinates(gcps_sp) =~ easting+northing
    proj4string(gcps_sp) <- CRS(ortho@crs@projargs)
    print(gcps_sp)
    raster::shapefile(gcps_sp, here::here("..","output_imagery","gcps",sprintf("gcps_%s",treatment_current)))
  }
  #   Checkpoints
  checkpoints_sp <- checkpoints_with_locations %>% filter(type == "CP")
  coordinates(checkpoints_sp) =~ easting+northing
  proj4string(checkpoints_sp) <- CRS(ortho@crs@projargs)
  raster::shapefile(checkpoints_sp, here::here("..","output_imagery","checkpoints",sprintf("checkpoints_%s",treatment_current)), overwrite=TRUE)
}


