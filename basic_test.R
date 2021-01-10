

# Attach Packages
library(tidyverse)
library(janitor)
library(gridExtra)
library(raster)
library(ggforce)

# Load Data
checkpoint_data <- read_csv(here::here("context_capture_report_data.csv")) %>%
  janitor::clean_names() 
treatment_data <- read_csv(here::here("treatment_data.csv")) %>%
  janitor::clean_names()
checkpoint_locations <- read_csv(here::here("cp_locations.csv"), col_names = TRUE, skip = 1) %>%
  janitor::clean_names()  %>%
  dplyr::select(1:4) %>%
  drop_na()

# Constant Checkpoints
point_ids <- unique(checkpoint_data$point_id)
gcp_point_ids <- unique(checkpoint_data[checkpoint_data$type == "GCP",]$point_id)
constant_checkpoint_ids <- point_ids[!(point_ids %in% gcp_point_ids)]

# Group By Treatment
checkpoints_only <- checkpoint_data %>%
  filter(point_id %in% constant_checkpoint_ids)
#  mutate(treatment = paste(gcp, ppk, ppk_base, sep="_"))
treatment_summaries <- checkpoints_only %>%
  group_by(treatment) %>%
  summarize(q1 = quantile(x3d_error)[1], q2 = quantile(x3d_error)[2], q3 = quantile(x3d_error)[3], q4 = quantile(x3d_error)[4])


# Some graphs
treatment_order <- c("1 GCP + PPK", "2 GCPs + PPK", "3 GCPs", "4 GCPs", "5 GCPs", "6 GCPs", "7 GCPs", "8 GCPs", "JPLM", "AZU1", "VDCY", "Local")

horz_treatment_plot <- ggplot(data = checkpoints_only, aes(x = factor(treatment, level = treatment_order), 
                                                           y = horz_error)) +
  geom_boxplot() + 
  xlab('Treatment') + 
  ylab('Horizontal Error Magnitude (m)') + 
  ggtitle('Horizontal Checkpoint Error by Photogrammetry Routine') +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))
  

vert_treatment_plot <- ggplot(data = checkpoints_only, aes(x = factor(treatment, level = treatment_order), 
                                                           y = abs(vert_error))) +
  geom_boxplot() +
  xlab('Treatment') + 
  ylab('Vertical Error Magnitude (m)') + 
  ggtitle('Vertical Checkpoint Error by Photogrammetry Routine') +
  scale_y_continuous(expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(horz_treatment_plot, vert_treatment_plot, ncol = 1)



horz_point_plot <- ggplot(data = checkpoints_only, aes(x = point_id, y = horz_error)) +
  geom_boxplot(aes(group = cut_width(point_id, 1))) +
  xlab('Checkpoint ID') + 
  ylab('Horizontal Error Magnitude (m)') + 
  ggtitle('Horizontal Checkpoint Error by Checkpoint Number') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) 


vert_point_plot <- ggplot(data = checkpoints_only, aes(x = point_id, y = abs(vert_error))) +
  geom_boxplot(aes(group = cut_width(point_id, 1))) +
  xlab('Checkpoint ID') + 
  ylab('Vertical Error Magnitude (m)') + 
  ggtitle('Vertical Checkpoint Error by Checkpoint Number') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) 

grid.arrange(horz_point_plot, vert_point_plot, ncol = 1)



# Error values for each photogrammetry method, across all checkpoints
checkpoint_mean_errors <- checkpoints_only %>%
  group_by(treatment) %>%
  summarize(horz_mean = mean(horz_error, na.rm=TRUE),
            vert_mean = mean(abs(vert_error), na.rm=TRUE))
 
# add error ellipses and text showing mean and stdev for each subplot    
# also clean up caption, make sure newlines are good
ggplot() + 
  geom_point(data = checkpoints_only, aes(x=horz_error, y=abs(vert_error))) + 
  geom_point(data = checkpoint_mean_errors, aes(x=horz_mean, y=vert_mean),
             col = "red", size = 5, shape = 'x') +
  stat_ellipse(data = checkpoints_only, aes(x=horz_error, y=abs(vert_error)), col="red") + 
  facet_wrap(~treatment) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 12),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.margin=unit(0.05, "lines"),
        panel.border = element_rect(color = "gray3", fill = NA, size = 1),
        strip.background = element_rect(color = "gray3", size = 1)) +
  ggtitle('Checkpoint Location Error by Algorithm') + 
  xlab('Horizontal Error (m)') + 
  ylab('Vertical Error (m)') +
#  labs(caption = 'Horizontal and vertical error magnitude for each checkpoint, split by algorithm type. The mean errors \nacross all checkpoints are presented as red X marks. 95% confidence ellipses are presented in red.') + 
  scale_x_continuous(limits=c(0, 0.06), expand=c(0,0), breaks=c(0.01,0.03,0.05)) + 
  scale_y_continuous(limits=c(0, 0.15), expand=c(0,0))


# Error values for each individual checkpoint, across all processing methods tested
treatment_mean_errors <- checkpoints_only %>%
  group_by(point_id) %>%
  summarize(horz_mean = mean(horz_error, na.rm=TRUE),
            vert_mean = mean(abs(vert_error), na.rm=TRUE))
ggplot() + 
  geom_point(data = checkpoints_only, aes(x=horz_error, y=abs(vert_error), col=gcps >0)) +
  scale_colour_manual(name = 'gcps > 0', values = setNames(c('antiquewhite4','black'),c(T, F))) +
  geom_point(data = treatment_mean_errors, aes(x=horz_mean, y=vert_mean),
             col = "red", size = 5, shape = 'x') +
  stat_ellipse(data = checkpoints_only, aes(x=horz_error, y=abs(vert_error)), col="red") + 
  facet_wrap(~point_id) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 12),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.margin=unit(0.05, "lines"),
        panel.border = element_rect(color = "gray3", fill = NA, size = 1),
        strip.background = element_rect(color = "gray3", size = 1)) +
  ggtitle('Checkpoint Location Error by Checkpoint ID') + 
  xlab('Horizontal Error (m)') + 
  ylab('Vertical Error (m)') +
#  labs(caption = 'Horizontal and vertical error magnitude for each algorithm, split by checkpoint ID. The mean errors \nacross all treatments are presented as red X marks. 95% confidence ellipses are presented in red.') + 
  scale_x_continuous(limits=c(0, 0.06), expand=c(0,0), breaks=c(0.01,0.03,0.05)) +#, labels=c("0.1","0.2","0.3","0.4","0.5")) + 
  scale_y_continuous(limits=c(0, 0.15), expand=c(0,0))




checkpoints_with_locations <- checkpoints_only
checkpoints_with_locations$northing <- rep(0, nrow(checkpoints_with_locations))
checkpoints_with_locations$easting <- rep(0, nrow(checkpoints_with_locations))
for (row in 1:nrow(checkpoints_with_locations))
{
  point <- as.numeric(checkpoints_with_locations[row,]$point_id)
  checkpoints_with_locations[row,]$easting <- as.numeric(checkpoint_locations[checkpoint_locations$pt == point,]$easting_x)
  checkpoints_with_locations[row,]$northing <- as.numeric(checkpoint_locations[checkpoint_locations$pt == point,]$northing_y)
}
checkpoints_with_locations <- checkpoints_with_locations %>%
  filter(treatment == "8 GCPs")

#ggplot(data = checkpoints_with_locations) + 
#  geom_point(aes(x = northing, y = easting, col = x3d_error, size = abs(x3d_error)))
#  geom_arrow(aes(x = northing, y = easting, dx = vert_error*30, dy = horz_error*30))


ortho_r <- raster(here::here("..","output_imagery","ortho","ortho.png"), band=1)
ortho_g <- raster(here::here("..","output_imagery","ortho","ortho.png"), band=2)
ortho_b <- raster(here::here("..","output_imagery","ortho","ortho.png"), band=3)
ortho <- stack(ortho_r, ortho_g, ortho_b)
#tho <- crop(ortho, c(6565100, 6565350, 1873968, 1874216))
#plotRGB(ortho)

#arrows(checkpoints_with_locations$easting, checkpoints_with_locations$northing, 
#       checkpoints_with_locations$easting + checkpoints_with_locations$horz_error*1000, checkpoints_with_locations$northing + checkpoints_with_locations$vert_error*1000,
#       lwd = 3, length = 0.03, col = "red")

checkpoints_sp <- checkpoints_with_locations
coordinates(checkpoints_sp) =~ easting+northing
proj4string(checkpoints_sp) <- CRS(ortho@crs@projargs)
raster::shapefile(checkpoints_sp, here::here("..","output_imagery","checkpoints"), overwrite=TRUE)


# Build raster image data frame 
ortho_size <- ortho@nrows * ortho@ncols
ortho_df <- as.data.frame(ortho, xy = TRUE)
# Scale to use real coordinates instead of pixel values


# Get pixel limits of orthophoto
ortho_extent <- extent(ortho)
i_min <- extent(ortho_df)@xmin
i_max <- extent(ortho_df)@xmax
j_min <- extent(ortho_df)@ymin
j_max <- extent(ortho_df)@ymax
# Get spatial northing/easting limits of checkpoint locations
east_min <- 6565021 #min(checkpoints_with_locations$easting)
east_max <- 6565438 #max(checkpoints_with_locations$easting)
north_min <- 1873965 #min(checkpoints_with_locations$northing)
north_max <- 1874216 #max(checkpoints_with_locations$northing)
# Add real spatial data to raster dataframe
ortho_df$easting  <- east_min + (ortho_df$x - i_min) * (east_max - east_min) / (i_max - i_min)
ortho_df$northing <- north_min + (ortho_df$y - j_min) * (north_max - north_min) / (j_max - j_min)

# Spatially clip orthophoto around checkpoint region
ortho_df_clipped <- ortho_df %>%
  filter(northing > 1874025 & northing < 1874175) %>%
  filter(easting > 6565100 & easting < 6565350)

# Get checkpoints just from the best-performing method
checkpoints_8gcp <- checkpoints_with_locations %>% 
  filter(treatment=="8_gcps")


# Get dimensions for a temporary, hacky, howm-grown scalebar 
north_min <- min(ortho_df_clipped$y)
north_max <- max(ortho_df_clipped$y)
east_min <- min(ortho_df_clipped$x)
east_max <- max(ortho_df_clipped$x)
scale_loc_y <- north_min+(north_max-north_min)/15
scale_loc_x <- east_min+(east_max-east_min)/15
scale_length <- 100
scale_height <- 5
scalebar_df <- data.frame(x1 = scale_loc_x,
                          y1 = scale_loc_y,
                          x2 = scale_loc_x+scale_length,
                          y2 = scale_loc_y+scale_height)
error_plot <- ggplot() +
  geom_tile(data=ortho_df_clipped, 
            aes(x=easting, y=northing, fill=rgb(ortho.1/255,ortho.2/255,ortho.3/255))) +
  geom_circle(data=checkpoints_8gcp,
              aes(x0=easting,
                  y0=northing,
                  r=horz_error*200),
              col="red",
              size=1) +
  geom_circle(data=checkpoints_8gcp,
              aes(x0=easting,
                  y0=northing,
                  r=vert_error*200),
              col="white",
              size=1) +
  scale_fill_identity() + 
  xlab("Easting") + 
  ylab("Northing") + 
  theme_minimal() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
#  scalebar(data=scene_extent_polygon, location="bottomleft", transform=FALSE)
  geom_rect(scalebar_df, mapping=aes(xmin=x1, ymin=y1, xmax=x2, ymax=y2),
            col="white", size=5)




reproj_error <- ggplot() + 
  geom_boxplot(data = checkpoints_only, aes(y=rms_reprojection_err, x=treatment),
               fill = c(rep("brown",9), rep("orange",4)),
               notch = TRUE) + 
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 12),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        panel.margin=unit(0.05, "lines"),
        panel.border = element_rect(color = "gray3", fill = NA, size = 1),
        strip.background = element_rect(color = "gray3", size = 1)) +
  ggtitle('RMS Reprojection Error by Algorithm') + 
  xlab('Processing Algorithm') + 
  ylab('Reprojection Error (pixels)') # +
#  labs(caption = 'RMS reprojection error across all checkpoints for each processing method.')
