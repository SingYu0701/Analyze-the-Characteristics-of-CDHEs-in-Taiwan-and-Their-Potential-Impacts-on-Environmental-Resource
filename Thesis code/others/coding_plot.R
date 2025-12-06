library(ggplot2)
library(maps)
library(dplyr)
library(grid)
library(patchwork)

taiwan_map <- map_data("world", region = "Taiwan") %>%
  filter(long >= 119, long <= 123)

stations <- data.frame(
  name = c("Keelung", "Hengchun"),
  lon = c(121.7405, 120.7463),
  lat = c(25.1333, 22.0039)
)
central_mountain <- data.frame(
  long = c(
    120.55, 120.6, 120.65, 120.7, 120.75,
    120.8, 121.0, 121.2, 121.4, 121.5, 121.55, 121.5,
    121.4, 121.3, 121.1, 120.9, 120.75, 120.65, 120.6, 120.55
  ),
  lat = c(
    22.6, 22.55, 22.52, 22.5, 22.5,
    22.7, 23.0, 23.5, 24.0, 24.5, 24.8, 25.0,
    24.7, 24.5, 24.2, 23.8, 23.5, 23.0, 22.8, 22.6
  )
)
legend_grob <- grobTree(
  rectGrob(gp = gpar(fill = "white")),  
  pointsGrob(0.1, 0.5, pch = 15, size = unit(5, "mm"), gp = gpar(col = "gray40", fill = "gray40", alpha = 0.5)),
  textGrob("Central \nMountain", x = 0.25, y = 0.5, just = "left", gp = gpar(fontface = "bold", fontsize = 10, col = "gray20"))
)

p1<-ggplot() +

  geom_rect(aes(xmin = 119, xmax = 123, ymin = 21.5, ymax = 23.5),
            fill = "#fb9a66", alpha = 0.4) +
  geom_rect(aes(xmin = 119, xmax = 123, ymin = 23.5, ymax = 26),
            fill = "#fdbf2f", alpha = 0.4) +
  geom_polygon(data = taiwan_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", linewidth = 0.5) +
  geom_polygon(data = central_mountain, aes(x = long, y = lat),
               fill = "gray40", alpha = 0.5, color = NA) +
  geom_hline(yintercept = 23.5, color = "red", linetype = "dashed", linewidth = 1) +
  geom_point(data = stations, aes(x = lon, y = lat), color = "blue", size = 3) +
  geom_text(data = stations, aes(x = lon, y = lat, label = name),
            vjust = 0, hjust = -0.25, color = "blue") +
  annotate("text", x = 120.2, y = 25.4, label = "Subtropical Climate", 
           color = "black", size = 5, fontface = "bold") +
  annotate("text", x = 120, y = 21.7, label = "Tropical Climate", 
           color = "black", size = 5, fontface = "bold") +
  coord_fixed(1.2, xlim = c(119, 123), ylim = c(21.5, 25.5)) +
  labs(title = "Taiwan Climate Zones",
       subtitle = "Red dashed line: 23.5°N (Tropic of Cancer)",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  annotation_custom(legend_grob, xmin = 122, xmax = 123, ymin = 21.5, ymax = 22)
###########################################
library(ggplot2)
library(maps)

world_map <- map_data("world") %>%
  mutate(long360 = ifelse(long < 0, long + 360, long))



nino3_4 <- data.frame(xmin = 190, xmax = 240, ymin = -5, ymax = 5)   


taiwan <- data.frame(lon = 121, lat = 23.5)
taiwan$lon360 <- ifelse(taiwan$lon < 0, taiwan$lon + 360, taiwan$lon)
world_map2 <- world_map %>%
  mutate(long360_cut = ifelse(long360 > 260, NA, long360))  

p2<-ggplot() +

  geom_polygon(data = world_map2, aes(x = long360_cut, y = lat, group = group),
               fill = "grey90", color = "black", size = 0.2) +

  geom_rect(data = nino3_4, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "red", alpha = 0.3, color = "red") +

  geom_text(data = nino3_4, aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = "Nino3.4 Region"),
            color = "red", size = 4, hjust = 0.5, vjust = 0.5) +

  geom_point(data = taiwan, aes(x = lon360, y = lat), color = "red", size = 2) +
  geom_text(data = taiwan, aes(x = lon360 + 3, y = lat, label = "Taiwan"),
            color = "red", hjust = 0, size = 4) +

  coord_quickmap(xlim = c(120, 250), ylim = c(-15, 45)) +

  scale_x_continuous(
    breaks = seq(120, 250, by = 20),
    labels = function(x) {
      sapply(x, function(lon) {
        lon180 <- ifelse(lon > 180, lon - 360, lon)
        if(lon180 < 0) paste0(abs(lon180), "°W") else paste0(lon180, "°E")
      })
    }
  ) +
  theme_minimal() +
  labs(title = "Nino3.4 Region and Taiwan Climate Zones",
       x = "Longitude", y = "Latitude") +
  theme(
         plot.title = element_text( hjust = 0.5)
       )

p1 + p2 + plot_layout(ncol = 2, widths  = c(1,2.5))


# =====================================
library(ggplot2)
library(stars)
library(dplyr)
library(grid)


dem <- read_stars("Taiwan_DEM.tif")
dem_df <- as.data.frame(dem, xy = TRUE)
colnames(dem_df) <- c("lon", "lat", "elev")
dem_df$elev <- as.numeric(dem_df$elev)

central_mountain_df <- dem_df %>% filter(elev > 500)

stations <- data.frame(
  name = c("Keelung", "Hengchun"),
  lon = c(121.7405, 120.7463),
  lat = c(25.1333, 22.0039)
)


taiwan_map <- map_data("world", region = "Taiwan") %>%
  filter(long >= 119, long <= 123)


legend_grob <- grobTree(
  rectGrob(gp = gpar(fill = "white")),
  pointsGrob(0.1, 0.5, pch = 15, size = unit(5, "mm"), 
             gp = gpar(col = "gray40", fill = "gray40", alpha = 0.5)),
  textGrob("Central \nMountain", x = 0.25, y = 0.5, just = "left", 
           gp = gpar(fontface = "bold", fontsize = 10, col = "gray20"))
)


ggplot() +

  geom_rect(aes(xmin = 119, xmax = 123, ymin = 21.5, ymax = 23.5),
            fill = "#fb9a66", alpha = 0.4) +
  geom_rect(aes(xmin = 119, xmax = 123, ymin = 23.5, ymax = 26),
            fill = "#fdbf2f", alpha = 0.4) +
  

  geom_polygon(data = taiwan_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", linewidth = 0.5) +
  

  geom_tile(data = central_mountain_df, aes(x = lon, y = lat),
            fill = "gray40", alpha = 0.5) +
  

  geom_point(data = stations, aes(x = lon, y = lat), color = "blue", size = 3) +
  geom_text(data = stations, aes(x = lon, y = lat, label = name),
            vjust = 0, hjust = -0.25, color = "blue",size=5) +
  

  geom_hline(yintercept = 23.5, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 120.2, y = 25.4, label = "Subtropical Climate", 
           color = "black", size = 5, fontface = "bold") +
  annotate("text", x = 120, y = 21.7, label = "Tropical Climate", 
           color = "black", size = 5, fontface = "bold") +
  
  coord_fixed(1.2, xlim = c(119, 123), ylim = c(21.5, 25.5)) +

  labs(
       x = "Longitude", y = "Latitude") +
  
  theme_minimal() +
  

  annotation_custom(legend_grob, xmin = 122, xmax = 123, ymin = 21.5, ymax = 22)
