library(patchwork)

p1<-ggplot(north_spi1_values, aes(x = factor(month), y = factor(year), fill = spi)) +
  geom_tile(color = "grey80", size = 0.4) +
  scale_fill_gradient2(
    low = "blue",  high = "white",
    limits = c(-3.5, 3.5),
    name = "SPI1"
  ) +
  scale_y_discrete(
    expand = c(0, 0),
    limits = rev(levels(factor(north_spi1_values$year)))
  ) +
  scale_x_discrete(
    expand = c(0, 0),
    labels = month.abb
  ) +
  labs(
    title = "(a) SPI1 Heatmap of Keelung",
    x = "Month",
    y = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 6)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "grey20"),
    axis.text.y = element_text(size = 11, color = "grey20"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid = element_blank()
  )

p2<-ggplot(hwdi_full_north, aes(x = factor(month), y = factor(year))) +
  geom_tile(aes(fill = hwdi), color = "grey90", size = 0.4) +
  scale_fill_gradient(
    low = "white",
    high = "blue",
    name = "days",
    breaks = seq(floor(min(hwdi_full_north$hwdi)), ceiling(max(hwdi_full_north$hwdi)), by = 3)  # 每 2 天一個刻度
  ) +
  scale_y_discrete(
    expand = c(0, 0),
    limits = rev(levels(factor(hwdi_full_north$year)))
  ) +
  scale_x_discrete(
    expand = c(0, 0),
    labels = month.abb
  ) +
  labs(
    title = "(c) HWDI Heatmap of Keelung",
    x = "month",
    y = "year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 12)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "grey20"),
    axis.text.y = element_text(size = 11, color = "grey20"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid = element_blank()
  )


p3<-ggplot(south_spi1_values, aes(x = factor(month), y = factor(year), fill = spi)) +
  geom_tile(color = "grey80", size = 0.4) +
  scale_fill_gradient2(
    low = "blue",  high = "white",
    limits = c(-3.5, 3.5),
    name = "SPI1"
  ) +
  scale_y_discrete(
    expand = c(0, 0),
    limits = rev(levels(factor(south_spi1_values$year)))
  ) +
  scale_x_discrete(
    expand = c(0, 0),
    labels = month.abb
  ) +
  labs(
    title = "(b) SPI1 Heatmap of Hengchun",
    x = "Month",
    y = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 6)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "grey20"),
    axis.text.y = element_text(size = 11, color = "grey20"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid = element_blank()
  )
  
p4<-ggplot(hwdi_full_south, aes(x = factor(month), y = factor(year))) +
  geom_tile(aes(fill = hwdi), color = "grey90", size = 0.4) +  # 淡灰格線
  scale_fill_gradient(low = "white", high = "blue", name = "days") +
  scale_y_discrete(expand = c(0, 0), limits = rev(levels(factor(hwdi_full_south$year)))) +
  scale_x_discrete(expand = c(0, 0), labels = month.abb) +
  labs(
    title = "(d) HWDI Heatmap of Hengchun",
    x = "month",
    y = "year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 12)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "grey20"),
    axis.text.y = element_text(size = 11, color = "grey20"),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid = element_blank()
  )

(p1|p3) / (p2 | p4)
combined<-(p1|p3) / (p2 | p4)
ggsave("Fig4.jpg", plot = combined,
       width = 6000, height = 8000, units = "px", dpi = 600)
#cor
library(cowplot)
(p21 | p11) 
p1<-north_oni_hwdi_lag_df$plot
p2<-south_oni_hwdi_lag_df$plot
p3<-north_oni_spi_lag_df$plot
p4<-south_oni_spi_lag_df$plot
p5<-north_spi_hwdi_lag_df$plot
p6<-south_spi_hwdi_lag_df$plot
p1 <- p1 + theme(legend.position = "none")
p2 <- p2 + theme(legend.position = "none")
p3 <- p3 + theme(legend.position = "none")
p4 <- p4 + theme(legend.position = "none")
p5 <- p5 + theme(legend.position = "none")
p6 <- p6 + theme(legend.position = "none")

(p1 | p2)/ (p3 | p4)/ (p5 | p6) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combined<-(p1 | p2)/ (p3 | p4)/ (p5 | p6)
ggsave("Fig6.jpg", plot = combined,
       width = 6000, height = 8000, units = "px", dpi = 600)
##


p1<-plot(oni_beast_result, main="ONI beast decomposition", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","darkgreen","darkgreen","yellow","gray"))
p2<-plot(north_beast_result, main="SPI1 beast decomposition of Keelung", 
         vars=c("y","s","scp","t","tcp","slpsgn","error"),
         col = c("black", "red","red","darkgreen","darkgreen","yellow","gray"))
p3<-plot(beast_north_hwdi_result, main="HWDI beast decomposition of Keelung", 
         vars=c("y","s","scp","t","tcp","slpsgn","error"),
         col = c("black", "red","red","darkgreen","darkgreen","yellow","gray"))
p4<-plot(south_beast_result, main="SPI1 beast decomposition of Hengchun", 
         vars=c("y","s","scp","t","tcp","slpsgn","error"),
         col = c("black", "red","red","darkgreen","darkgreen","yellow","gray"))
p5<-plot(beast_south_hwdi_result, main="HWDI beast decomposition of Hengchun", 
         vars=c("y","s","scp","t","tcp","slpsgn","error"),
         col = c("black", "red","red","darkgreen","darkgreen","yellow","gray"))


