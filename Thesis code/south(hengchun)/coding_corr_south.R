library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(reshape2)

hwdi_south <- read_excel("hwdi_south.xlsx")
south_spi1 <- read_excel("south_spi1_values.xlsx")


all_south <- south_spi1 %>%
  inner_join(hwdi_south, by = c("year", "month")) %>%
  arrange(year, month) %>%
  rename(
    spi = spi,
    hwdi = hwdi,
  ) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))
all_south <- all_south %>%
  mutate(
    spi = as.numeric(spi),
    hwdi = as.numeric(hwdi),
  )
b1<-hist(all_south$spi,main = "(c) Histogram of SPI1 Hengchun" ,xlab = "SPI1")
abline(v = -1, col = "red", lwd = 2)
d1<-hist(all_south$hwdi,main = "(d) Histogram of HWDI Hengchun" ,xlab = "HWDI",breaks = seq(floor(min(all_south$hwdi, na.rm = TRUE)),
                                                                                   ceiling(max(all_south$hwdi, na.rm = TRUE)),
                                                                                   by = 1))
abline(v = 1, col = "red", lwd = 2)

#write_xlsx(all_south, path = "all_south.xlsx")

oni<-read.csv("oni.csv")

df_all_south <- south_spi1 %>%
  inner_join(hwdi_south, by = c("year", "month")) %>%
  inner_join(oni, by = c("year", "month")) %>%
  arrange(year, month) %>%
  rename(
    spi = spi,
    hwdi = hwdi,
    oni = oni
  ) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))
df_all_south <- df_all_south %>%
  mutate(
    spi = as.numeric(spi),
    hwdi = as.numeric(hwdi),
    oni = as.numeric(oni)
  )

cor_south <- cor(
  df_all_south[, c("spi", "hwdi", "oni")],
  method = "pearson",
  use = "complete.obs"
)
print(cor_south)
cor_melt <- melt(cor_south)

cor_melt$Var1 <- factor(toupper(cor_melt$Var1), levels = c("SPI", "HWDI", "ONI"))
cor_melt$Var2 <- factor(toupper(cor_melt$Var2), levels = c("SPI", "HWDI", "ONI"))

ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "green", 
                       mid = "white",
                       high = "blue",
                      limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  geom_text(aes(label = round(value, 3)), color = "black", size = 6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=18),
        axis.text.y = element_text(size=18),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  guides(fill = guide_colorbar(
    barwidth = 1.2,  
    barheight = 10,   
    title.position = "top",
    title.hjust = 0.5
  ))+
  labs(title = "(b) Correlation Heatmap (Hengchun)",
       x = NULL, y = NULL)


library(ggplot2)
library(Hmisc)

calc_lag_corr_plot <- function(x, y, max_lag = 12, method = "spearman", title = "Lag correlation plot", show_legend = TRUE) {
  library(Hmisc)
  library(ggplot2)
  
  lag_values <- numeric(max_lag + 1)
  p_values <- numeric(max_lag + 1)
  
  for (lag in 0:max_lag) {
    x_sub <- x[1:(length(x) - lag)]
    y_sub <- y[(lag + 1):length(y)]
    valid_idx <- complete.cases(x_sub, y_sub)
    x_sub <- x_sub[valid_idx]
    y_sub <- y_sub[valid_idx]
    
    if (length(x_sub) > 2) {
      rc <- rcorr(x_sub, y_sub, type = method)
      lag_values[lag + 1] <- rc$r[1, 2]
      p_values[lag + 1] <- rc$P[1, 2]
    } else {
      lag_values[lag + 1] <- NA
      p_values[lag + 1] <- NA
    }
  }
  
  significance <- rep("no", length(p_values))
  significance[p_values < 0.1] <- "weak"
  significance[p_values < 0.05] <- "yes"
  
  lag_df <- data.frame(
    lag = 0:max_lag,
    correlation = lag_values,
    p_value = p_values,
    significance = significance
  )
  lag_df$significance <- factor(lag_df$significance, levels = c("yes", "weak", "no"))
  
  p <- ggplot(lag_df, aes(x = lag, y = correlation)) +
    geom_line(color = "gray40") +
    geom_point(aes(color = significance, shape = significance), size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("yes" = "blue", "weak" = "lightblue", "no" = "black"), na.value = "gray") +
    scale_shape_manual(values = c("yes" = 16, "weak" = 16, "no" = 16)) +
    labs(
      title = title,
      x = "Lag (months)",
      y = paste0(method, " correlation"),
      color = "Significant",
      shape = "Significant"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = if (show_legend) "bottom" else "none"
    )
  
  return(list(plot = p, df = lag_df))
}
south_oni_hwdi_lag_df <- calc_lag_corr_plot(df_all_south$oni, df_all_south$hwdi, max_lag = 6,  title = "(b) ONI leads HWDI lag correlation of Hengchun", show_legend = FALSE)

south_oni_spi_lag_df <- calc_lag_corr_plot(df_all_south$oni, df_all_south$spi, max_lag = 6, title = "(d) ONI leads SPI lag correlation of Hengchun", show_legend = FALSE)

south_spi_hwdi_lag_df <- calc_lag_corr_plot(df_all_south$spi, df_all_south$hwdi, max_lag = 6, title = "(f) SPI leads HWDI lag correlation of Hengchun", show_legend = FALSE)


