library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(reshape2)

hwdi_north <- read_excel("hwdi_north.xlsx")
north_spi1 <- read_excel("north_spi1_values.xlsx")


all_north <- north_spi1 %>%
  inner_join(hwdi_north, by = c("year", "month")) %>%
  arrange(year, month) %>%
  rename(
    spi = spi,
    hwdi = hwdi,
  ) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))
all_north <- all_north %>%
  mutate(
    spi = as.numeric(spi),
    hwdi = as.numeric(hwdi),
  )

a1<-hist(all_north$spi,main = "(a) Histogram of SPI1 Keelung" ,xlab = "SPI1")
abline(v = -1, col = "red", lwd = 2)
c1<-hist(all_north$hwdi,main = "(b) Histogram of HWDI Keelung" ,xlab = "HWDI")
abline(v = 1, col = "red", lwd = 2)


oni<-read.csv("oni.csv")

#########################

df_all_north <- north_spi1 %>%
  inner_join(hwdi_north, by = c("year", "month")) %>%
  inner_join(oni, by = c("year", "month")) %>%
  arrange(year, month) %>%
  rename(
    spi = spi,
    hwdi = hwdi,
    oni = oni
  ) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))
df_all_north <- df_all_north %>%
  mutate(
    spi = as.numeric(spi),
    hwdi = as.numeric(hwdi),
    oni = as.numeric(oni)
  )


cor_north <- cor(
  df_all_north[, c("spi", "hwdi", "oni")],
  method = "pearson",
  use = "complete.obs"
)
print(cor_north)
cor_melt <- melt(cor_north)

cor_melt$Var1 <- factor(toupper(cor_melt$Var1), levels = c("SPI", "HWDI", "ONI"))
cor_melt$Var2 <- factor(toupper(cor_melt$Var2), levels = c("SPI", "HWDI", "ONI"))


ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2( low = "green", 
                        mid = "white",
                        high = "blue",
                      limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  geom_text(aes(label = round(value, 3)), color = "black", size = 6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,size=18),
        axis.text.y = element_text(size=18),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  labs(title = "(a) Correlation Heatmap (Keelung)",
       x = NULL, y = NULL)

#low = "#0033A0", 
#mid = "white",
#high = "#8B0000",


library(Hmisc)
calc_lag_corr_plot <- function(x, y, max_lag = 24, method = "spearman", title = "Lag correlation plot", show_legend = TRUE) {
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
north_oni_hwdi_lag_df <- calc_lag_corr_plot(df_all_north$oni, df_all_north$hwdi, max_lag = 6,  title = "(a) ONI leads HWDI lag correlation of Keelung", show_legend = FALSE)

north_oni_spi_lag_df <- calc_lag_corr_plot(df_all_north$oni, df_all_north$spi, max_lag = 6, title = "(c) ONI leads SPI lag correlation of Keelung", show_legend = FALSE)

north_spi_hwdi_lag_df <- calc_lag_corr_plot(df_all_north$spi, df_all_north$hwdi, max_lag = 6, title = "(e) SPI leads HWDI lag correlation of Keelung", show_legend = FALSE)


