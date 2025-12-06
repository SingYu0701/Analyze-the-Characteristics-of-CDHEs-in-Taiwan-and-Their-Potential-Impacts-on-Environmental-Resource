library(readxl)
library(zoo)
library(SPEI)
library(dplyr)
library(tidyr)
library(Rbeast)
library(lubridate)
library(writexl)
library(fitdistrplus)
####
rain_south <- read_excel("rain_south.xlsx")
descdist(rain_south$precipitation)

rain_south <- zoo(rain_south$precipitation, order.by = as.yearmon(paste(rain_south$year, rain_south$month), "%Y %m"))

rain_south <- ts(coredata(rain_south), start = c(1995, 1), frequency = 12)
set.seed(001)
rain_south_beast_result <- beast(rain_south,
                           freq = 12,
                           maxknot = 30,
                           numSam = 15000,
                           burnin = 3000)
plot(rain_south_beast_result, main="Rain beast decomposition of Hengchun")
rain_south_beast_result$trend$cp
rain_south_beast_result$season$cp
decimal_to_ym <- function(decimal_years) {
  years <- floor(decimal_years)
  months <- round((decimal_years - years) * 12) + 1  
  paste0(years, "-", sprintf("%02d", months))
}

sort(decimal_to_ym(rain_south_beast_result$season$cp))
sort(decimal_to_ym(rain_south_beast_result$trend$cp))



south_spi1 <- spi(rain_south, scale = 1, distribution = "Gamma")
south_spi1$fitted <-pmax(pmin(south_spi1$fitted, 3.5), -3.5)
plot(south_spi1)+ ggtitle("SPI1 of Hengchun")+ 
  theme(strip.text = element_blank())

south_spi1_values <- data.frame(
  date = time(south_spi1$fitted),
  spi = as.numeric(south_spi1$fitted)
) %>%
  mutate(
    date2 = as.Date(as.yearmon(date)),
    year = as.integer(format(date2, "%Y")),
    month = as.integer(format(date2, "%m"))
  ) %>%
  dplyr::select(year, month, spi)
south_spi1_values
ggplot(south_spi1_values, aes(x = factor(month), y = factor(year), fill = spi)) +
  geom_tile(color = "grey80", size = 0.4) +
  scale_fill_gradient2(
    low = "red",  high = "white",
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
    title = "SPI1 Heatmap of Hengchun",
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
south_spi_ts <- ts(south_spi1_values$spi, start = c(1995, 1), frequency = 12)
set.seed(001)
south_beast_result <- beast(south_spi_ts,
                            freq = 12,
                            maxknot = 30,       
                            numSam = 15000,     
                            burnin = 3000)

plot(south_beast_result, main="SPI1 beast decomposition of Hengchun")
plot(south_beast_result, main="SPI1 beast decomposition of Hengchun", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))
sort(decimal_to_ym(south_beast_result$season$cp))
sort(decimal_to_ym(south_beast_result$trend$cp))

#write_xlsx(south_spi1_values, path = "south_spi1_values.xlsx")
####
temp_south <- read_excel("all_data_south.xlsx")
temp_south <- temp_south %>%
  mutate(
    mmdd = format(date, "%m-%d"),
    year = year(date),
    month = month(date)
  )


thresholds_south <- temp_south %>%
  group_by(mmdd) %>%
  filter(year >= 1995 & year <= 2014) %>%
  summarise(threshold_95_south = quantile(temperature, 0.95, na.rm = TRUE))

temp_south <- temp_south %>%
  left_join(thresholds_south, by = "mmdd") %>%
  mutate(
    exceed = ifelse(is.na(temperature), FALSE, temperature > threshold_95_south)
  )



temp_south <- temp_south %>%
  mutate(
    exceed_flag = ifelse(exceed, 1, 0),
    date_diff = c(NA, diff(date)),
    new_group = ifelse(is.na(date_diff) | date_diff > 1 | exceed_flag == 0, 1, 0)
  )
temp_south$wave_id <- cumsum(temp_south$new_group)


events <- temp_south %>%
  filter(exceed_flag == 1) %>%
  group_by(wave_id) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    duration = as.numeric(end_date - start_date) + 1
  ) %>%
  filter(duration >= 3)
events <- events %>%
  mutate(
    start_date = as.Date(start_date),
    end_date = as.Date(end_date)
  )
events_expanded <- events %>%
  rowwise() %>%
  mutate(dates = list(seq.Date(start_date, end_date, by = "day"))) %>%
  unnest(cols = c(dates))


events_expanded <- events_expanded %>%
  mutate(
    year = year(dates),
    month = month(dates)
  )


hwdi_monthly <- events_expanded %>%
  group_by(year, month) %>%
  summarise(hwdi = n(), .groups = "drop") %>%
  arrange(year, month)



all_months_south <- temp_south %>%
  distinct(year, month)

hwdi_full_south <- all_months_south %>%
  left_join(hwdi_monthly, by = c("year", "month")) %>%
  mutate(hwdi = ifelse(is.na(hwdi), 0, hwdi)) %>%
  arrange(year, month)

hwdi_full_south
#write_xlsx(hwdi_full_south, path = "hwdi_south.xlsx")

hwdi_south_ts <- ts(hwdi_full_south$hwdi, start = c(min(hwdi_full_south$year), min(hwdi_full_south$month)), frequency = 12)
set.seed(011)
beast_south_hwdi_result <- beast(hwdi_south_ts,
                                 freq = 12)

plot(beast_south_hwdi_result, main="HWDI beast decomposition of Hengchun")
plot(beast_south_hwdi_result, main="HWDI beast decomposition of Hengchun", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))
sort(decimal_to_ym(beast_south_hwdi_result$season$cp))
sort(decimal_to_ym(beast_south_hwdi_result$trend$cp))

library(scales) 
library(ggplot2)
ggplot(hwdi_full_south, aes(x = factor(month), y = factor(year))) +
  geom_tile(aes(fill = hwdi), color = "grey90", size = 0.4) +
  scale_fill_gradient(low = "white", high = "red", name = "days") +
  scale_y_discrete(expand = c(0, 0), limits = rev(levels(factor(hwdi_full_south$year)))) +
  scale_x_discrete(expand = c(0, 0), labels = month.abb) +
  labs(
    title = "HWDI Heatmap of Hengchun",
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



cat("\n--- Seasonal amplitude analysis (Hengchun HWDI) ---\n")
south_hwdi_season_df <- data.frame(
  date = as.Date(as.yearmon(beast_south_hwdi_result[["time"]])),
  season = beast_south_hwdi_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date))


south_hwdi_season_avg <- south_hwdi_season_df %>%
  group_by(year) %>%
  summarise(mean_season = mean(season, na.rm = TRUE))


south_hwdi_amp_df <- south_hwdi_season_df %>%
  left_join(south_hwdi_season_avg, by = "year") %>%
  mutate(amplitude = season - mean_season)


south_hwdi_amp_year <- south_hwdi_amp_df %>%
  group_by(year) %>%
  summarise(mean_amp = mean(abs(amplitude), na.rm = TRUE))


south_hwdi_corr_amp <- inner_join(south_hwdi_amp_year, co2_sel, by = "year")
south_hwdi_corr_amp_test <- cor.test(south_hwdi_corr_amp$mean_amp,
                                     south_hwdi_corr_amp$mean,
                                     method = "spearman")
cat("HWDI amplitude - Spearman r:", south_hwdi_corr_amp_test$estimate, "\n")
cat("p-value:", south_hwdi_corr_amp_test$p.value, "\n")




cat("\n--- Seasonal amplitude analysis (Keelung HWDI) ---\n")
north_hwdi_season_df <- data.frame(
  date = as.Date(as.yearmon(beast_north_hwdi_result[["time"]])),
  season = beast_north_hwdi_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date))

north_hwdi_season_avg <- north_hwdi_season_df %>%
  group_by(year) %>%
  summarise(mean_season = mean(season, na.rm = TRUE))

north_hwdi_amp_df <- north_hwdi_season_df %>%
  left_join(north_hwdi_season_avg, by = "year") %>%
  mutate(amplitude = season - mean_season)

north_hwdi_amp_year <- north_hwdi_amp_df %>%
  group_by(year) %>%
  summarise(mean_amp = mean(abs(amplitude), na.rm = TRUE))

north_hwdi_corr_amp <- inner_join(north_hwdi_amp_year, co2_sel, by = "year")
north_hwdi_corr_amp_test <- cor.test(north_hwdi_corr_amp$mean_amp,
                                     north_hwdi_corr_amp$mean,
                                     method = "spearman")
cat("HWDI amplitude - Spearman r:", north_hwdi_corr_amp_test$estimate, "\n")
cat("p-value:", north_hwdi_corr_amp_test$p.value, "\n")
