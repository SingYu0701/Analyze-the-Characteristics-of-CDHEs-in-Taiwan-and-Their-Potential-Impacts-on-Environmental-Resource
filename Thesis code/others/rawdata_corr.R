library(dplyr)
library(lubridate)
library(zoo)
library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)  # 用來排版多圖

# 讀取資料
north_temp_data <- read_xlsx("D:/成大/資源所/SPI+HWDI/data/north/all_data_north.xlsx")
rain_north <- read_excel("D:/成大/資源所/SPI+HWDI/data/north/rain_north.xlsx")
south_temp_data <- read_xlsx("D:/成大/資源所/SPI+HWDI/data/south/all_data_south.xlsx")
rain_south <- read_excel("D:/成大/資源所/SPI+HWDI/data/south/rain_south.xlsx")

# 處理北部降雨量
rain_north <- rain_north %>% 
  rename(rainfall = precipitation, date = ...4) %>% 
  mutate(date = as.Date(date))
north_temp_data <- north_temp_data %>%
  mutate(date = as.Date(date))
# 處理南部降雨量
rain_south <- rain_south %>% 
  rename(rainfall = precipitation, date = ...4) %>% 
  mutate(date = as.Date(date))
south_temp_data <- south_temp_data %>%
  mutate(date = as.Date(date))
# 北部折線圖函數
plot_temp <- function(data, title){
  ggplot(data, aes(x = date, y = temperature)) +
    geom_line(color = "red") +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(x = "Year", y = "Temperature (°C)", title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(size = 12),   # X 軸字體
      axis.text.y = element_text(size = 12),   # Y 軸字體
      axis.title.x = element_text(size = 14),  # X 軸標題
      axis.title.y = element_text(size = 14)   # Y 軸標題
    )
}

plot_rain <- function(data, title){
  ggplot(data, aes(x = date, y = rainfall)) +
    geom_line(color = "blue") +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(x = "Year", y = "Rainfall (mm)", title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )
}
# 北部圖
p1 <- plot_temp(north_temp_data, "(b) Daily max temperature of Keelung")
p2 <- plot_rain(rain_north, "(a) Monthly Rainfall of Keelung")
p2/p1
# 南部圖
p3 <- plot_temp(south_temp_data, "(b) Daily max temperature of Hengchun")
p4 <- plot_rain(rain_south, "(a) Monthly Rainfall of Hengchun")
p4/p3


co2_data <- data.frame(
  year = 1979:2024,
  mean = c(336.85,338.91,340.11,340.86,342.53,344.07,345.54,346.97,348.68,
           351.16,352.79,354.06,355.39,356.09,356.83,358.33,360.17,361.93,
           363.05,365.7,367.79,368.96,370.57,372.58,375.14,376.95,378.98,
           381.15,382.9,385.02,386.5,388.75,390.62,392.65,395.4,397.34,
           399.65,403.06,405.22,407.61,410.07,412.44,414.7,417.08,419.36,422.8)
)
oni <- c(
  0.96,0.72,0.53,0.3,0.14,-0.03,-0.24,-0.54,-0.81,-0.97,-1,-0.98,
  -0.9,-0.75,-0.59,-0.39,-0.31,-0.3,-0.27,-0.32,-0.35,-0.4,-0.45,-0.49,
  -0.5,-0.36,-0.1,0.28,0.75,1.22,1.6,1.9,2.14,2.33,2.4,2.39,
  2.24,1.93,1.44,0.99,0.45,-0.13,-0.78,-1.12,-1.31,-1.35,-1.48,-1.57,
  -1.55,-1.3,-1.07,-0.98,-1.02,-1.04,-1.1,-1.11,-1.16,-1.26,-1.46,-1.65,
  -1.66,-1.41,-1.07,-0.81,-0.71,-0.64,-0.55,-0.51,-0.55,-0.63,-0.75,-0.74,
  -0.68,-0.52,-0.44,-0.34,-0.25,-0.12,-0.08,-0.13,-0.19,-0.29,-0.35,-0.31,
  -0.15,0.03,0.09,0.2,0.43,0.65,0.79,0.86,1.01,1.21,1.31,1.14,
  0.92,0.63,0.38,-0.04,-0.26,-0.16,0.08,0.21,0.26,0.29,0.35,0.35,
  0.37,0.31,0.23,0.17,0.17,0.28,0.47,0.64,0.7,0.67,0.66,0.69,
  0.64,0.58,0.45,0.43,0.29,0.11,-0.06,-0.14,-0.11,-0.29,-0.57,-0.84,
  -0.85,-0.77,-0.57,-0.37,-0.14,-0.03,0.1,0.3,0.54,0.77,0.94,0.94,
  0.66,0.22,-0.12,-0.32,-0.38,-0.47,-0.56,-0.81,-1.07,-1.34,-1.5,-1.6,
  -1.64,-1.52,-1.29,-1.01,-0.84,-0.61,-0.37,-0.23,-0.24,-0.35,-0.55,-0.73,
  -0.85,-0.79,-0.61,-0.33,0.01,0.28,0.45,0.58,0.71,1.01,1.36,1.56,
  1.5,1.22,0.84,0.35,-0.17,-0.66,-1.05,-1.35,-1.56,-1.64,-1.64,-1.59,
  -1.42,-1.19,-0.93,-0.73,-0.55,-0.44,-0.48,-0.62,-0.83,-1.01,-1.09,-1.04,
  -0.86,-0.72,-0.59,-0.47,-0.26,-0.01,0.25,0.37,0.37,0.27,0.05,-0.21,
  -0.43,-0.43,-0.34,-0.3,-0.36,-0.41,-0.4,-0.32,-0.26,-0.18,-0.17,-0.27,
  -0.42,-0.46,-0.27,0.04,0.21,0.16,0.05,0.07,0.23,0.49,0.64,0.66,
  0.55,0.47,0.53,0.7,0.93,1.18,1.52,1.86,2.16,2.42,2.57,2.64,
  2.48,2.14,1.58,0.94,0.39,-0.07,-0.36,-0.54,-0.63,-0.69,-0.67,-0.56,
  -0.34,-0.16,0.05,0.2,0.3,0.31,0.14,-0.11,-0.38,-0.65,-0.84,-0.97,
  -0.92,-0.85,-0.7,-0.5,-0.22,-0.01,0.09,0.23,0.49,0.76,0.9,0.81,
  0.75,0.72,0.71,0.66,0.54,0.45,0.28,0.14,0.19,0.35,0.51,0.55,
  0.5,0.48,0.4,0.19,-0.08,-0.3,-0.41,-0.57,-0.89,-1.17,-1.27,-1.19,
  -1.05,-0.93,-0.84,-0.66,-0.48,-0.38,-0.4,-0.49,-0.67,-0.81,-0.98,-0.98,
  -0.97,-0.93,-0.99,-1.06,-0.99,-0.85,-0.81,-0.91,-1.01,-0.99,-0.92,-0.83,
  -0.68,-0.43,-0.15,0.16,0.48,0.77,1.07,1.32,1.56,1.78,1.92,1.95,
  1.78,1.48,1.14,0.71,0.39,0.15,0.04,-0.11,-0.21,-0.26,-0.37,-0.52
)
oni_annual <- data.frame(
  year = 1995:(1995 + (length(oni)/12 - 1)),  # 從 1995 開始，每 12 個月為一年
  mean_oni = tapply(oni, (seq_along(oni)-1) %/% 12, mean)  # 每 12 個月算平均
)

head(oni_annual)

library(Rbeast)

# 篩選 1995~2024
north_temp_data <- north_temp_data %>% filter(format(date, "%Y") >= 1995 & format(date, "%Y") <= 2024)
rain_north <- rain_north %>% filter(format(date, "%Y") >= 1995 & format(date, "%Y") <= 2024)
south_temp_data <- south_temp_data %>% filter(format(date, "%Y") >= 1995 & format(date, "%Y") <= 2024)
rain_south <- rain_south %>% filter(format(date, "%Y") >= 1995 & format(date, "%Y") <= 2024)
co2_sel <- co2_data %>% filter(year >= 1995 & year <= 2024)

# ===== 轉 ts =====
north_temp_ts <- ts(north_temp_data$temperature, start = c(1995,1), frequency = 365)
north_rain_ts <- ts(rain_north$rainfall, start = c(1995,1), frequency = 12)
south_temp_ts <- ts(south_temp_data$temperature, start = c(1995,1), frequency = 365)
south_rain_ts <- ts(rain_south$rainfall, start = c(1995,1), frequency = 12)

# ===== 北部每日最高溫 =====
cat("\n--- BEAST: Keelung Daily Max Temperature ---\n")
north_beast_temp <- beast(
  y = north_temp_ts,
  season = "harmonic",
  period = 365
)
plot(north_beast_temp, main = "(b) BEAST decomposition - Keelung Daily Max Temperature", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))

# 抓趨勢並年平均
north_trend_df <- data.frame(
  date = north_temp_data$date,
  trend = north_beast_temp[["trend"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(mean_trend = mean(trend, na.rm = TRUE))

north_corr_temp <- inner_join(north_trend_df, co2_sel, by = "year")
north_corr_temp_test <- cor.test(north_corr_temp$mean_trend, north_corr_temp$mean, method = "spearman")
cat("Temperature - Spearman r:", north_corr_temp_test$estimate, "\n")
cat("p-value:", north_corr_temp_test$p.value, "\n")

north_trend_month <- data.frame(
  date = north_temp_data$date,
  trend = north_beast_temp[["trend"]][["Y"]]
  ) %>%
  mutate(year = year(date),
        month = month(date)) %>%
  group_by(year, month) %>%
  summarise(mean_trend_month = mean(trend, na.rm = TRUE), .groups = "drop")
 
north_corr_temp_oni_test <- cor.test(north_trend_month$mean_trend_month, oni, method = "spearman",exact = FALSE)
cat("Temperature trend - Spearman r with ONI:", north_corr_temp_oni_test$estimate, "\n")
cat("p-value:", north_corr_temp_oni_test$p.value, "\n")


cat("\n--- Keelung Temperature Residuals ---\n")
north_temp_error_df <- data.frame(
  date = north_temp_data$date,
  error = north_beast_temp$data -  north_beast_temp[["trend"]][["Y"]] - north_beast_temp[["season"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(mean_error = mean(error, na.rm = TRUE))
north_corr_temp_error <- inner_join(north_temp_error_df, co2_sel, by = "year")
north_corr_temp_error_test <- cor.test(
  north_corr_temp_error$mean_error,
  north_corr_temp_error$mean,
  method = "spearman"
)
cat("Temperature residual - Spearman r:", north_corr_temp_error_test$estimate, "\n")
cat("p-value:", north_corr_temp_error_test$p.value, "\n")

north_temp_error_month <- data.frame(
  date = north_temp_data$date,
  error = north_beast_temp$data - north_beast_temp[["trend"]][["Y"]] - north_beast_temp[["season"]][["Y"]]
) %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  group_by(year, month) %>%
  summarise(
    mean_error_month = mean(error, na.rm = TRUE),
    .groups = "drop"
  )

north_corr_temp_error_oni_test <- cor.test(
  north_temp_error_month$mean_error_month,
  oni,
  method = "spearman", exact = FALSE
)
cat("Temperature residual oni - Spearman r:", north_corr_temp_error_oni_test$estimate, "\n")
cat("p-value:", north_corr_temp_error_oni_test$p.value, "\n")


cat("\n--- Keelung Temperature Seasonal Amplitude ---\n")
# 取出 season 成分
north_temp_season_df <- data.frame(
  date = north_temp_data$date,
  season = north_beast_temp[["season"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y")))

# 每年平均季節成分
north_temp_season_avg <- north_temp_season_df %>%
  group_by(year) %>%
  summarise(mean_season = mean(season, na.rm = TRUE))

# 計算每筆相對於年度平均的振幅
north_temp_amp_df <- north_temp_season_df %>%
  left_join(north_temp_season_avg, by = "year") %>%
  mutate(amplitude = season - mean_season)

# 年度平均振幅（取絕對值平均）
north_temp_amp_year <- north_temp_amp_df %>%
  group_by(year) %>%
  summarise(mean_amp = mean(abs(amplitude), na.rm = TRUE))

# 和 CO2 進行 Spearman 相關分析
north_temp_corr_amp <- inner_join(north_temp_amp_year, co2_sel, by = "year")
north_temp_corr_amp_test <- cor.test(north_temp_corr_amp$mean_amp,
                                     north_temp_corr_amp$mean,
                                     method = "spearman")
cat("Temperature amplitude - Spearman r:", north_temp_corr_amp_test$estimate, "\n")
cat("p-value:", north_temp_corr_amp_test$p.value, "\n")
north_temp_corr_amp_oni <- inner_join(north_temp_amp_year, oni_annual, by = "year")
north_temp_corr_amp_oni_test <- cor.test(
  north_temp_corr_amp_oni$mean_amp,
  north_temp_corr_amp_oni$mean_oni,
  method = "spearman"
)



north_temp_season_df <- data.frame(
  date = north_temp_data$date,
  season = north_beast_temp[["season"]][["Y"]]
) %>%
  mutate(year = year(date),
         month = month(date))

# 每月平均季節成分
north_temp_season_month <- north_temp_season_df %>%
  group_by(year, month) %>%
  summarise(mean_season_month = mean(season, na.rm = TRUE), .groups = "drop")

# 計算每筆相對於月平均的振幅
north_temp_amp_month <- north_temp_season_df %>%
  left_join(north_temp_season_month, by = c("year", "month")) %>%
  mutate(amplitude = season - mean_season_month)

# 月平均振幅（取絕對值平均）
north_temp_amp_month_avg <- north_temp_amp_month %>%
  group_by(year, month) %>%
  summarise(mean_amp_month = mean(abs(amplitude), na.rm = TRUE), .groups = "drop")

# 對 ONI 做 Spearman correlation（ONI 必須是月資料）
north_temp_corr_amp_oni_month <- cor.test(
  north_temp_amp_month_avg$mean_amp_month,
  oni,
  method = "spearman",
  exact = FALSE
)

cat("Temperature amplitude (monthly) - Spearman r with ONI:", 
    north_temp_corr_amp_oni_month$estimate, "\n")
cat("p-value:", north_temp_corr_amp_oni_month$p.value, "\n")

# ===== 北部月降雨量 =====
cat("\n--- BEAST: Keelung Monthly Rainfall ---\n")
north_beast_rain <- beast(
  y = north_rain_ts,
  season = "harmonic",
  period = 12
)
plot(north_beast_rain, main = "(a) BEAST decomposition - Keelung Monthly Rainfall", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))

north_rain_trend_df <- data.frame(
  date = rain_north$date,
  trend = north_beast_rain[["trend"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(mean_trend = mean(trend, na.rm = TRUE))

north_corr_rain <- inner_join(north_rain_trend_df, co2_sel, by = "year")
north_corr_rain_test <- cor.test(north_corr_rain$mean_trend, north_corr_rain$mean, method = "spearman")
cat("Rainfall - Spearman r:", north_corr_rain_test$estimate, "\n")
cat("p-value:", north_corr_rain_test$p.value, "\n")

north_corr_rain_oni_test <- cor.test(
  north_beast_rain[["trend"]][["Y"]],
  oni,
  method = "spearman",exact = FALSE
)

cat("Rainfall trend - Spearman r with ONI:", north_corr_rain_oni_test$estimate, "\n")
cat("p-value:", north_corr_rain_oni_test$p.value, "\n")

cat("\n--- Keelung Rainfall Residuals ---\n")
north_rain_error_df <- data.frame(
  date = rain_north$date,
  error = north_beast_rain$data - north_beast_rain[["trend"]][["Y"]] - north_beast_rain[["season"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(mean_error = mean(error, na.rm = TRUE))

north_corr_rain_error <- inner_join(north_rain_error_df, co2_sel, by = "year")
north_corr_rain_error_test <- cor.test(
  north_corr_rain_error$mean_error,
  north_corr_rain_error$mean,
  method = "spearman"
)

cat("Rainfall residual - Spearman r:", north_corr_rain_error_test$estimate, "\n")
cat("p-value:", north_corr_rain_error_test$p.value, "\n")

north_corr_rain_error_oni_test <- cor.test(
  (north_beast_rain$data - north_beast_rain[["trend"]][["Y"]] - north_beast_rain[["season"]][["Y"]]),
  oni,
  method = "spearman",exact = FALSE
)

cat("Rainfall residual - Spearman r with ONI:", north_corr_rain_error_oni_test$estimate, "\n")
cat("p-value:", north_corr_rain_error_oni_test$p.value, "\n")
cat("\n--- Keelung Rainfall Seasonal Amplitude ---\n")
north_rain_season_df <- data.frame(
  date = rain_north$date,
  season = north_beast_rain[["season"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y")))

north_rain_season_avg <- north_rain_season_df %>%
  group_by(year) %>%
  summarise(mean_season = mean(season, na.rm = TRUE))

north_rain_amp_df <- north_rain_season_df %>%
  left_join(north_rain_season_avg, by = "year") %>%
  mutate(amplitude = season - mean_season)

north_rain_amp_year <- north_rain_amp_df %>%
  group_by(year) %>%
  summarise(mean_amp = mean(abs(amplitude), na.rm = TRUE))

north_corr_rain_amp <- inner_join(north_rain_amp_year, co2_sel, by = "year")
north_corr_rain_amp_test <- cor.test(north_corr_rain_amp$mean_amp,
                                     north_corr_rain_amp$mean,
                                     method = "spearman")
cat("Rainfall amplitude - Spearman r:", north_corr_rain_amp_test$estimate, "\n")
cat("p-value:", north_corr_rain_amp_test$p.value, "\n")

north_rain_month <- data.frame(
  date = rain_north$date,                # 月資料的日期
  season = north_beast_rain[["season"]][["Y"]]  # 月資料
) %>%
  mutate(year = year(date), month = month(date))

# 計算每年的月平均
north_rain_month_avg <- north_rain_month %>%
  group_by(year) %>%
  summarise(mean_season_year = mean(season, na.rm = TRUE), .groups = "drop")

# 計算振幅 = 每月 - 當年平均
north_rain_amp_month <- north_rain_month %>%
  left_join(north_rain_month_avg, by = "year") %>%
  mutate(amplitude_month = season - mean_season_year)

# 跟 ONI correlation
north_corr_rain_amp_oni <- cor.test(
  north_rain_amp_month$amplitude_month,
  oni,
  method = "spearman",
  exact = FALSE
)

cat("Rainfall monthly amplitude - Spearman r with ONI:", north_corr_rain_amp_oni$estimate, "\n")
cat("p-value:", north_corr_rain_amp_oni$p.value, "\n")
# ===== 南部每日最高溫 =====
cat("\n--- BEAST: Hengchun Daily Max Temperature ---\n")
south_beast_temp <- beast(
  y = south_temp_ts,
  season = "harmonic",
  period = 365
)
plot(south_beast_temp, main = "(b) BEAST decomposition - Hengchun Daily Max Temperature", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))

south_trend_df <- data.frame(
  date = south_temp_data$date,
  trend = south_beast_temp[["trend"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(mean_trend = mean(trend, na.rm = TRUE))

south_corr_temp <- inner_join(south_trend_df, co2_sel, by = "year")
south_corr_temp_test <- cor.test(south_corr_temp$mean_trend, south_corr_temp$mean, method = "spearman")
cat("Temperature - Spearman r:", south_corr_temp_test$estimate, "\n")
cat("p-value:", south_corr_temp_test$p.value, "\n")

south_trend_month <- data.frame(
  date = south_temp_data$date,
  trend = south_beast_temp[["trend"]][["Y"]]
) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  group_by(year, month) %>%
  summarise(mean_trend_month = mean(trend, na.rm = TRUE), .groups = "drop")

south_corr_temp_oni_test <- cor.test(south_trend_month$mean_trend_month, oni, method = "spearman",exact = FALSE)
cat("Temperature trend - Spearman r with ONI:", south_corr_temp_oni_test$estimate, "\n")
cat("p-value:", south_corr_temp_oni_test$p.value, "\n")




cat("\n--- Hengchun Temperature Residuals ---\n")
south_temp_error_df <- data.frame(
  date = south_temp_data$date,
  error = south_beast_temp$data - south_beast_temp[["trend"]][["Y"]] - south_beast_temp[["season"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(mean_error = mean(error, na.rm = TRUE))

south_corr_temp_error <- inner_join(south_temp_error_df, co2_sel, by = "year")
south_corr_temp_error_test <- cor.test(
  south_corr_temp_error$mean_error,
  south_corr_temp_error$mean,
  method = "spearman"
)

cat("Temperature residual - Spearman r:", south_corr_temp_error_test$estimate, "\n")
cat("p-value:", south_corr_temp_error_test$p.value, "\n")
south_temp_error_month <- data.frame(
  date = south_temp_data$date,
  error = south_beast_temp$data - south_beast_temp[["trend"]][["Y"]] - south_beast_temp[["season"]][["Y"]]
) %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>%
  group_by(year, month) %>%
  summarise(
    mean_error_month = mean(error, na.rm = TRUE),
    .groups = "drop"
  )

south_corr_temp_error_oni_test <- cor.test(
  south_temp_error_month$mean_error_month,
  oni,
  method = "spearman", exact = FALSE
)
cat("Temperature residual oni - Spearman r:", south_corr_temp_error_oni_test$estimate, "\n")
cat("p-value:", south_corr_temp_error_oni_test$p.value, "\n")

cat("\n--- Hengchun Temperature Seasonal Amplitude ---\n")
south_temp_season_df <- data.frame(
  date = south_temp_data$date,
  season = south_beast_temp[["season"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y")))

south_temp_season_avg <- south_temp_season_df %>%
  group_by(year) %>%
  summarise(mean_season = mean(season, na.rm = TRUE))

south_temp_amp_df <- south_temp_season_df %>%
  left_join(south_temp_season_avg, by = "year") %>%
  mutate(amplitude = season - mean_season)

south_temp_amp_year <- south_temp_amp_df %>%
  group_by(year) %>%
  summarise(mean_amp = mean(abs(amplitude), na.rm = TRUE))

south_corr_temp_amp <- inner_join(south_temp_amp_year, co2_sel, by = "year")
south_corr_temp_amp_test <- cor.test(south_corr_temp_amp$mean_amp,
                                     south_corr_temp_amp$mean,
                                     method = "spearman")
cat("Temperature amplitude - Spearman r:", south_corr_temp_amp_test$estimate, "\n")
cat("p-value:", south_corr_temp_amp_test$p.value, "\n")
south_temp_season_df <- data.frame(
  date = south_temp_data$date,
  season = south_beast_temp[["season"]][["Y"]]
) %>%
  mutate(year = year(date),
         month = month(date))

# 每月平均季節成分
south_temp_season_month <- south_temp_season_df %>%
  group_by(year, month) %>%
  summarise(mean_season_month = mean(season, na.rm = TRUE), .groups = "drop")

# 計算每筆相對於月平均的振幅
south_temp_amp_month <- south_temp_season_df %>%
  left_join(south_temp_season_month, by = c("year", "month")) %>%
  mutate(amplitude = season - mean_season_month)

# 月平均振幅（取絕對值平均）
south_temp_amp_month_avg <- south_temp_amp_month %>%
  group_by(year, month) %>%
  summarise(mean_amp_month = mean(abs(amplitude), na.rm = TRUE), .groups = "drop")

# 對 ONI 做 Spearman correlation（ONI 必須是月資料）
south_temp_corr_amp_oni_month <- cor.test(
  south_temp_amp_month_avg$mean_amp_month,
  oni,
  method = "spearman",
  exact = FALSE
)
cat("Temperature amplitude (monthly) - Spearman r with ONI:", 
    south_temp_corr_amp_oni_month$estimate, "\n")
cat("p-value:", south_temp_corr_amp_oni_month$p.value, "\n")

# ===== 南部月降雨量 =====
cat("\n--- BEAST: Hengchun Monthly Rainfall ---\n")
south_beast_rain <- beast(
  y = south_rain_ts,
  season = "harmonic",
  period = 12
)
plot(south_beast_rain, main = "(a) BEAST decomposition - Hengchun Monthly Rainfall",
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))

south_rain_trend_df <- data.frame(
  date = rain_south$date,
  trend = south_beast_rain[["trend"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(mean_trend = mean(trend, na.rm = TRUE))

south_corr_rain <- inner_join(south_rain_trend_df, co2_sel, by = "year")
south_corr_rain_test <- cor.test(south_corr_rain$mean_trend, south_corr_rain$mean, method = "spearman")
cat("Rainfall - Spearman r:", south_corr_rain_test$estimate, "\n")
cat("p-value:", south_corr_rain_test$p.value, "\n")
south_corr_rain_oni_test <- cor.test(
  south_beast_rain[["trend"]][["Y"]],
  oni,
  method = "spearman",exact = FALSE
)

cat("Rainfall trend - Spearman r with ONI:", south_corr_rain_oni_test$estimate, "\n")
cat("p-value:", south_corr_rain_oni_test$p.value, "\n")

cat("\n--- Hengchun Rainfall Residuals ---\n")
south_rain_error_df <- data.frame(
  date = rain_south$date,
  error = south_beast_rain$data - south_beast_rain[["trend"]][["Y"]] - south_beast_rain[["season"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(mean_error = mean(error, na.rm = TRUE))

south_corr_rain_error <- inner_join(south_rain_error_df, co2_sel, by = "year")
south_corr_rain_error_test <- cor.test(
  south_corr_rain_error$mean_error,
  south_corr_rain_error$mean,
  method = "spearman"
)

cat("Rainfall residual - Spearman r:", south_corr_rain_error_test$estimate, "\n")
cat("p-value:", south_corr_rain_error_test$p.value, "\n")
south_corr_rain_error_oni_test <- cor.test(
  (south_beast_rain$data - south_beast_rain[["trend"]][["Y"]] - south_beast_rain[["season"]][["Y"]]),
  oni,
  method = "spearman",exact = FALSE
)

cat("Rainfall residual - Spearman r with ONI:", south_corr_rain_error_oni_test$estimate, "\n")
cat("p-value:", south_corr_rain_error_oni_test$p.value, "\n")


south_rain_season_df <- data.frame(
  date = rain_south$date,
  season = south_beast_rain[["season"]][["Y"]]
) %>%
  mutate(year = as.numeric(format(date, "%Y")))

south_rain_season_avg <- south_rain_season_df %>%
  group_by(year) %>%
  summarise(mean_season = mean(season, na.rm = TRUE))

south_rain_amp_df <- south_rain_season_df %>%
  left_join(south_rain_season_avg, by = "year") %>%
  mutate(amplitude = season - mean_season)

south_rain_amp_year <- south_rain_amp_df %>%
  group_by(year) %>%
  summarise(mean_amp = mean(abs(amplitude), na.rm = TRUE))

south_corr_rain_amp <- inner_join(south_rain_amp_year, co2_sel, by = "year")
south_corr_rain_amp_test <- cor.test(south_corr_rain_amp$mean_amp,
                                     south_corr_rain_amp$mean,
                                     method = "spearman")
cat("Rainfall amplitude - Spearman r:", south_corr_rain_amp_test$estimate, "\n")
cat("p-value:", south_corr_rain_amp_test$p.value, "\n")

south_rain_month <- data.frame(
  date = rain_south$date,                # 月資料的日期
  season = south_beast_rain[["season"]][["Y"]]  # 月資料
) %>%
  mutate(year = year(date), month = month(date))

# 計算每年的月平均
south_rain_month_avg <- south_rain_month %>%
  group_by(year) %>%
  summarise(mean_season_year = mean(season, na.rm = TRUE), .groups = "drop")

# 計算振幅 = 每月 - 當年平均
south_rain_amp_month <- south_rain_month %>%
  left_join(south_rain_month_avg, by = "year") %>%
  mutate(amplitude_month = season - mean_season_year)

# 跟 ONI correlation
south_corr_rain_amp_oni <- cor.test(
  south_rain_amp_month$amplitude_month,
  oni,
  method = "spearman",
  exact = FALSE
)

cat("Rainfall monthly amplitude - Spearman r with ONI:", south_corr_rain_amp_oni$estimate, "\n")
cat("p-value:", south_corr_rain_amp_oni$p.value, "\n")

######################################

# ===============================
# === 南部 (Hengchun) SPI ===
# ===============================

# --- 趨勢與 CO2 相關 ---
south_spi_trend_df <- data.frame(
  date = as.Date(as.yearmon(south_beast_result[["time"]])),
  trend = south_beast_result[["trend"]][["Y"]]
) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(mean_trend = mean(trend, na.rm = TRUE))

south_spi_corr_trend <- inner_join(south_spi_trend_df, co2_sel, by = "year")
south_spi_corr_trend_test <- cor.test(south_spi_corr_trend$mean_trend,
                                      south_spi_corr_trend$mean,
                                      method = "spearman")
cat("--- Hengchun SPI trend ---\n")
cat("Spearman r:", south_spi_corr_trend_test$estimate, "\n")
cat("p-value:", south_spi_corr_trend_test$p.value, "\n")

south_spi_corr_trend_oni_test <- cor.test(south_beast_result[["trend"]][["Y"]],
                                      oni,
                                      method = "spearman",exact=FALSE)
cat("Spearman r:", south_spi_corr_trend_oni_test$estimate, "\n")
cat("p-value:", south_spi_corr_trend_oni_test$p.value, "\n")

# --- 週期性振幅與 CO2 相關 ---
cat("\n--- Seasonal amplitude analysis (Hengchun SPI) ---\n")
south_spi_season_df <- data.frame(
  date = as.Date(as.yearmon(south_beast_result[["time"]])),
  season = south_beast_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date))

south_spi_season_avg <- south_spi_season_df %>%
  group_by(year) %>%
  summarise(mean_season = mean(season, na.rm = TRUE))

south_spi_amp_df <- south_spi_season_df %>%
  left_join(south_spi_season_avg, by = "year") %>%
  mutate(amplitude = season - mean_season)

south_spi_amp_year <- south_spi_amp_df %>%
  group_by(year) %>%
  summarise(mean_amp = mean(abs(amplitude), na.rm = TRUE))

south_spi_corr_amp <- inner_join(south_spi_amp_year, co2_sel, by = "year")
south_spi_corr_amp_test <- cor.test(south_spi_corr_amp$mean_amp,
                                    south_spi_corr_amp$mean,
                                    method = "spearman")
cat("SPI amplitude - Spearman r:", south_spi_corr_amp_test$estimate, "\n")
cat("p-value:", south_spi_corr_amp_test$p.value, "\n")
south_spi_season_df <- data.frame(
  date = as.Date(as.yearmon(south_beast_result[["time"]])),  # 原本就是月資料
  season = south_beast_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date),
         month = month(date))

# 每年平均
south_spi_season_avg <- south_spi_season_df %>%
  group_by(year) %>%
  summarise(mean_season_year = mean(season, na.rm = TRUE), .groups = "drop")

# 計算每月相對於年度平均的振幅
south_spi_amp_month <- south_spi_season_df %>%
  left_join(south_spi_season_avg, by = "year") %>%
  mutate(amplitude_month = season - mean_season_year)

# 和 ONI 做 Spearman correlation（月尺度）
south_spi_corr_amp_oni <- cor.test(
  south_spi_amp_month$amplitude_month,
  oni,
  method = "spearman",
  exact = FALSE
)

cat("SPI monthly amplitude - Spearman r with ONI:", south_spi_corr_amp_oni$estimate, "\n")
cat("p-value:", south_spi_corr_amp_oni$p.value, "\n")

cat("\n--- Hengchun SPI Residuals ---\n")
south_spi_error_df <- data.frame(
  date = as.Date(as.yearmon(south_beast_result[["time"]])),
  error = south_beast_result[["data"]] - 
    south_beast_result[["trend"]][["Y"]] - 
    south_beast_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(mean_error = mean(error, na.rm = TRUE))

south_spi_corr_error <- inner_join(south_spi_error_df, co2_sel, by = "year")
south_spi_corr_error_test <- cor.test(
  south_spi_corr_error$mean_error,
  south_spi_corr_error$mean,
  method = "spearman"
)
cat("SPI residual - Spearman r:", south_spi_corr_error_test$estimate, "\n")
cat("p-value:", south_spi_corr_error_test$p.value, "\n")

south_spi_corr_error_oni_test <- cor.test(
  (south_beast_result[["data"]] - 
    south_beast_result[["trend"]][["Y"]] - 
    south_beast_result[["season"]][["Y"]]),
  oni,
  method = "spearman",exact=FALSE
)
cat("SPI residual - Spearman r:", south_spi_corr_error_oni_test$estimate, "\n")
cat("p-value:", south_spi_corr_error_oni_test$p.value, "\n")
# ===============================
# === 北部 (Keelung) SPI ===
# ===============================

# --- 趨勢與 CO2 相關 ---
north_spi_trend_df <- data.frame(
  date = as.Date(as.yearmon(north_beast_result[["time"]])),
  trend = north_beast_result[["trend"]][["Y"]]
) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(mean_trend = mean(trend, na.rm = TRUE))

north_spi_corr_trend <- inner_join(north_spi_trend_df, co2_sel, by = "year")
north_spi_corr_trend_test <- cor.test(north_spi_corr_trend$mean_trend,
                                      north_spi_corr_trend$mean,
                                      method = "spearman")
cat("\n--- Keelung SPI trend ---\n")
cat("Spearman r:", north_spi_corr_trend_test$estimate, "\n")
cat("p-value:", north_spi_corr_trend_test$p.value, "\n")
north_spi_corr_trend_oni_test <- cor.test(north_beast_result[["trend"]][["Y"]],
                                          oni,
                                          method = "spearman",exact=FALSE)
cat("Spearman r:", north_spi_corr_trend_oni_test$estimate, "\n")
cat("p-value:", north_spi_corr_trend_oni_test$p.value, "\n")

# --- 週期性振幅與 CO2 相關 ---
cat("\n--- Seasonal amplitude analysis (Keelung SPI) ---\n")
north_spi_season_df <- data.frame(
  date = as.Date(as.yearmon(north_beast_result[["time"]])),
  season = north_beast_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date))

north_spi_season_avg <- north_spi_season_df %>%
  group_by(year) %>%
  summarise(mean_season = mean(season, na.rm = TRUE))

north_spi_amp_df <- north_spi_season_df %>%
  left_join(north_spi_season_avg, by = "year") %>%
  mutate(amplitude = season - mean_season)

north_spi_amp_year <- north_spi_amp_df %>%
  group_by(year) %>%
  summarise(mean_amp = mean(abs(amplitude), na.rm = TRUE))

north_spi_corr_amp <- inner_join(north_spi_amp_year, co2_sel, by = "year")
north_spi_corr_amp_test <- cor.test(north_spi_corr_amp$mean_amp,
                                    north_spi_corr_amp$mean,
                                    method = "spearman")
cat("SPI amplitude - Spearman r:", north_spi_corr_amp_test$estimate, "\n")
cat("p-value:", north_spi_corr_amp_test$p.value, "\n")
north_spi_season_df <- data.frame(
  date = as.Date(as.yearmon(north_beast_result[["time"]])),  # 原本就是月資料
  season = north_beast_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date),
         month = month(date))

# 每年平均
north_spi_season_avg <- north_spi_season_df %>%
  group_by(year) %>%
  summarise(mean_season_year = mean(season, na.rm = TRUE), .groups = "drop")

# 計算每月相對於年度平均的振幅
north_spi_amp_month <- north_spi_season_df %>%
  left_join(north_spi_season_avg, by = "year") %>%
  mutate(amplitude_month = season - mean_season_year)

# 和 ONI 做 Spearman correlation（月尺度）
north_spi_corr_amp_oni <- cor.test(
  north_spi_amp_month$amplitude_month,
  oni,
  method = "spearman",
  exact = FALSE
)

cat("SPI monthly amplitude - Spearman r with ONI:", north_spi_corr_amp_oni$estimate, "\n")
cat("p-value:", north_spi_corr_amp_oni$p.value, "\n")

cat("\n--- Keelung SPI Residuals ---\n")
north_spi_error_df <- data.frame(
  date = as.Date(as.yearmon(north_beast_result[["time"]])),
  error = north_beast_result[["data"]] - 
    north_beast_result[["trend"]][["Y"]] - 
    north_beast_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(mean_error = mean(error, na.rm = TRUE))

north_spi_corr_error <- inner_join(north_spi_error_df, co2_sel, by = "year")
north_spi_corr_error_test <- cor.test(
  north_spi_corr_error$mean_error,
  north_spi_corr_error$mean,
  method = "spearman"
)
cat("SPI residual - Spearman r:", north_spi_corr_error_test$estimate, "\n")
cat("p-value:", north_spi_corr_error_test$p.value, "\n")
north_spi_corr_error_oni_test <- cor.test(
  (north_beast_result[["data"]] - 
     north_beast_result[["trend"]][["Y"]] - 
     north_beast_result[["season"]][["Y"]]),
  oni,
  method = "spearman",exact=FALSE
)
cat("SPI residual - Spearman r:", north_spi_corr_error_oni_test$estimate, "\n")
cat("p-value:", north_spi_corr_error_oni_test$p.value, "\n")

# ===============================
# === 南部 (Hengchun) HWDI ===
# ===============================

cat("\n--- Seasonal amplitude analysis (Hengchun HWDI) ---\n")
south_hwdi_season_df <- data.frame(
  date = as.Date(as.yearmon(beast_south_hwdi_result[["time"]])),
  season = beast_south_hwdi_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date))

# 每年平均週期
south_hwdi_season_avg <- south_hwdi_season_df %>%
  group_by(year) %>%
  summarise(mean_season = mean(season, na.rm = TRUE))

# 計算振幅（偏離年度平均）
south_hwdi_amp_df <- south_hwdi_season_df %>%
  left_join(south_hwdi_season_avg, by = "year") %>%
  mutate(amplitude = season - mean_season)

# 每年平均振幅（絕對值）
south_hwdi_amp_year <- south_hwdi_amp_df %>%
  group_by(year) %>%
  summarise(mean_amp = mean(abs(amplitude), na.rm = TRUE))

# 振幅與 CO2 相關
south_hwdi_corr_amp <- inner_join(south_hwdi_amp_year, co2_sel, by = "year")
south_hwdi_corr_amp_test <- cor.test(south_hwdi_corr_amp$mean_amp,
                                     south_hwdi_corr_amp$mean,
                                     method = "spearman")
cat("HWDI amplitude - Spearman r:", south_hwdi_corr_amp_test$estimate, "\n")
cat("p-value:", south_hwdi_corr_amp_test$p.value, "\n")

south_hwdi_season_df <- data.frame(
  date = as.Date(as.yearmon(beast_south_hwdi_result[["time"]])),  # 月資料
  season = beast_south_hwdi_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date),
         month = month(date))

# 每年平均
south_hwdi_season_avg <- south_hwdi_season_df %>%
  group_by(year) %>%
  summarise(mean_season_year = mean(season, na.rm = TRUE), .groups = "drop")

# 計算每月相對於年度平均的振幅
south_hwdi_amp_month <- south_hwdi_season_df %>%
  left_join(south_hwdi_season_avg, by = "year") %>%
  mutate(amplitude_month = season - mean_season_year)

# 與 ONI 做 Spearman correlation（月尺度）
south_hwdi_corr_amp_oni <- cor.test(
  south_hwdi_amp_month$amplitude_month,
  oni,
  method = "spearman",
  exact = FALSE
)

cat("HWDI monthly amplitude - Spearman r with ONI:", south_hwdi_corr_amp_oni$estimate, "\n")
cat("p-value:", south_hwdi_corr_amp_oni$p.value, "\n")

cat("\n--- Hengchun HWDI Residuals ---\n")
south_hwdi_error_df <- data.frame(
  date = as.Date(as.yearmon(beast_south_hwdi_result[["time"]])),
  error = beast_south_hwdi_result[["data"]] - 
    beast_south_hwdi_result[["trend"]][["Y"]] - 
    beast_south_hwdi_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(mean_error = mean(error, na.rm = TRUE))

south_hwdi_corr_error <- inner_join(south_hwdi_error_df, co2_sel, by = "year")
south_hwdi_corr_error_test <- cor.test(
  south_hwdi_corr_error$mean_error,
  south_hwdi_corr_error$mean,
  method = "spearman"
)

cat("HWDI residual - Spearman r:", south_hwdi_corr_error_test$estimate, "\n")
cat("p-value:", south_hwdi_corr_error_test$p.value, "\n")

south_hwdi_corr_error_oni_test <- cor.test(
  (beast_south_hwdi_result[["data"]] - 
    beast_south_hwdi_result[["trend"]][["Y"]] - 
    beast_south_hwdi_result[["season"]][["Y"]]),
  oni,
  method = "spearman",exact=FALSE
)

cat("HWDI residual - Spearman r:", south_hwdi_corr_error_oni_test$estimate, "\n")
cat("p-value:", south_hwdi_corr_error_oni_test$p.value, "\n")

cat("\n--- Hengchun HWDI Trend ---\n")
south_hwdi_trend_df <- data.frame(
  date = as.Date(as.yearmon(beast_south_hwdi_result[["time"]])),
  trend = beast_south_hwdi_result[["trend"]][["Y"]]
) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(mean_trend = mean(trend, na.rm = TRUE))

south_hwdi_corr_trend_test <- cor.test(south_hwdi_trend_df$mean_trend,
                                       co2_sel$mean,
                                       method = "spearman")
cat("HWDI trend - Spearman r:",south_hwdi_corr_trend_test$estimate, "\n")
cat("p-value:", south_hwdi_corr_trend_test$p.value, "\n")

south_hwdi_corr_trend_oni_test <- cor.test(beast_south_hwdi_result[["trend"]][["Y"]],
                                           oni,
                                           method = "spearman",exact=FALSE)
cat("HWDI trend - Spearman r:", south_hwdi_corr_trend_oni_test$estimate, "\n")
cat("p-value:",south_hwdi_corr_trend_oni_test$p.value, "\n")

# ===============================
# === 北部 (Keelung) HWDI ===
# ===============================

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

north_hwdi_season_df <- data.frame(
  date = as.Date(as.yearmon(beast_north_hwdi_result[["time"]])),  # 月資料
  season = beast_north_hwdi_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date),
         month = month(date))

# 每年平均
north_hwdi_season_avg <- north_hwdi_season_df %>%
  group_by(year) %>%
  summarise(mean_season_year = mean(season, na.rm = TRUE), .groups = "drop")

# 計算每月相對於年度平均的振幅
north_hwdi_amp_month <- north_hwdi_season_df %>%
  left_join(north_hwdi_season_avg, by = "year") %>%
  mutate(amplitude_month = season - mean_season_year)

# 與 ONI 做 Spearman correlation（月尺度）
north_hwdi_corr_amp_oni <- cor.test(
  north_hwdi_amp_month$amplitude_month,
  oni,
  method = "spearman",
  exact = FALSE
)

cat("HWDI monthly amplitude - Spearman r with ONI:", north_hwdi_corr_amp_oni$estimate, "\n")
cat("p-value:", north_hwdi_corr_amp_oni$p.value, "\n")



cat("\n--- Keelung HWDI Residuals ---\n")
north_hwdi_error_df <- data.frame(
  date = as.Date(as.yearmon(beast_north_hwdi_result[["time"]])),
  error = beast_north_hwdi_result[["data"]] - 
    beast_north_hwdi_result[["trend"]][["Y"]] - 
    beast_north_hwdi_result[["season"]][["Y"]]
) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(mean_error = mean(error, na.rm = TRUE))

north_hwdi_corr_error <- inner_join(north_hwdi_error_df, co2_sel, by = "year")
north_hwdi_corr_error_test <- cor.test(
  north_hwdi_corr_error$mean_error,
  north_hwdi_corr_error$mean,
  method = "spearman"
)

cat("HWDI residual - Spearman r:", north_hwdi_corr_error_test$estimate, "\n")
cat("p-value:", north_hwdi_corr_error_test$p.value, "\n")
north_hwdi_corr_error_oni_test <- cor.test(
  (beast_north_hwdi_result[["data"]] - 
     beast_north_hwdi_result[["trend"]][["Y"]] - 
     beast_north_hwdi_result[["season"]][["Y"]]),
  oni,
  method = "spearman",exact=FALSE
)

cat("HWDI residual - Spearman r:", north_hwdi_corr_error_oni_test$estimate, "\n")
cat("p-value:", north_hwdi_corr_error_oni_test$p.value, "\n")

cat("\n--- Keelung HWDI Trend ---\n")
north_hwdi_trend_df <- data.frame(
  date = as.Date(as.yearmon(beast_north_hwdi_result[["time"]])),
  trend = beast_north_hwdi_result[["trend"]][["Y"]]
) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(mean_trend = mean(trend, na.rm = TRUE))

north_hwdi_corr_trend <- inner_join(north_hwdi_trend_df, co2_sel, by = "year")
north_hwdi_corr_trend_test <- cor.test(north_hwdi_corr_trend$mean_trend,
                                       north_hwdi_corr_trend$mean,
                                       method = "spearman")
cat("HWDI trend - Spearman r:", north_hwdi_corr_trend_test$estimate, "\n")
cat("p-value:", north_hwdi_corr_trend_test$p.value, "\n")

north_hwdi_corr_trend_oni_test <- cor.test(beast_north_hwdi_result[["trend"]][["Y"]],
                                           oni,
                                           method = "spearman",exact =FALSE)
cat("HWDI trend - Spearman r:", north_hwdi_corr_trend_oni_test$estimate, "\n")
cat("p-value:", north_hwdi_corr_trend_oni_test$p.value, "\n")

#--------------------------------------------
  
