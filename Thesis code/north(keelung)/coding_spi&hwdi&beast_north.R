library(readxl)
library(zoo)
library(SPEI)
library(dplyr)
library(tidyr)
library(Rbeast)
library(lubridate)
library(writexl)
library(ggplot2)
library(fitdistrplus)
rain_north <- read_excel("D:/成大/資源所/SPI+HWDI/data/north/rain_north.xlsx")
descdist(rain_north$precipitation)
rain_north <- zoo(rain_north$precipitation, order.by = as.yearmon(paste(rain_north$year, rain_north$month), "%Y %m"))

rain_north <- ts(coredata(rain_north), start = c(1995, 1), frequency = 12)
set.seed(123)
rain_north_beast_result <- beast(rain_north,
                                 freq = 12,
                                 maxknot = 30,
                                 numSam = 15000,
                                 burnin = 3000)
plot(rain_north_beast_result, main="Rain beast decomposition of Keelung")
plot(rain_north_beast_result, main="Rain beast decomposition of Keelung", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))

decimal_to_ym <- function(decimal_years) {
  years <- floor(decimal_years)
  months <- round((decimal_years - years) * 12) + 1  # +1 因為0表示1月
  paste0(years, "-", sprintf("%02d", months))
}

decimal_to_ym(rain_north_beast_result$season$cp)
decimal_to_ym(rain_north_beast_result$trend$cp)

north_spi1 <- spi(rain_north, scale = 1, distribution = "Gamma")
north_spi1$fitted <-pmax(pmin(north_spi1$fitted, 3.5), -3.5)
plot(north_spi1)+ ggtitle("SPI1 of Keelung")+ 
  theme(strip.text = element_blank())

north_spi1_values <- data.frame(
  date = time(north_spi1$fitted),
  spi = as.numeric(north_spi1$fitted)
) %>%
  mutate(
    date2 = as.Date(as.yearmon(date)),
    year = as.integer(format(date2, "%Y")),
    month = as.integer(format(date2, "%m"))
  ) %>%
  dplyr::select(year, month, spi)

ggplot(north_spi1_values, aes(x = factor(month), y = factor(year), fill = spi)) +
  geom_tile(color = "grey80", size = 0.4) +
  scale_fill_gradient2(
    low = "red",  high = "white",
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
    title = "SPI1 Heatmap of Keelung",
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

north_spi1_values
north_spi_ts <- ts(north_spi1_values$spi, start = c(1995, 1), frequency = 12)
set.seed(123)
north_beast_result <- beast(north_spi_ts,
                            freq = 12,
                            maxknot = 30,       # 允許較多變化點
                            numSam = 15000,     # 增加 MCMC 樣本數
                            burnin = 3000)

plot(north_beast_result, main="SPI1 beast decomposition of Keelung")
plot(north_beast_result, main="SPI1 beast decomposition of Keelung", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))
decimal_to_ym(north_beast_result$season$cp)
decimal_to_ym(north_beast_result$trend$cp)
#write_xlsx(north_spi1_values, path = "D:/成大/資源所/SPI+HWDI/data/north/north_spi1_values.xlsx")
####
temp_north <- read_excel("D:/成大/資源所/SPI+HWDI/data/north/all_data_north.xlsx")
temp_north <- temp_north %>%
  mutate(
    mmdd = format(date, "%m-%d"),
    year = year(date),
    month = month(date)
  )

# 1. 計算每天的95百分位門檻 (歷年同月合併計算)
thresholds_north <- temp_north %>%
  group_by(mmdd) %>%
  filter(year >= 1995 & year <= 2014) %>%
  summarise(threshold_95_north = quantile(temperature, 0.95, na.rm = TRUE))

temp_north <- temp_north %>%
  left_join(thresholds_north, by = "mmdd") %>%
  mutate(
    exceed = ifelse(is.na(temperature), FALSE, temperature > threshold_95_north)
  )

# 2. 找連續三天以上 exceed = TRUE 的區間，標記熱浪事件

temp_north <- temp_north %>%
  mutate(
    exceed_flag = ifelse(exceed, 1, 0),
    date_diff = c(NA, diff(date)),
    new_group = ifelse(is.na(date_diff) | date_diff > 1 | exceed_flag == 0, 1, 0)
  )
temp_north$wave_id <- cumsum(temp_north$new_group)

# 3. 熱浪事件判斷: exceed = TRUE 且長度 >= 3 天的區間視為熱浪事件
events <- temp_north %>%
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

# 新增年與月欄位
events_expanded <- events_expanded %>%
  mutate(
    year = year(dates),
    month = month(dates)
  )

# 計算每年每月熱浪持續天數
hwdi_monthly <- events_expanded %>%
  group_by(year, month) %>%
  summarise(hwdi = n(), .groups = "drop") %>%
  arrange(year, month)


# 4. 完整月份沒有熱浪事件的補0
all_months_north <- temp_north %>%
  distinct(year, month)

hwdi_full_north <- all_months_north %>%
  left_join(hwdi_monthly, by = c("year", "month")) %>%
  mutate(hwdi = ifelse(is.na(hwdi), 0, hwdi)) %>%
  arrange(year, month)

hwdi_full_north
#write_xlsx(hwdi_full_north, path = "D:/成大/資源所/SPI+HWDI/data/north/hwdi_north.xlsx")

hwdi_north_ts <- ts(hwdi_full_north$hwdi, start = c(min(hwdi_full_north$year), min(hwdi_full_north$month)), frequency = 12)
set.seed(002)
beast_north_hwdi_result <- beast(hwdi_north_ts,
                                 freq = 12)

plot(beast_north_hwdi_result, main="HWDI beast decomposition of Keelung")
plot(beast_north_hwdi_result, main="HWDI beast decomposition of Keelung", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))
sort(decimal_to_ym(beast_north_hwdi_result$season$cp))
sort(decimal_to_ym(beast_north_hwdi_result$trend$cp))

library(scales)  # for date formatting if needed
library(ggplot2)
ggplot(hwdi_full_north, aes(x = factor(month), y = factor(year))) +
  geom_tile(aes(fill = hwdi), color = "grey90", size = 0.4) +
  scale_fill_gradient(
    low = "white",
    high = "red",
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
    title = "HWDI Heatmap of Keelung",
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
