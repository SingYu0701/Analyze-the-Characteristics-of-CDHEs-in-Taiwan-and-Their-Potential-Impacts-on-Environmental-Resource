library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)
library(tidyverse)
library(writexl)

files <- list.files(path = "  ", pattern = "MaxAirTemperature.*\\.csv$", full.names = TRUE)

read_and_clean <- function(file_path) {
  file_name <- basename(file_path)
  year <- str_extract(file_name, "(?<=-)\\d{4}(?=-)")
  
  df <- read_csv(file_path, na = c("--", "X"), show_col_types = FALSE)
  
  colnames(df)[1] <- "day"
  colnames(df)[2:13] <- as.character(1:12)
  
  df_long <- df %>%
    pivot_longer(cols = -day, names_to = "month", values_to = "temperature") %>%
    mutate(
      year = as.integer(year),
      day = as.integer(day),
      month = as.integer(month),
      date = make_date(year, month, day)
    ) %>%
    filter(!is.na(date)) %>%  # 移除錯誤日期
    select(date, temperature)
  
  return(df_long)
}
all_data_north <- map_dfr(files, read_and_clean) %>%
    arrange(date)
print(head(all_data_north, 40))
write_xlsx(all_data_north, "all_data_north.xlsx")


######


precip_raw <- read_csv("data/north/466940-2025-Precipitation-month.csv", 
                       na = c("--", "x", "NA"))


colnames(precip_raw)[1] <- "year"


precip_long <- precip_raw %>%
  pivot_longer(
    cols = -year,
    names_to = "month",
    values_to = "precipitation"
  ) %>%
  mutate(
    month = as.integer(month),
    year = as.integer(year),
    precipitation = as.numeric(precipitation)
  ) %>%
  arrange(year, month)

write_xlsx(precip_long, "data/north/rain_north.xlsx")
