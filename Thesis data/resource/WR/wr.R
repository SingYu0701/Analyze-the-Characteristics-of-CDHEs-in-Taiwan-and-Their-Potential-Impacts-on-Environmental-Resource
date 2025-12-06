library(dplyr)
library(writexl)
# 設定資料夾路徑
folder_path <- "D:/成大/資源所/SPI+HWDI/data/resource/WR/"

# 要讀的年份
years <- 2012:2024

# 建立空的資料框來存結果
all_WR <- data.frame()

for (y in years) {
  
  file_path <- paste0(folder_path, y, ".tsv")
  temp <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE)
  colnames(temp) <- c("name", "Capacity", "storage", "Date")
  temp$Date <- as.Date(temp$Date)
  
  temp <- temp %>%
    filter(name %in% c("新山水庫", "牡丹水庫")) %>%
    mutate(Year = as.numeric(format(Date, "%Y")))
  
  temp_avg <- temp %>%
    group_by(name, Year) %>%
    summarise(Mean_Storage = mean(storage, na.rm = TRUE))
  
  all_WR <- bind_rows(all_WR, temp_avg)
}

# 檢查結果
print(all_WR)
xinshan <- all_WR %>% filter(name == "新山水庫")
mudan   <- all_WR %>% filter(name == "牡丹水庫")


# 寫成兩個 Excel 檔
write_xlsx(xinshan, "D:/成大/資源所/SPI+HWDI/data/resource/WR/north_wr.xlsx")
write_xlsx(mudan,   "D:/成大/資源所/SPI+HWDI/data/resource/WR/south_wr.xlsx")

