library(readxl)
library(zoo)
library(dplyr)
library(tidyr)
library(Rbeast)
library(lubridate)


oni<-read.csv("D:/成大/資源所/SPI+HWDI/data/oni.csv")
oni_ts <- ts(oni$oni, start = c(min(oni$year), min(oni$month)), frequency = 12)
set.seed(002)
oni_beast_result <- beast(oni_ts,   # ONI 本身就是 anomaly，無需週期性
                          freq = 12,
                          maxknot = 10,
                          numSam = 10000,
                          burnin = 2000)

# 繪圖
plot(oni_beast_result, main="ONI beast decomposition")
decimal_to_ym <- function(decimal_years) {
  years <- floor(decimal_years)
  months <- round((decimal_years - years) * 12) + 1  # +1 因為0表示1月
  paste0(years, "-", sprintf("%02d", months))
}
plot(oni_beast_result, main="ONI beast decomposition", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))
sort(decimal_to_ym(oni_beast_result$season$cp))
sort(decimal_to_ym(oni_beast_result$trend$cp))
######
nino3.4<-read.csv("D:/成大/資源所/SPI+HWDI/data/nino3.4.csv")
nino3.4_ts <- ts(nino3.4$nino, start = c(min(nino3.4$year), min(nino3.4$month)), frequency = 12)
nino3.4_beast_result <- beast(nino3.4_ts,
                              season = "none",   # ONI 本身就是 anomaly，無需週期性
                              freq = 12,
                              maxknot = 10,
                              numSam = 10000,
                              burnin = 2000)

# 繪圖
plot(nino3.4_beast_result)

#####
wp <- read_excel("D:/成大/資源所/SPI+HWDI/data/wp.xlsx")
wp_ts <- ts(wp$wp, start = c(min(wp$wp), min(wp$wp)), frequency = 12)
wp_beast_result <- beast(wp_ts,
                         season = "none",   # ONI 本身就是 anomaly，無需週期性
                         freq = 12,
                         maxknot = 10,
                         numSam = 10000,
                         burnin = 2000)

# 繪圖
plot(wp_beast_result)