library(readxl)
library(zoo)
library(dplyr)
library(tidyr)
library(Rbeast)
library(lubridate)


oni<-read.csv("oni.csv")
oni_ts <- ts(oni$oni, start = c(min(oni$year), min(oni$month)), frequency = 12)
set.seed(002)
oni_beast_result <- beast(oni_ts,  
                          freq = 12,
                          maxknot = 10,
                          numSam = 10000,
                          burnin = 2000)


plot(oni_beast_result, main="ONI beast decomposition")
decimal_to_ym <- function(decimal_years) {
  years <- floor(decimal_years)
  months <- round((decimal_years - years) * 12) + 1  
  paste0(years, "-", sprintf("%02d", months))
}
plot(oni_beast_result, main="ONI beast decomposition", 
     vars=c("y","s","scp","t","tcp","slpsgn","error"),
     col = c("black", "red","red","blue","blue","yellow","gray"))
sort(decimal_to_ym(oni_beast_result$season$cp))
sort(decimal_to_ym(oni_beast_result$trend$cp))
