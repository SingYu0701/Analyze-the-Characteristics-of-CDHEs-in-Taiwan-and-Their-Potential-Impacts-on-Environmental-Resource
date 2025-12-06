library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(scales)# 用來計算 point-biserial correlation

# 1. 讀取資料
file_path <- "D:/成大/資源所/SPI+HWDI/data/resource/north_resource.xlsx"
data <- read_excel(file_path)

# 2. 建立事件變數
data <- data %>%
  mutate(
    Event_t  = ifelse(`Compound event` %in% c("Y"), 1, 0),
    Event_t1 = lag(Event_t, 1) # 前一年事件 t-1
  ) %>%
  arrange(Year)

# 3. 系統名稱
systems <- c("Reservoir", "Rice", "Aquaculture", "Electricity", "Heat-related cases")

# 4. 建立結果資料框
results <- data.frame(System=character(),
                      Effect=character(),
                      Correlation=numeric(),
                      stringsAsFactors = FALSE)

# 5. 逐組合計算 point-biserial correlation
for(sys in systems){
  
  # 當年事件 × 當年偏差
  idx <- !is.na(data[[paste0(sys, " (%)")]]) & !is.na(data$Event_t)
  r1 <- biserial(data[[paste0(sys, " (%)")]][idx],
                 data$Event_t[idx])
  
  results <- rbind(results,
                   data.frame(System=sys,
                              Effect="當年事件 × 當年偏差",
                              Correlation=r1))
  
  # 前一年事件 × 當年偏差
  idx <- !is.na(data[[paste0(sys, " (%)")]]) & !is.na(data$Event_t1)
  r2 <- biserial(data[[paste0(sys, " (%)")]][idx],
                 data$Event_t1[idx])
  
  results <- rbind(results,
                   data.frame(System=sys,
                              Effect="前一年事件 × 當年偏差",
                              Correlation=r2))
  
  # 當年事件 × 年變化
  idx <- !is.na(data[[paste0(sys, " vs prev yr (%)")]]) & !is.na(data$Event_t)
  r3 <- biserial(data[[paste0(sys, " vs prev yr (%)")]][idx],
                 data$Event_t[idx])
  
  results <- rbind(results,
                   data.frame(System=sys,
                              Effect="當年事件 × 年變化",
                              Correlation=r3))
  
  # 前一年事件 × 年變化
  idx <- !is.na(data[[paste0(sys, " vs prev yr (%)")]]) & !is.na(data$Event_t1)
  r4 <- biserial(data[[paste0(sys, " vs prev yr (%)")]][idx],
                 data$Event_t1[idx])
  
  results <- rbind(results,
                   data.frame(System=sys,
                              Effect="前一年事件 × 年變化",
                              Correlation=r4))
}

# 6. 顯示結果
print(results)

results_selected2 <- results %>%
  filter(
    (System == "Reservoir" & Effect %in% c("前一年事件 × 年變化")) |
      (System == "Rice" & Effect == "前一年事件 × 年變化") |
      (System == "Aquaculture" & Effect %in% c("前一年事件 × 年變化","當年事件 × 年變化")) |
      (System == "Electricity" & Effect == "前一年事件 × 年變化") |
      (System == "Heat-related cases" & Effect == "前一年事件 × 年變化")
  ) %>%
  mutate(Effect_Type = ifelse(grepl("前一年", Effect), "lag", "current"))

ggplot(results_selected2, aes(x = reorder(System, Correlation), y = Correlation, fill = Effect_Type)) +
  geom_col(width = 0.5, position = position_dodge(width = 0.6)) +  # 🔹 柱子變細
  geom_text(
    aes(label = round(Correlation, 3)),
    position = position_dodge(width = 0.6),
    hjust = -0.05, size = 4
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(-0.8, 0.8),               
    breaks = seq(-0.8, 0.8, by = 0.2),    
    expand = expansion(mult = c(0, 0.05))
  ) +
  ylab("Correlation") +
  xlab("System") +
  ggtitle("(a) Effects of Compound Drought–Heat Events on Resource Systems in Keelung") +
  scale_fill_manual(values = c("lag" = "orange", "current" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 12),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 14, angle = 20, hjust = 1),
    legend.title = element_text(size = 12, face = "bold"),   
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.6, "lines"),
    legend.position = c(0.9, 0.2),
    legend.background = element_rect(fill = alpha("white", 0.5), color = NA)
  )
