library(ggplot2)


co2_data <- data.frame(
  year = 1979:2024,
  mean = c(336.85,338.91,340.11,340.86,342.53,344.07,345.54,346.97,348.68,
           351.16,352.79,354.06,355.39,356.09,356.83,358.33,360.17,361.93,
           363.05,365.7,367.79,368.96,370.57,372.58,375.14,376.95,378.98,
           381.15,382.9,385.02,386.5,388.75,390.62,392.65,395.4,397.34,
           399.65,403.06,405.22,407.61,410.07,412.44,414.7,417.08,419.36,422.8)
)


ggplot(co2_data, aes(x = year, y = mean)) +
  geom_line(color = "red", size = 1.2) +
  labs(
    title = bquote("Global CO"[2]*" annual trend (1979–2024)"),
    x = "Year",
    y = expression("CO"[2]*" mole fraction (ppm)")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
