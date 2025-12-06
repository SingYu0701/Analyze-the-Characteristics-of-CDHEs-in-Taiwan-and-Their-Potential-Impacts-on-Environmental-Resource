library(readxl)
library(copula)
library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)
library(metR)
all_north <- read_excel("data/north/all_north.xlsx")
oni <- read.csv("data/oni.csv")

##########################
lag_months <- 5
spi_aligned <- all_north$spi[1:(nrow(all_north)-lag_months)]
hwdi_aligned <- all_north$hwdi[(1+lag_months):nrow(all_north)]
data_north <- data.frame(spi=spi_aligned, hwdi=hwdi_aligned)

data_north_prob <- pobs(as.matrix(data_north))
cop_clayton_reverse2 <- rotCopula(claytonCopula(), flip = c(FALSE, TRUE))  # HWDI flipped
fit_reverse2 <- fitCopula(cop_clayton_reverse2, data = data_north_prob, method = "ml")
summary(fit_reverse2)
AIC(fit_reverse2)

spi_thresh <- (-1)
hwdi_thresh <- 0


F_spi_thresh <- ecdf(data_north$spi)(spi_thresh)      # u1 = P(SPI ≤ -1)
F_hwdi_thresh <- ecdf(data_north$hwdi)(hwdi_thresh)   # u2 = P(HWDI ≤ 0)

cop_fitted <- fit_reverse2@copula
C_uv <- pCopula(c(F_spi_thresh, F_hwdi_thresh), cop_fitted)

compound_prob <- F_spi_thresh - C_uv  

cat(sprintf("複合事件機率 P(SPI ≤ %.2f 且 HWDI > %.2f) = %.4f\n", spi_thresh, hwdi_thresh, compound_prob))

persp(fit_reverse2@copula, dCopula, main = "(a) Rotated Clayton: SPI ≤ -1, HWDI > 0 of Keelung",xlab="SPI",ylab="HWDI",
      zlim = c(0, 2.5))

set.seed(123)
sim_uv <- rCopula(10000, cop_fitted)

inv_ecdf_spi <- function(p) quantile(data_north$spi, probs = p, type = 1)
inv_ecdf_hwdi <- function(p) quantile(data_north$hwdi, probs = p, type = 1)

sim_spi <- inv_ecdf_spi(sim_uv[,1])
sim_hwdi <- inv_ecdf_hwdi(sim_uv[,2])

sim_data <- data.frame(spi = sim_spi, hwdi = sim_hwdi)


compound_prob_orig <- mean(data_north$spi <= spi_thresh & data_north$hwdi > hwdi_thresh)


compound_prob_sim <- mean(sim_data$spi <= spi_thresh & sim_data$hwdi > hwdi_thresh)

cond_prob <- mean(sim_data$hwdi > hwdi_thresh & sim_data$spi <= spi_thresh) /
  mean(sim_data$spi <= spi_thresh)


theta <- coef(fit_reverse2)
tau <- cor(data_north$spi, data_north$hwdi, method = "kendall")

cat(sprintf("原始資料複合事件比例 = %.4f\n", compound_prob_orig))
cat(sprintf("Copula 模擬複合事件比例 = %.4f\n", compound_prob_sim))
cat(sprintf("條件機率 P(HWDI > %.2f | SPI <= %.2f) = %.4f\n", hwdi_thresh, spi_thresh, cond_prob))
cat(sprintf("依賴參數 θ = %.4f, Kendall's tau = %.4f\n", theta, tau))


##########################

oni_aligned <- oni$oni[1:nrow(data_north)]  


oni_lag1 <- dplyr::lag(oni_aligned, 1)  # ONI(t-1) 對應 SPI(t)


data_north <- data_north %>%
  mutate(oni = oni_lag1) %>%
  mutate(
    oni_state_monthly = case_when(
      oni >= 0.5  ~ "El_Nino",
      oni <= -0.5 ~ "La_Nina",
      TRUE        ~ "Neutral"
    )
  ) %>%
  mutate(
    el_nino_5mo = rollapply(oni, width = 5, FUN = function(x) all(x >= 0.5), fill = FALSE, align = "right"),
    la_nina_5mo = rollapply(oni, width = 5, FUN = function(x) all(x <= -0.5), fill = FALSE, align = "right")
  ) %>%
  mutate(
    oni_state = case_when(
      el_nino_5mo ~ "El_Nino",
      la_nina_5mo ~ "La_Nina",
      TRUE        ~ "Neutral"
    )
  )
data_north <- data_north %>% slice(2:n())


fit_copula_group <- function(data) {
  if(nrow(data) < 5) {
    return(NULL)
  }
  u_data <- pobs(as.matrix(data[, c("spi", "hwdi")]))
  cop <- rotCopula(claytonCopula(), flip = c(FALSE, TRUE))
  fit <- fitCopula(cop, u_data, method = "ml")
  
  # Kendall tau 與 Pearson rho
  kendall <- cor(data$spi, data$hwdi, method = "kendall")
  pearson <- cor(data$spi, data$hwdi, method = "pearson")
  
  list(
    n = nrow(data),
    rho_copula = coef(fit),
    kendall_tau = kendall,
    pearson_rho = pearson
  )
}


grouped <- group_by(data_north, oni_state_monthly)
results <- group_map(grouped, ~ fit_copula_group(.x))
names(results) <- group_keys(grouped)$oni_state_monthly     


states <- c("El_Nino", "La_Nina", "Neutral")

results_df <- do.call(rbind, lapply(states, function(n) {
  res <- results[[n]]
  if(is.null(res)) {
    return(data.frame(
      oni_state = n,
      n = NA,
      rho_copula = NA,
      kendall_tau = NA,
      pearson_rho = NA
    ))
  }
  data.frame(
    oni_state = n,
    n = res$n,
    copula_theta = as.numeric(res$rho_copula),
    kendall_tau = as.numeric(res$kendall_tau),
    pearson_rho = as.numeric(res$pearson_rho)
  )
}))

plot_data <- results_df %>%
  pivot_longer(cols = c("copula_theta", "kendall_tau", "pearson_rho"),
               names_to = "metric",
               values_to = "value")
print(results_df)

ggplot(plot_data, aes(x = oni_state, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "(a) Comparison of SPI and HWDI Correlation of Keelung",
    x = "ENSO state",
    y = "correlation",
    fill = "index"
  ) +
  theme_minimal() +
  ylim(-0.5, 0.55) +
  scale_fill_manual(values = c(
    "copula_theta" = "#6BAED6",
    "kendall_tau" = "#C7E9C0",
    "pearson_rho" = "#FC9272"
  )) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    axis.title = element_text(size = 14, face = "bold"),              
    axis.text = element_text(size = 12),                            
    legend.title = element_text(size = 14),                       
    legend.text = element_text(size = 12),                            
    legend.key.size = unit(1.5, "lines")                            
  )

#"copula_theta" = "#4A4AFF",
#"kendall_tau" = "#0080FF",
#"pearson_rho" = "#00E3E3"
##############

spi_seq <- seq(-3, 3, length.out = 100)
hwdi_seq <- seq(0, 15, length.out = 100)
grid <- expand.grid(spi = spi_seq, hwdi = hwdi_seq)

F_spi <- ecdf(data_north$spi)(grid$spi)
F_hwdi <- ecdf(data_north$hwdi)(grid$hwdi)


u_mat <- cbind(F_spi, F_hwdi)
C_uv <- pCopula(u_mat, cop_fitted)

compound_prob_grid <- F_spi - C_uv


return_period_grid <- ifelse(compound_prob_grid > 0, 1 / compound_prob_grid, NA)
return_period_grid <- pmin(return_period_grid, 200)

grid$return_period <- return_period_grid

grid_clean <- grid[is.finite(grid$return_period), ]

grid_clean$rp_group <- cut(grid_clean$return_period,
                           breaks = seq(0, 200, length.out = 16))  # 15段

rp_levels <- levels(grid_clean$rp_group)
get_mid <- function(x) {
  nums <- as.numeric(unlist(regmatches(x, gregexpr("[0-9.]+", x))))
  mean(nums)
}
rp_labels <- sapply(rp_levels, get_mid)
rp_labels_fmt <- sprintf("%.1f", rp_labels)

ggplot(grid_clean, aes(x = spi, y = hwdi, fill = return_period)) +
  geom_tile() +
  geom_contour(
    aes(z = return_period),
    color = "white", size = 0.8,
    breaks = c(5, 10, 20, 50, 100, 200)
  ) +
  geom_text_contour(
    aes(z = return_period),
    color = "black", stroke = 0.2,
    size = 4,
    breaks = c(5, 10, 20, 50, 100, 200)
  ) +
  scale_fill_gradientn(
    colors = c("red", "orange"), 
    trans = "log10",
    name = "Return Period\n(months)",
    breaks = c(5, 10, 20, 50, 100, 200),
    labels = c("5", "10", "20", "50", "100", "200"),
    guide = guide_colourbar(
      nbin = 200,
      ticks.colour = "black",
      barheight = unit(6, "cm")
    )
  ) +
  scale_x_continuous(breaks = seq(-3, 3, by = 1)) +
  scale_y_continuous(breaks = 0:15) +
  labs(
    title = "(a) Joint Return Period SPI and HWDI of Keelung",
    x = "SPI", y = "HWDI"
  ) +
  coord_cartesian(ylim = c(0, 12)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")  
  )
