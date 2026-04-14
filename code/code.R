library(forecast)
library(ggplot2)
library(tidyverse)
library(scales)

my_theme <- theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12, colour = "black", hjust = 0.5),
    axis.title.x = element_text(angle = 0, colour = "black", face = "bold", size = 11),
    axis.title.y = element_text(colour = "black", face = "bold", size = 11),
    axis.text.x  = element_text(colour = "black", size = 10),
    axis.text.y  = element_text(colour = "black", size = 10),
    legend.title = element_text(face = "bold", size = 10, colour = "black"),
    legend.text  = element_text(size = 10, colour = "black"),
    strip.text   = element_text(face = "bold", size = 10, colour = "black")
  )

raw_data <- read_csv("data/bus.csv", show_col_types = FALSE)

clean_data <- raw_data %>%
  filter(Month != "All months") %>%
  mutate(VALUE = as.numeric(VALUE))

bus_ts <- ts(clean_data$VALUE, start = c(2014, 1), frequency = 12)

bus_ts_millions <- bus_ts / 1e6

summary_stats <- tibble(
  Start = "2014-01",
  End = "2024-12",
  Frequency = 12,
  Observations = length(bus_ts_millions),
  Minimum_Millions = round(min(bus_ts_millions, na.rm = TRUE), 2),
  Maximum_Millions = round(max(bus_ts_millions, na.rm = TRUE), 2),
  Mean_Millions = round(mean(bus_ts_millions, na.rm = TRUE), 2),
  Median_Millions = round(median(bus_ts_millions, na.rm = TRUE), 2),
  SD_Millions = round(sd(bus_ts_millions, na.rm = TRUE), 2)
)

write.csv(summary_stats, "tables/Table0_Summary_Statistics.csv", row.names = FALSE)

p1 <- autoplot(bus_ts_millions, size = 1) +
  ggtitle("Figure 1: Monthly Dublin Bus Passenger Numbers from 2014 to 2024") +
  xlab("Year") +
  ylab("Passenger Numbers (Millions)") +
  my_theme

ggsave("figures/Figure1_TimePlot.png", p1, width = 10, height = 6, dpi = 300)

bus_ts_v2 <- bus_ts_millions
window(bus_ts_v2, start = c(2020, 3), end = c(2021, 12)) <- NA
bus_ts_bridged <- na.interp(bus_ts_v2)

p2 <- autoplot(bus_ts_millions, series = "Original", colour = "gray40", size = 0.9) +
  autolayer(bus_ts_bridged, series = "Adjusted Series", size = 1.1) +
  ggtitle("Figure 2: Original and Adjusted Dublin Bus Passenger Series") +
  xlab("Year") +
  ylab("Passenger Numbers (Millions)") +
  scale_colour_manual(values = c("Original" = "gray40", "Adjusted Series" = "red3")) +
  guides(colour = guide_legend(title = "Series")) +
  my_theme

ggsave("figures/Figure2_Adjustment.png", p2, width = 10, height = 6, dpi = 300)

p3 <- ggseasonplot(
  bus_ts_bridged,
  year.labels = TRUE,
  year.labels.left = TRUE
) +
  ggtitle("Figure 3: Seasonal Plot of Adjusted Dublin Bus Passenger Numbers") +
  xlab("Month") +
  ylab("Passenger Numbers (Millions)") +
  my_theme

ggsave("figures/Figure3_SeasonalPlot.png", p3, width = 10, height = 8, dpi = 300)

fit_stl <- stl(bus_ts_bridged, s.window = "periodic")

p4 <- autoplot(fit_stl) +
  ggtitle("Figure 4: STL Decomposition of Adjusted Dublin Bus Passenger Data") +
  xlab("Year") +
  my_theme

ggsave("figures/Figure4_STL_Decomposition.png", p4, width = 10, height = 6, dpi = 300)

diff_bus <- diff(bus_ts_bridged, lag = 12)

png("figures/Figure5_ACF_PACF.png", width = 1200, height = 600, res = 150)
par(mfrow = c(1, 2))
Acf(na.omit(diff_bus), main = "ACF of Seasonally Differenced Series")
Pacf(na.omit(diff_bus), main = "PACF of Seasonally Differenced Series")
dev.off()

train_data <- window(bus_ts_bridged, end = c(2023, 12))
test_data  <- window(bus_ts_millions, start = c(2024, 1))

fit_snaive <- snaive(train_data, h = 12)
fit_ets <- ets(train_data)
fit_arima <- auto.arima(train_data, stepwise = FALSE, approximation = FALSE)

p6 <- autoplot(test_data, series = "Actual 2024 Data", size = 1.1) +
  autolayer(forecast(fit_snaive, h = 12), series = "Seasonal Naive", PI = FALSE, size = 1) +
  autolayer(forecast(fit_ets, h = 12), series = "ETS Model", PI = FALSE, size = 1) +
  autolayer(forecast(fit_arima, h = 12), series = "ARIMA Model", PI = FALSE, size = 1) +
  ggtitle("Figure 6: Comparison of Forecasting Methods on the 2024 Test Set") +
  xlab("Year") +
  ylab("Passenger Numbers (Millions)") +
  guides(colour = guide_legend(title = "Forecast Method")) +
  my_theme

ggsave("figures/Figure6_Model_Comparison.png", p6, width = 10, height = 6, dpi = 300)

acc_table <- rbind(
  Seasonal_Naive = accuracy(fit_snaive, test_data)["Test set", c("RMSE", "MAE", "MASE")],
  ETS = accuracy(forecast(fit_ets, h = 12), test_data)["Test set", c("RMSE", "MAE", "MASE")],
  ARIMA = accuracy(forecast(fit_arima, h = 12), test_data)["Test set", c("RMSE", "MAE", "MASE")]
)

acc_table <- as.data.frame(acc_table)
acc_table$Model <- rownames(acc_table)
rownames(acc_table) <- NULL
acc_table <- acc_table[, c("Model", "RMSE", "MAE", "MASE")]
acc_table <- acc_table %>%
  mutate(
    RMSE = round(RMSE, 3),
    MAE  = round(MAE, 3),
    MASE = round(MASE, 3)
  )

write.csv(acc_table, "tables/Table1_Model_Accuracy.csv", row.names = FALSE)

png("figures/Figure7_Residual_Diagnostics.png", width = 1200, height = 800, res = 150)
checkresiduals(fit_ets)
dev.off()

final_model <- ets(bus_ts_bridged)
forecast_2025 <- forecast(final_model, h = 12)

p8 <- autoplot(forecast_2025, size = 1) +
  ggtitle("Figure 8: Forecasted Monthly Dublin Bus Passenger Numbers for 2025") +
  xlab("Year") +
  ylab("Passenger Numbers (Millions)") +
  my_theme

ggsave("figures/Figure8_FinalForecast.png", p8, width = 10, height = 6, dpi = 300)

forecast_values <- as.data.frame(forecast_2025)
colnames(forecast_values) <- c(
  "Point Forecast",
  "80% Lower Bound",
  "80% Upper Bound",
  "95% Lower Bound",
  "95% Upper Bound"
)
forecast_values <- round(forecast_values, 2)

write.csv(forecast_values, "tables/Table2_2025_Forecast_Results.csv", row.names = FALSE)