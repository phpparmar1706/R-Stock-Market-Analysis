# =====================================================
# COMPLETE R STUDIO DASHBOARD
# AUTO DOWNLOAD DATA FROM YAHOO FINANCE
# NO MANUAL DATA UPLOAD REQUIRED
# ALL CHARTS IN ONE SCREEN
# =====================================================

# =====================================================
# STEP 1 : INSTALL PACKAGES
# RUN ONLY FIRST TIME
# =====================================================

install.packages("quantmod")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("TTR")
install.packages("forecast")
install.packages("patchwork")

# =====================================================
# STEP 2 : LOAD LIBRARIES
# =====================================================

library(quantmod)
library(ggplot2)
library(dplyr)
library(TTR)
library(forecast)
library(patchwork)

# =====================================================
# STEP 3 : AUTO DOWNLOAD STOCK DATA
# =====================================================

# EXAMPLES:
# UBER
# AAPL
# TSLA
# RELIANCE.NS
# TCS.NS

stock_name <- "UBER"

getSymbols(
  stock_name,
  src = "yahoo",
  from = "2022-01-01",
  auto.assign = TRUE
)

# =====================================================
# STEP 4 : CONVERT DATAFRAME
# =====================================================

uber_data <- data.frame(
  Date = index(get(stock_name)),
  coredata(get(stock_name))
)

# =====================================================
# STEP 5 : RENAME COLUMNS
# =====================================================

colnames(uber_data) <- c(
  "Date",
  "Open",
  "High",
  "Low",
  "Close",
  "Volume",
  "Adjusted"
)

# =====================================================
# STEP 6 : REMOVE MISSING VALUES
# =====================================================

uber_data <- na.omit(uber_data)

# =====================================================
# STEP 7 : DAILY RETURNS
# =====================================================

uber_data$Daily_Return <- c(
  NA,
  diff(uber_data$Close) /
    head(uber_data$Close, -1)
)

# =====================================================
# STEP 8 : MOVING AVERAGES
# =====================================================

uber_data$MA20 <- SMA(
  uber_data$Close,
  20
)

uber_data$MA50 <- SMA(
  uber_data$Close,
  50
)

# =====================================================
# STEP 9 : BUY SELL SIGNAL
# =====================================================

uber_data$Signal <- ifelse(
  uber_data$MA20 > uber_data$MA50,
  "BUY",
  "SELL"
)

# =====================================================
# STEP 10 : KPI VALUES
# =====================================================

latest_close <- tail(
  uber_data$Close,
  1
)

highest_price <- max(
  uber_data$High
)

lowest_price <- min(
  uber_data$Low
)

avg_volume <- mean(
  uber_data$Volume
)

volatility <- sd(
  uber_data$Daily_Return,
  na.rm = TRUE
)

cat("\n========================")
cat("\n STOCK KPI REPORT")
cat("\n========================")

cat(
  "\nLatest Close Price : ",
  latest_close
)

cat(
  "\nHighest Price : ",
  highest_price
)

cat(
  "\nLowest Price : ",
  lowest_price
)

cat(
  "\nAverage Volume : ",
  round(avg_volume, 2)
)

cat(
  "\nVolatility : ",
  round(volatility, 5)
)

# =====================================================
# GRAPH 1 : CLOSE PRICE
# =====================================================

p1 <- ggplot(
  uber_data,
  aes(Date, Close)
) +
  geom_line(
    color = "blue",
    linewidth = 1
  ) +
  ggtitle("Closing Price Trend") +
  theme_minimal()

# =====================================================
# GRAPH 2 : VOLUME
# =====================================================

p2 <- ggplot(
  uber_data,
  aes(Date, Volume)
) +
  geom_col(
    fill = "darkgreen"
  ) +
  ggtitle("Trading Volume") +
  theme_minimal()

# =====================================================
# GRAPH 3 : HIGH VS LOW
# =====================================================

p3 <- ggplot(
  uber_data,
  aes(Date)
) +
  geom_line(
    aes(y = High,
        color = "High")
  ) +
  geom_line(
    aes(y = Low,
        color = "Low")
  ) +
  ggtitle("High vs Low Price") +
  theme_minimal()

# =====================================================
# GRAPH 4 : OPEN VS CLOSE
# =====================================================

p4 <- ggplot(
  uber_data,
  aes(Open, Close)
) +
  geom_point(
    color = "red",
    size = 2
  ) +
  ggtitle("Open vs Close") +
  theme_minimal()

# =====================================================
# GRAPH 5 : MOVING AVERAGES
# =====================================================

p5 <- ggplot(
  uber_data,
  aes(Date)
) +
  geom_line(
    aes(y = Close),
    color = "black"
  ) +
  geom_line(
    aes(y = MA20),
    color = "blue",
    linewidth = 1
  ) +
  geom_line(
    aes(y = MA50),
    color = "red",
    linewidth = 1
  ) +
  ggtitle("Moving Average Analysis") +
  theme_minimal()

# =====================================================
# GRAPH 6 : TREND ANALYSIS
# =====================================================

p6 <- ggplot(
  uber_data,
  aes(Date, Close)
) +
  geom_line(
    color = "blue"
  ) +
  geom_smooth(
    method = "lm",
    color = "red",
    se = FALSE
  ) +
  ggtitle("Trend Analysis") +
  theme_minimal()

# =====================================================
# GRAPH 7 : DAILY RETURN HISTOGRAM
# =====================================================

p7 <- ggplot(
  uber_data,
  aes(Daily_Return)
) +
  geom_histogram(
    bins = 30,
    fill = "skyblue",
    color = "black"
  ) +
  ggtitle("Daily Return Histogram") +
  theme_minimal()

# =====================================================
# GRAPH 8 : BUY SELL SIGNALS
# =====================================================

p8 <- ggplot(
  uber_data,
  aes(Date, Close)
) +
  geom_line(
    color = "black"
  ) +
  geom_point(
    aes(color = Signal),
    size = 2
  ) +
  ggtitle("Buy / Sell Signals") +
  theme_minimal()

# =====================================================
# STEP 11 : TIME SERIES
# =====================================================

close_ts <- ts(uber_data$Close)

# =====================================================
# STEP 12 : ARIMA FORECAST
# =====================================================

arima_model <- auto.arima(close_ts)

forecast_values <- forecast(
  arima_model,
  h = 30
)

png(
  "forecast_plot.png",
  width = 800,
  height = 500
)

plot(
  forecast_values,
  main = "ARIMA Forecast"
)

dev.off()

# =====================================================
# STEP 13 : SHOW ALL CHARTS TOGETHER
# =====================================================

dashboard <-
  (p1 | p2) /
  (p3 | p4) /
  (p5 | p6) /
  (p7 | p8)

print(dashboard)

# =====================================================
# STEP 14 : FINAL DATA
# =====================================================

View(uber_data)

# =====================================================
# END OF PROJECT
# =====================================================