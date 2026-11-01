# BDA400 - Assignment 1 - Pritpal Singh - display and visualisation
source("stock_functions.R")

frames  <- load_stock_data("portfolio.txt")
summary <- summarise_portfolio(frames)

# 1. Compact table, formatted like a quote board
board <- data.frame(Symbol = summary$Symbol,
                    Mean   = format(round(summary$Mean, 2),   nsmall = 2),
                    Median = format(round(summary$Median, 2), nsmall = 2),
                    StdDev = format(round(summary$StdDev, 2), nsmall = 2))
print(board, row.names = FALSE)

# 2. Mean versus median for every stock
barplot(t(as.matrix(summary[, c("Mean", "Median")])),
        beside = TRUE, names.arg = summary$Symbol,
        legend.text = c("Mean", "Median"), main = "Mean vs median close")

# 3. Candlestick-style chart with both moving averages (needs an xts object)
for (sym in names(frames)) {
  d <- frames[[sym]]
  x <- xts(d[, c("Open", "High", "Low", "Close", "Volume")], order.by = d$Date)
  chartSeries(x, name = sym, theme = chartTheme("white"))
  addSMA(n = 20, col = "red")
  addSMA(n = 50, col = "darkgreen")
}
