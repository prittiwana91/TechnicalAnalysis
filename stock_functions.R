# BDA400 - Assignment 1 - Pritpal Singh
suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
})

read_symbols <- function(path) {
  x <- trimws(readLines(path, warn = FALSE))
  unique(toupper(x[nzchar(x)]))
}

load_stock_data <- function(path = "portfolio.txt", start_date = "2025-01-01") {
  syms <- read_symbols(path)
  frames <- Map(function(s) {
    res <- try(getSymbols(s, src = "yahoo", from = start_date, auto.assign = FALSE),
               silent = TRUE)
    if (inherits(res, "try-error")) {
      warning("Skipping ", s, " - download failed")
      return(NULL)
    }
    d <- as.data.frame(coredata(res))
    names(d) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    cbind(Date = index(res), d)
  }, syms)
  Filter(Negate(is.null), frames)
}

calculate_statistics <- function(df, symbol) {
  cl <- df$Close
  tab <- table(round(cl))                        # mode of whole-dollar prices
  data.frame(
    Symbol = symbol,
    Mean   = mean(cl),
    Median = median(cl),
    Mode   = as.numeric(names(tab)[which.max(tab)]),
    StdDev = sd(cl),
    SMA20  = tail(SMA(cl, 20), 1),
    SMA50  = tail(SMA(cl, 50), 1),
    row.names = NULL
  )
}

summarise_portfolio <- function(frames) {
  rows <- Map(calculate_statistics, frames, names(frames))
  do.call(rbind, rows)
}
