library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(plotly)
library(htmlwidgets)

#---- Settings -----------------------------------------------------------------

balance <- 1000
balance_label <- "Investing"

assets <- 100
period <- "YTD"

up_color <- "#16A34A"
down_color <- "#DC2626"

watchlist_n <- 2
watchlist_pool <- c("AAPL", "GOOG")

commission <- 0

source("R/config.R")

#---- Helpers ------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || any(is.na(x))) y else x
}

get_range_start <- function(rg, df_max = today, stock_dates = stock_data$date) {
  if (month(df_max) == 2 && day(df_max) == 29) {
    switch(
      rg,
      "1W"  = df_max + days(1) - days(7),
      "1M"  = df_max + days(1) - months(1),
      "3M"  = df_max + days(1) - months(3),
      "YTD" = as.Date(paste(year(df_max), 1, 1, sep = "-")),
      "1Y"  = df_max + days(1) - years(1),
      "5Y"  = df_max + days(1) - years(5),
      "ALL" = min(stock_dates),
      stop("Unknown range: ", rg)
    )
  } else {
    switch(
      rg,
      "1W"  = df_max - days(7) + days(1),
      "1M"  = df_max - months(1) + days(1),
      "3M"  = df_max - months(3) + days(1),
      "YTD" = as.Date(paste(year(df_max), 1, 1, sep = "-")),
      "1Y"  = df_max - years(1) + days(1),
      "5Y"  = df_max - years(5) + days(1),
      "ALL" = min(stock_dates),
      stop("Unknown range: ", rg)
    )
  }
}

#---- Data ---------------------------------------------------------------------

appDir <- getwd()
dataDir <- file.path(appDir, "data")
logsDir  <- file.path(appDir, "logs")

df_list1 <- qs::qread(paste(dataDir, "SP500.qs", sep = "/"))

df <- df_list1[1:assets] %>%
  bind_rows() %>%
  select(
    ticker:index,
    date = ref_date,
    open = price_open,
    high = price_high,
    low = price_low,
    close = price_close,
    volume
  ) %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate(prior_close = lag(close, 1)) %>%
  ungroup()

if (month(max(df$date)) == 2 & day(max(df$date)) == 29) {
  today <- max(df$date) + days(1) - years(1) - days(1)
  if (month(today) == 2 && day(today) == 29) {
    past_3_months  = today + days(1) - months(3)
  } else {
    past_3_months  = today - months(3) + days(1)
  }
} else {
  today <- max(df$date) - years(1)
  if (month(today) == 2 && day(today) == 29) {
    past_3_months  = today + days(1) - months(3)
  } else {
    past_3_months  = today - months(3) + days(1)
  }
}

stock_data <- df %>%
  filter(date <= today, !is.na(prior_close))

tickers <- stock_data %>%
  distinct(ticker) %>%
  pull()

df_list2 <- qs::qread(paste(dataDir, "ratings.qs", sep = "/"))

df1 <- df_list2 %>%
  bind_rows() %>%
  #filter(ticker %in% tickers) %>%
  arrange(ticker, desc(date)) %>%
  group_by(ticker) %>%
  mutate(latest = row_number()) %>%
  ungroup()

df2 <- df1 %>%
  group_by(ticker) %>%
  summarise(factor = n()) %>%
  ungroup() %>%
  mutate(factor = factor / max(factor))

df3 <- df1 %>%
  filter(date >= past_3_months) %>%
  group_by(ticker) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(n = max(n)) %>%
  left_join(., df2) %>%
  mutate(n = n * factor,
         n = ceiling(n)) %>%
  select(-factor)

ratings_data <- df1 %>%
  left_join(., df3) %>%
  mutate(check = ifelse(latest <= n, TRUE, FALSE)) %>%
  filter(check == TRUE) %>%
  select(-c(latest, n, check))

#---- UI helpers ---------------------------------------------------------------

top_bar <- function(left = NULL, center = NULL, right_id) {
  div(
    class = "top-bar",
    div(
      class = "top-bar-inner",
      div(class = "top-bar-side top-bar-left", left),
      div(class = "top-bar-center", center),
      div(
        class = "top-bar-side top-bar-right",
        actionButton(
          inputId = right_id,
          label = "I'm done investing",
          class = "done-investing-button"
        )
      )
    )
  )
}

#---- UI assets ----------------------------------------------------------------

app_css <- tags$style(HTML("
  body {
    margin: 0;
    padding: 0;
    background-color: white;
  }

  .screen {
    display: none;
  }

  .screen-active {
    display: block;
  }

  .web-app {
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    min-height: 100vh;
    position: relative;
  }

  .top-bar {
    position: sticky;
    top: 0;
    z-index: 1000;
    background: #ffffff;
    width: 100vw;
    margin-inline: calc(50% - 50vw);
    transition: box-shadow 0.18s ease, background-color 0.18s ease;
  }

  .top-bar.scrolled {
    box-shadow: 0 1px 3px rgba(48, 51, 51, 0.09), 0 -1px 3px rgba(0, 0, 0, 0.01);
  }

  .top-bar-inner {
    position: relative;
    height: 64px;
    width: 100%;
    padding: 0 24px;
    display: flex;
    align-items: center;
    justify-content: space-between;
  }

  .top-bar-side {
    display: flex;
    align-items: center;
    min-width: 160px;
    height: 100%;
  }

  .top-bar-left {
    justify-content: flex-start;
  }

  .top-bar-right {
    justify-content: flex-end;
    gap: 12px;
  }

  .top-bar-center {
    position: absolute;
    left: 50%;
    top: 50%;
    transform: translate(-50%, -50%);
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    min-width: 160px;
    max-width: 320px;
    text-align: center;
    line-height: 1.05;
    white-space: nowrap;
    opacity: 0;
    pointer-events: none;
    transition: opacity 0.18s ease;
  }

  .top-bar.scrolled .top-bar-center {
    opacity: 1;
  }

  .top-bar-title {
    font-size: 13px;
    font-weight: 700;
    color: #6B7280;
    margin: 0;
    letter-spacing: -0.1px;
  }

  .top-bar-value {
    font-size: 13px;
    font-weight: 700;
    color: #111827;
    margin: 2px 0 0 0;
    letter-spacing: -0.1px;
  }

  .nav-icon-button,
  .done-investing-button {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    height: 40px;
    min-height: 40px;
    border: none;
    border-radius: 10px;
    background: transparent;
    color: #111827;
    box-shadow: none;
    transition: background-color 0.15s ease, color 0.15s ease;
  }

  .nav-icon-button {
    min-width: 40px;
    padding: 0 10px;
    font-size: 18px;
    font-weight: 500;
  }

  .done-investing-button {
    padding: 0 12px;
    font-size: 13px;
    font-weight: 700;
    white-space: nowrap;
  }

  .nav-icon-button:hover,
  .done-investing-button:hover {
    background-color: transparent;
    color: #16A34A;
  }

  .nav-icon-button:focus,
  .nav-icon-button:active,
  .done-investing-button:focus,
  .done-investing-button:active {
    outline: none;
    box-shadow: none;
    background-color: #f3f4f6;
  }

  .bottom-split {
    box-sizing: border-box;
    max-width: 1440px;
    min-width: 1049px;
    padding: 0 24px;
    display: flex;
    align-items: flex-start;
    flex-flow: wrap;
    justify-content: flex-start;
    margin: 0 -12px;
    width: calc(100% + 24px);
  }

  .main-pane {
    display: initial;
    flex: 0 0 calc(75% - 24px);
    margin: 0 12px;
    min-width: 0;
    padding-top: 36px;
    padding-right: 24px;
    width: auto;
  }

  .side-pane {
    display: initial;
    flex: 0 0 calc(25% - 24px);
    width: calc(25% - 24px);
    box-sizing: content-box;
    height: calc(100vh - 64px - 48px);
    margin: 0 -12px -64px;
    overflow-y: auto;
    padding: 36px 24px 24px;
    position: sticky;
    scrollbar-width: none;
    top: 64px;
  }

  .side-box {
    display: flex;
    flex-direction: column;
    height: 100%;
    position: relative;
    border: 1px solid #d1d5db;
    border-radius: 4px;
    box-sizing: border-box;
    width: auto;
    direction: ltr;
    will-change: transform;
    overflow: hidden;
    max-height: 532px;
  }

  .side-box-title {
    height: 52px;
    left: 0px;
    width: 100%;
    border-bottom: 1px solid #d1d5db;
    align-items: center;
    display: flex;
    justify-content: space-between;
    padding: 0px 28px 0px 24px;
    font-size: 15px;
    font-weight: 700;
    color: black;
  }

  .side-section-title {
    height: 60px;
    left: 0px;
    width: 100%;
    align-items: center;
    display: flex;
    justify-content: space-between;
    padding: 0px 28px 0px 24px;
    font-size: 13px;
    font-weight: 400;
    color: black;
  }

  .side-divider {
    border-top: 1px solid #e5e7eb;
    margin: 16px 0;
  }

  .watchlist-item {
    height: 60px;
    left: 0px;
    width: 100%;
    align-items: center;
    display: flex;
    justify-content: space-between;
    padding: 0px 24px;
  }
  
  .watchlist-item.btn,
  .watchlist-item.btn-default {
    text-align: left !important;
  }

  .watchlist-left {
    flex: 0 1 50%;
    margin-right: 12px;
    min-width: 0px;
    padding-bottom: 2px;
  }

  .watchlist-ticker {
    color: black;
    font-size: 13px;
    font-weight: 700;
    letter-spacing: -0.1px;
    line-height: 1.53;
    display: block;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .watchlist-chart {
    height: 100%;
    width: 60px;
    align-items: center;
    display: flex;
    overflow: visible;
  }

  .watchlist-right {
    flex: 0 1 50%;
    margin-left: 12px;
    text-align: right;
    display: inline-flex;
    flex-direction: column;
  }

  .watchlist-price {
    color: black;
    font-size: 15px;
    font-weight: 400;
    letter-spacing: -0.1px;
    line-height: 1.6;
  }

  .watchlist-pct {
    margin-top: 2px;
    font-size: 15px;
    font-weight: 400;
    letter-spacing: -0.1px;
    line-height: 1.6;
    overflow: hidden;
  }

  .holding-shares-label {
    color: #6B7280;
    font-size: 13px;
    font-weight: 400;
    line-height: 1.3;
    margin-top: 2px;
  }

  .account-balance {
    display: flex;
    flex-direction: column;
    gap: 4px;
    margin-bottom: 24px;
  }

  .balance-title {
    font-size: 32px;
    font-weight: 500;
    margin-top: 0px;
    line-height: 1.15;
  }

  .balance-value {
    font-size: 32px;
    font-weight: 500;
    margin: 0;
    line-height: 1.15;
  }

  .list-chart-wrap {
    margin-bottom: 24px;
  }

  .list-funds-row {
    display: flex;
    gap: 200px;
    flex-wrap: wrap;
    align-items: center;
    color: black;
    font-size: 13px;
    font-weight: 400;
    padding: 8px 14px;
    text-align: center;
    line-height: 1.2;
    margin: 12px -14px 24px;
    min-width: 52px;
  }

  .explore-assets-title {
    width: 100%;
    margin-bottom: 24px;
    border-bottom: none;
    padding-bottom: 16px;
    font-size: 24px;
    font-weight: 500;
  }

  .asset-filter-wrap {
    width: 100%;
    border-bottom: 1px solid #d1d5db;
    margin-bottom: 0;
    padding-bottom: 8px;
  }

  .asset-filter-row {
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
    align-items: center;
  }

  .asset-filter-button {
    background-color: transparent;
    border: none;
    color: black;
    font-size: 13px;
    font-weight: 700;
    padding: 8px 0px;
    line-height: 1.2;
    border-radius: 0;
    box-shadow: none;
  }

  .asset-filter-button:hover,
  .asset-filter-button:focus,
  .asset-filter-button:active {
    background-color: transparent !important;
    color: #16A34A !important;
    box-shadow: none !important;
    outline: none;
  }

  .asset-table-header {
    width: 100%;
    display: flex;
    align-items: center;
    padding: 12px 0px;
    border-bottom: 1px solid #d1d5db;
  }

  .asset-col-name {
    flex: 1 1 40%;
    text-align: left;
    font-size: 13px;
    font-weight: 400;
    color: black;
  }

  .asset-col-price {
    flex: 0 0 250px;
    text-align: right;
    font-size: 13px;
    font-weight: 400;
    color: black;
  }

  .asset-col-ytd {
    flex: 0 0 250px;
    text-align: right;
    font-size: 13px;
    font-weight: 400;
    color: black;
  }

  .asset-list {
    width: 100%;
  }

  .asset-row-button {
    width: 100%;
    border: none;
    border-bottom: 1px solid #d1d5db;
    background-color: transparent;
    padding: 0;
    border-radius: 0;
    box-shadow: none;
  }

  .asset-row-button:hover,
  .asset-row-button:focus,
  .asset-row-button:active {
    background-color: #f3f4f6 !important;
    cursor: pointer;
    border-radius: 0;
    box-shadow: none !important;
    outline: none;
  }

  .asset-row-inner {
    display: flex;
    align-items: center;
    width: 100%;
    padding: 16px 0;
  }

  .asset-name {
    flex: 1 1 40%;
    text-align: left;
    font-size: 13px;
    font-weight: 700;
    color: black;
    line-height: 1.3;
  }

  .asset-price {
    flex: 0 0 250px;
    text-align: right;
    font-size: 13px;
    font-weight: 400;
    line-height: 1.3;
  }

  .asset-ytd {
    flex: 0 0 250px;
    text-align: right;
    font-size: 13px;
    font-weight: 400;
    line-height: 1.3;
  }

  .section {
    align-items: flex-start;
    display: flex;
    flex-flow: wrap;
    justify-content: flex-start;
    margin-bottom: 48px;
  }

  .section-title {
    width: 100%;
    margin-bottom: 24px;
    border-bottom: 1px solid #d1d5db;
    padding-bottom: 16px;
    font-size: 24px;
    font-weight: 500;
  }

  .stocks-separator {
    border-top: 1px solid #dddddd;
    margin-top: 12px;
    margin-bottom: 0px;
  }

  .card-button {
    width: 100%;
    border: none;
    background-color: transparent;
  }

  .card-button:hover,
  .card-button:focus,
  .card-button:active  {
    background-color: #f3f4f6 !important;
    cursor: pointer;
    border-radius: 0;
    box-shadow: none;
  }

  .detail-header {
    display: flex;
    flex-direction: column;
    gap: 4px;
    margin-bottom: 24px;
  }

  .detail-metrics-row {
    display: flex;
    gap: 4px;
    flex-wrap: wrap;
  }

  .detail-metrics-inline {
    display: flex;
    gap: 4px;
    flex-wrap: wrap;
    align-items: baseline;
  }

  .detail-metric {
    font-size: 13px;
    font-weight: 700;
    line-height: 1.2;
  }

  .detail-metric-period {
    font-weight: 400;
    color: #6B7280;
  }

  .detail-chart-wrap {
    height: 100%;
  }

  .range-button-wrap {
    margin: 12px -14px 24px;
  }

  .range-button-row {
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
    align-items: center;
  }

  .range-button {
    background-color: transparent;
    border: none;
    color: black;
    font-size: 13px;
    font-weight: 400;
    padding: 8px 14px;
    border-radius: 10px;
    min-width: 52px;
    text-align: center;
    line-height: 1.2;
  }

  .range-button:hover {
    background-color: transparent;
    cursor: pointer;
    color: #16A34A;
  }

  .range-button-active {
    background-color: transparent;
    color: black;
    font-weight: 700;
  }

  .detail-stats-wrap {
  }

  .detail-stat-row {
    display: flex;
    gap: 20px;
    margin-bottom: 18px;
    flex-wrap: wrap;
  }

  .detail-stat-row:last-child {
    margin-bottom: 0;
  }

  .detail-stat-box {
    flex: 1 1 calc(25% - 15px);
    min-width: 140px;
  }

  .detail-stat-box-empty {
    visibility: hidden;
  }

  .detail-stat-label {
    font-size: 13px;
    font-weight: 700;
    color: black;
    margin-bottom: 5px;
  }

  .detail-stat-value {
    font-size: 13px;
    font-weight: 400;
    color: black;
    line-height: 1.2;
  }

  .detail-stat-value-small {
    font-size: 13px;
    font-weight: 400;
    color: black;
    line-height: 1.2;
  }

  .detail-about-wrap {
  }

  .detail-about-row {
    display: flex;
    gap: 20px;
    flex-wrap: nowrap;
    overflow-x: auto;
  }

  .detail-about-pill {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    padding: 8px 14px;
    border-radius: 999px;
    background-color: rgba(22, 163, 74, 0.12);
    color: #16A34A;
    font-size: 13px;
    font-weight: 700;
    white-space: nowrap;
  }

  .detail-ratings-slot {
    width: 100%;
    flex: 0 0 100%;
  }

  .detail-ratings-slot > .shiny-html-output,
  .detail-ratings-slot > div {
    width: 100%;
  }

  .detail-ratings-wrap {
    width: 100%;
    flex: 0 0 100%;
  }

  .detail-ratings-row {
    display: grid;
    grid-template-columns: 134px 1fr;
    column-gap: 32px;
    align-items: center;
    width: 100%;
  }

  .detail-ratings-summary {
    width: 134px;
    height: 134px;
    min-width: 134px;
    border-radius: 50%;
    background: rgba(22, 163, 74, 0.12);
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
  }

  .detail-ratings-summary-pct {
    font-size: 24px;
    font-weight: 500;
    line-height: 1;
    color: #16A34A;
    margin-bottom: 6px;
  }

  .detail-ratings-summary-n {
    font-size: 13px;
    font-weight: 700;
    line-height: 1.2;
    color: #16A34A;
    text-align: center;
  }

  .detail-ratings-bars {
    width: 100%;
  }

  .detail-ratings-bar-row {
    display: flex;
    align-items: center;
    width: 100%;
    gap: 10px;
    margin-bottom: 28px;
  }

  .detail-ratings-bar-row:last-child {
    margin-bottom: 0px;
  }

  .detail-ratings-track-wrap {
    flex: 1 1 auto;
    min-width: 0;
  }

  .detail-ratings-label {
    flex: 0 0 34px;
    font-size: 13px;
    font-weight: 700;
    color: black;
    line-height: 1;
  }

  .detail-ratings-label-buy {
    color: #16A34A;
  }

  .detail-ratings-track {
    position: relative;
    width: 100%;
    height: 6px;
    border-radius: 999px;
    background: rgba(209, 213, 219, 1);
    overflow: visible;
  }

  .detail-ratings-track-buy {
    background: rgba(22, 163, 74, 0.12);
  }

  .detail-ratings-fill {
    height: 100%;
    border-radius: 999px;
  }

  .detail-ratings-fill-buy {
    background: #16A34A;
  }

  .detail-ratings-fill-neutral {
    background: black;
  }

  .detail-ratings-value {
    position: absolute;
    top: 50%;
    transform: translate(-2px, -50%);
    font-size: 13px;
    font-weight: 700;
    color: black;
    background: white;
    padding-inline: 2px;
    white-space: nowrap;
    line-height: 1;
  }

  .detail-ratings-value-buy {
    color: #16A34A;
  }

  .order-row {
    height: 45px;
    left: 0px;
    width: 100%;
    align-items: center;
    display: flex;
    justify-content: space-between;
    padding: 0px 24px;
    font-size: 13px;
    font-weight: 400;
    color: black;
  }

  .order-row-divider {
    position: relative;
  }

  .order-row-divider::after {
    content: \"\";
    position: absolute;
    left: 24px;
    right: 24px;
    bottom: 0;
    border-bottom: 1px solid #d1d5db;
  }

  .order-label {
    text-align: left;
  }

  .order-value {
    text-align: right;
  }
  
  .order-control-lock {
    opacity: 0.45;
    pointer-events: none;
  }

  .order-message-wrap {
    margin-bottom: 24px;
  }

  .order-message-title {
    font-size: 13px;
    font-weight: 700;
    color: black;
    line-height: 1.35;
    margin-bottom: 4px;
  }

  .order-message-text {
    font-size: 13px;
    font-weight: 400;
    color: #6B7280;
    line-height: 1.45;
  }

  .review-order-wrap-lower {
    padding-top: 24px;
  }

  .review-order-btn-back {
    display: block;
    width: 100%;
    background-color: transparent;
    color: #16A34A;
    border: 1px solid #16A34A;
    border-radius: 999px;
    font-size: 13px;
    font-weight: 700;
    padding: 12px 16px;
  }

  .review-order-btn-back:hover {
    background-color: rgba(22, 163, 74, 0.12);
    color: #16A34A;
    border-color: #16A34A;
  }

  .review-order-wrap {
    padding: 24px;
    margin-top: auto;
  }

  .review-order-btn {
    display: block;
    width: 100%;
    background-color: #16A34A;
    color: black;
    border: none;
    border-radius: 999px;
    font-size: 13px;
    font-weight: 700;
    padding: 12px 16px;
  }

  .review-order-btn:hover {
    background-color: rgba(22, 163, 74, 0.12);
    color: black;
  }

  #order_mode {
    width: 120px;
    margin-bottom: 0;
    display: flex;
    align-items: center;
  }

  #order_mode .form-group {
    margin-bottom: 0;
    width: 100%;
  }
  
  #order_mode .bootstrap-select {
    width: 100% !important;
  }

  #order_mode .dropdown-toggle {
    height: 34px;
    padding: 4px 10px;
    border: 1px solid #d1d5db;
    border-radius: 0px;;
    background-color: white;
    color: black;
    display: flex;
    align-items: center;
    justify-content: space-between;
    box-shadow: none !important;
    font-weight: 400;
  }

  #order_mode .bootstrap-select.open .dropdown-toggle,
  #order_mode .dropdown-toggle:focus,
  #order_mode .dropdown-toggle:active {
    border-color: #16A34A !important;
    outline: none !important;
    box-shadow: none !important;
  }

  /* dropdown menu items */
  #order_mode .dropdown-menu li a,
  #order_mode .dropdown-menu li span.text {
    color: black !important;
    font-weight: 400 !important;
  }

  /* selected item in opened dropdown */
  #order_mode .dropdown-menu li.selected a,
  #order_mode .dropdown-menu li.selected a span.text {
    background-color: transparent !important;
    color: black !important;
    font-weight: 400 !important;
  }

  /* optional: keep hover subtle and not blue */
  #order_mode .dropdown-menu li a:hover,
  #order_mode .dropdown-menu li a:focus {
    background-color: #f3f4f6 !important;
    color: black !important;
  }

  #order_shares_wrap {
    width: 120px;
    margin-bottom: 0;
    display: flex;
    align-items: center;
  }
  
  #order_shares_wrap input::placeholder {
    color: black;
    opacity: 1;
  }

  #order_shares_wrap input:focus::placeholder {
    color: #d1d5db;
    opacity: 1;
  }

  #order_shares_wrap .form-group {
    margin-bottom: 0;
  width: 100%;
  }

  #order_shares_wrap .form-control {
    height: 34px;
    padding: 4px 10px;
    border: 1px solid #d1d5db;
    border-radius: 0px;
    background-color: white;
    color: black;
    text-align: right;
    box-shadow: none !important;
    font-weight: 400;
  }

  #order_shares_wrap .form-control:focus {
    border-color: #16A34A !important;
    box-shadow: none !important;
    outline: none !important;
  }

  .detail-side-balance {
    height: 52px;
    border-top: 1px solid #d1d5db;
    align-items: center;
    display: flex;
    justify-content: center;
    padding: 0px 24px;
    font-size: 13px;
    font-weight: 400;
    color: black;
  }

  .review-modal .modal-dialog {
    width: 420px;
    max-width: calc(100vw - 32px);
    margin: 40px auto;
  }

  .review-modal .modal-content {
    border-radius: 4px;
    border: none;
    box-shadow: 0 12px 30px rgba(0, 0, 0, 0.12);
    padding: 0;
  }

  .review-modal .modal-body {
    padding: 24px;
  }

  .review-modal-close {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    min-height: 40px;
    min-width: 40px;
    border: none;
    border-radius: 10px;
    background: transparent;
    margin-top: -24px;
    margin-left: -24px;
    padding: 0 10px;
    color: #111827;
    box-shadow: none;
    font-size: 18px;
    font-weight: 500;
  }

  .review-modal-close:hover {
    background-color: transparent;
    color: #16A34A;
  }
  
  .review-modal-title {
    font-size: 24px;
    font-weight: 500;
    color: black;
    margin-bottom: 4px;
  }

  .review-modal-balance {
    font-size: 13px;
    font-weight: 400;
    color: #6B7280;
    margin-bottom: 24px;
  }

  .review-modal-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 16px;
    font-size: 13px;
    font-weight: 400;
    color: black;
    padding: 12px 0;
  }

  .review-modal-divider {
    border-bottom: 1px solid #d1d5db;
  }

  .review-modal-label {
    font-weight: 400;
  }

  .review-modal-value {
    font-weight: 700;
    text-align: right;
  }

  .review-modal-subheading {
    font-size: 13px;
    font-weight: 700;
    color: black;
    margin: 24px 0 4px 0;
  }

  .review-modal-text {
    font-size: 13px;
    font-weight: 400;
    color: black;
    line-height: 1.5;
    margin-bottom: 24px;
  }

  .review-submit-btn {
    width: 100%;
    background-color: #16A34A;
    color: black;
    border: none;
    border-radius: 999px;
    font-size: 13px;
    font-weight: 700;
    padding: 12px 16px;
  }

  .review-submit-btn:hover {
    background-color: rgba(22, 163, 74, 0.12);
    color: black;
  }
"))

app_js <- tags$script(HTML("
  var listScrollY = 0;

  Shiny.addCustomMessageHandler('saveListScroll', function(message) {
    listScrollY = window.pageYOffset || document.documentElement.scrollTop || 0;
  });

  Shiny.addCustomMessageHandler('restoreListScroll', function(message) {
    setTimeout(function() {
      window.scrollTo(0, listScrollY);
      updateStickyBars();
    }, 10);
  });

  Shiny.addCustomMessageHandler('scrollListToTop', function(message) {
    setTimeout(function() {
      window.scrollTo({
        top: 0,
        left: 0,
        behavior: message.behavior || 'instant'
      });
      updateStickyBars();
    }, 10);
  });
  
  Shiny.addCustomMessageHandler('scrollDetailToTop', function(message) {
    setTimeout(function() {
      window.scrollTo({ top: 0, left: 0, behavior: 'instant' });
      updateStickyBars();
    }, 10);
  });

  function updateStickyBars() {
    var isScrolled = (window.pageYOffset || document.documentElement.scrollTop || 0) > 0;
    var bars = document.querySelectorAll('.top-bar');
    bars.forEach(function(bar) {
      if (isScrolled) {
        bar.classList.add('scrolled');
      } else {
        bar.classList.remove('scrolled');
      }
    });
  }

  window.addEventListener('scroll', updateStickyBars);
  document.addEventListener('DOMContentLoaded', updateStickyBars);
"))

#---- UI: list screen ----------------------------------------------------------

list_screen <- div(
  id = "screen_home", class = "screen screen-active",
  div(
    class = "web-app",
    top_bar(
      left = actionButton(
        "go_home",
        label = NULL,
        icon = icon("home"),
        class = "nav-icon-button",
        title = "Home"
      ),
      center = tagList(
        div(class = "top-bar-value", textOutput("balance_header_display", inline = TRUE)),
        div(class = "top-bar-title", "Investing")
      ),
      right_id = "done_home"
    ),
    div(
      class = "bottom-split",
      div(
        class = "main-pane",
        fluidRow(
          column(
            width = 12,
            div(
              class = "account-balance",
              div(class = "balance-title", balance_label),
              div(class = "balance-value", textOutput("balance_display", inline = TRUE))
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "list-chart-wrap",
              plotlyOutput("portfolio_donut", height = "260px", width = "100%")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "list-funds-row",
              div("Available Funds"),
              div(textOutput("available_funds_display", inline = TRUE))
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "section",
              div(class = "explore-assets-title", "Explore Assets"),
              div(
                class = "asset-filter-wrap",
                div(
                  class = "asset-filter-row",
                  actionButton(
                    inputId = "asset_filter_stocks",
                    label = "Stocks",
                    class = "asset-filter-button"
                  )
                )
              ),
              div(
                class = "asset-table-header",
                div(class = "asset-col-name", "Name"),
                div(class = "asset-col-price", "Price"),
                div(class = "asset-col-ytd", "Year to date")
              ),
              uiOutput("stock_cards")
            )
          )
        )
      ),
      div(
        class = "side-pane",
        uiOutput("list_side_box")
      )
    )
  )
)

#---- UI: detail screen --------------------------------------------------------

detail_screen <- div(
  id = "screen_asset", class = "screen",
  div(
    class = "web-app",
    top_bar(
      left = actionButton(
        "back_to_home",
        label = NULL,
        icon = icon("chevron-left"),
        class = "nav-icon-button",
        title = "Back"
      ),
      center = uiOutput("detail_header_center"),
      right_id = "done_asset"
    ),
    div(
      class = "bottom-split",
      div(
        class = "main-pane",
        fluidRow(
          column(
            width = 12,
            div(
              class = "detail-header",
              div(class = "balance-title", textOutput("detail_ticker", inline = TRUE)),
              div(class = "balance-value", textOutput("detail_price", inline = TRUE)),
              div(
                class = "detail-metrics-row",
                uiOutput("detail_metrics")
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "detail-chart-wrap",
              plotlyOutput("detail_plot", height = "260px", width = "100%")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "range-button-wrap",
              uiOutput("range_buttons")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "section",
              div(class = "section-title", textOutput("detail_about_title", inline = TRUE)),
              uiOutput("detail_about_company")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "section",
              div(class = "section-title", textOutput("detail_stats_title", inline = TRUE)),
              uiOutput("detail_key_stats")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "section",
              div(class = "section-title", textOutput("detailratings_title", inline = TRUE)),
              div(class = "detail-ratings-slot", uiOutput("detail_ratings"))
            )
          )
        ),
      ),
      div(
        class = "side-pane",
        uiOutput("detail_side_box")
      )
    )
  )
)

#---- UI -----------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    app_css,
    app_js
  ),
  list_screen,
  detail_screen
)

#---- Server -------------------------------------------------------------------

server <- function(input, output, session) {
  
  live_mode <- reactiveVal(FALSE)
  
  # --- Participant identifiers ------------------------------------------------
  prolific_pid <- reactiveVal(NA_character_)
  study_id <- reactiveVal(NA_character_)
  session_id <- reactiveVal(NA_character_)
  
  # log of all interactions and trades
  interactions <- reactiveVal(
    tibble(
      prolific_pid = character(),
      study_id     = character(),
      session_id   = character(),
      timestamp    = as.POSIXct(character()),
      button_id    = character(),
      button_label = character(),
      page         = character(),        # "home" or "asset"
      asset        = character(),        # NA on home page
      shares       = numeric(),          # NA unless shares-related
      unit_price   = numeric(),          # NA unless order submitted
      total_cost   = numeric()           # NA unless order submitted
    )
  )
  
  observe({
    query <- parseQueryString(isolate(session$clientData$url_search))
    
    prolific_raw <- query[["PROLIFIC_PID"]]
    study_raw <- query[["STUDY_ID"]]
    session_raw <- query[["SESSION_ID"]]
    
    prolific_val <- if (!is.null(prolific_raw) && prolific_raw != "") prolific_raw else NA_character_
    study_val <- if (!is.null(study_raw) && study_raw != "") study_raw else NA_character_
    session_val <- if (!is.null(session_raw) && session_raw != "") session_raw else NA_character_
    
    prolific_pid(prolific_val)
    study_id(study_val)
    session_id(session_val)
    
    live_mode(!is.na(prolific_val) || !is.na(study_val) || !is.na(session_val))
  })
  
  # ---- Log initial deposit (once per session) -------------------------------
  deposit_logged <- reactiveVal(FALSE)
  
  observe({
    # Wait until the query observer has run at least once
    if (!deposit_logged() && !is.null(prolific_pid())) {
      log_interaction(
        "deposit",
        "Deposit funds",
        page = current_screen(),
        asset = "cash",
        shares = 1,
        unit_price = balance,
        total_cost = balance
      )
      deposit_logged(TRUE)
    }
  })
  
  #---- Server: state ----------------------------------------------------------
  
  n_stocks <- reactiveVal(assets)
  current_screen <- reactiveVal("home")
  selected_ticker <- reactiveVal(NULL)
  selected_range <- reactiveVal(period)
  
  cash_balance <- reactiveVal(balance)
  
  holdings <- reactiveVal(
    tibble(
      ticker = character(),
      shares = numeric()
    )
  )
  
  order_shares_val <- reactiveVal("")
  order_review_state <- reactiveVal("idle")
  
  watchlist_tickers <- reactiveVal(
    sample(watchlist_pool, size = min(watchlist_n, length(watchlist_pool)), replace = FALSE)
  )
  
  #---- Server: shared reactives ----------------------------------------------
  
  shown_tickers <- reactive({
    head(tickers, n_stocks())
  })
  
  range_start <- reactive({
    get_range_start(selected_range())
  })
  
  filtered_history <- reactive({
    stock_data %>%
      filter(date >= range_start())
  }) %>%
    bindCache(selected_range())
  
  ticker_summary <- reactive({
    filtered_history() %>%
      group_by(ticker) %>%
      summarise(
        baseline = first(prior_close),
        close = last(close),
        abs_change = close - baseline,
        pct_change = (close / baseline - 1) * 100,
        change_color = if_else(abs_change >= 0, up_color, down_color),
        .groups = "drop"
      )
  }) %>%
    bindCache(selected_range())
  
  hist_by_ticker <- reactive({
    split(filtered_history(), filtered_history()$ticker)
  }) %>%
    bindCache(selected_range())
  
  watchlist_start <- reactive({
    as.Date(paste(year(today), 1, 1, sep = "-"))
  })
  
  watchlist_history <- reactive({
    stock_data %>%
      filter(date >= watchlist_start())
  })
  
  watchlist_summary <- reactive({
    watchlist_history() %>%
      group_by(ticker) %>%
      summarise(
        baseline = first(prior_close),
        close = last(close),
        abs_change = close - baseline,
        pct_change = (close / baseline - 1) * 100,
        change_color = if_else(abs_change >= 0, up_color, down_color),
        .groups = "drop"
      )
  })
  
  watchlist_hist_by_ticker <- reactive({
    split(watchlist_history(), watchlist_history()$ticker)
  })
  
  portfolio_summary <- reactive({
    hold_df <- holdings()
    
    invested_df <- tibble(
      asset = character(),
      value = numeric()
    )
    
    if (nrow(hold_df) > 0) {
      invested_df <- hold_df %>%
        left_join(
          ticker_summary() %>% select(ticker, close),
          by = "ticker"
        ) %>%
        mutate(
          value = shares * close,
          asset = ticker
        ) %>%
        filter(!is.na(value), value > 0) %>%
        select(asset, value)
    }
    
    bind_rows(
      tibble(asset = "Cash", value = cash_balance()),
      invested_df
    ) %>%
      filter(value > 0)
  })
  
  #---- Server: detail reactives ----------------------------------------------
  
  detail_history <- reactive({
    req(selected_ticker())
    hist_by_ticker()[[selected_ticker()]]
  }) %>%
    bindCache(selected_ticker(), selected_range())
  
  detail_summary <- reactive({
    req(selected_ticker())
    ticker_summary() %>%
      filter(ticker == selected_ticker())
  }) %>%
    bindCache(selected_ticker(), selected_range())
  
  hovered_point <- reactive({
    d <- event_data("plotly_hover", source = "asset", priority = "event")
    
    if (is.null(d)) {
      detail_history() %>% slice_tail(n = 1)
    } else {
      detail_history() %>% slice(d$pointNumber + 1)
    }
  })
  
  hovered_metrics <- reactive({
    hp <- hovered_point()
    req(nrow(hp) == 1)
    
    baseline <- detail_history() %>% slice(1) %>% pull(prior_close)
    abs_change <- hp$close - baseline
    pct_change <- (hp$close / baseline - 1) * 100
    change_color <- ifelse(abs_change >= 0, up_color, down_color)
    
    list(
      date = hp$date,
      close = hp$close,
      abs_change = abs_change,
      pct_change = pct_change,
      change_color = change_color
    )
  })
  
  #---- Server: helpers --------------------------------------------------------
  
  log_interaction <- function(
    button_id,
    button_label = NA_character_,
    page = NA_character_,
    asset = NA_character_,
    shares = NA_real_,
    unit_price = NA_real_,
    total_cost = NA_real_
  ) {
    new_row <- tibble(
      prolific_pid = prolific_pid(),
      study_id     = study_id(),
      session_id   = session_id(),
      timestamp    = Sys.time(),
      button_id    = button_id,
      button_label = button_label,
      page         = page,
      asset        = asset,
      shares       = shares,
      unit_price   = unit_price,
      total_cost   = total_cost
    )
    
    interactions(bind_rows(interactions(), new_row))
  }
  
  mini_chart <- function(tk) {
    dfc <- hist_by_ticker()[[tk]]
    sum_row <- ticker_summary() %>% filter(ticker == tk)
    
    ggplot(dfc, aes(x = date, y = close)) +
      geom_hline(
        yintercept = sum_row$baseline,
        linetype = "dotted",
        color = "lightgrey",
        linewidth = 0.5
      ) +
      geom_line(color = sum_row$change_color, linewidth = 0.7) +
      theme_minimal(base_size = 9) +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  }
  
  watchlist_chart <- function(tk) {
    dfc <- watchlist_hist_by_ticker()[[tk]]
    sum_row <- watchlist_summary() %>% filter(ticker == tk)
    
    ggplot(dfc, aes(x = date, y = close)) +
      geom_hline(
        yintercept = sum_row$baseline,
        linetype = "dotted",
        color = "lightgrey",
        linewidth = 0.5
      ) +
      geom_line(color = sum_row$change_color, linewidth = 0.7) +
      theme_minimal(base_size = 9) +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background  = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(0, 0, 0, 0)
      )
  }
  
  fmt_dollar <- function(x) {
    x_num <- as.numeric(x)
    paste0("$", format(round(x_num, 2), nsmall = 2, big.mark = ",", scientific = FALSE))
  }
  
  fmt_dollar_signed <- function(x) {
    x_num <- as.numeric(x)
    prefix <- if (x_num >= 0) "+" else "-"
    paste0(prefix, "$", format(round(abs(x_num), 2), nsmall = 2, big.mark = ",", scientific = FALSE))
  }
  
  fmt_pct_signed <- function(x) {
    x_num <- as.numeric(x)
    prefix <- if (x_num >= 0) "+" else "-"
    paste0(prefix, format(round(abs(x_num), 2), nsmall = 2, big.mark = ",", scientific = FALSE), "%")
  }
  
  fmt_int <- function(x) {
    x_num <- as.numeric(x)
    paste0(format(round(x_num, 2), big.mark = ",", scientific = FALSE), "M")
  }
  
  fmt_pct <- function(x) {
    x_num <- as.numeric(x)
    paste0(format(round(x_num, 2), nsmall = 2, big.mark = ",", scientific = FALSE), "%")
  }
  
  show_done_modal <- function() {
    showModal(
      modalDialog(
        title = "Complete Investment",
        p("Completing the investment will end this task and return you to the survey. Are you sure? This action cannot be undone."),
        footer = tagList(
          actionButton("cancel_done", "Cancel"),  # changed line
          actionButton("confirm_done", "Yes, complete investment", class = "btn-primary")
        ),
        easyClose = FALSE
      )
    )
  }
  
  reset_order_inputs <- function() {
    order_shares_val("")
    order_review_state("idle")
  }
  
  #---- Server: list outputs ---------------------------------------------------
  
  output$balance_display <- renderText(fmt_dollar(balance))
  output$balance_header_display <- renderText(fmt_dollar(balance))
  
  output$available_funds_display <- renderText({
    fmt_dollar(cash_balance())
  })
  
  output$portfolio_donut <- renderPlotly({
    df_port <- portfolio_summary()
    req(nrow(df_port) > 0)
    
    df_port <- df_port %>%
      mutate(legend_label = paste0(asset, "  ", fmt_dollar(value)))
    
    base_colors <- c(
      "Cash" = "rgba(209,213,219,1)"
    )
    
    stock_palette <- c(
      "rgba(17,24,39,1)",
      "rgba(22,163,74,1)",
      "rgba(37,99,235,1)",
      "rgba(245,158,11,1)",
      "rgba(124,58,237,1)",
      "rgba(220,38,38,1)",
      "rgba(15,118,110,1)"
    )
    
    assets <- df_port$asset
    
    stock_assets <- sort(setdiff(unique(assets), "Cash"))
    
    stock_color_map <- setNames(
      stock_palette[seq_along(stock_assets)],
      stock_assets
    )
    
    color_map <- c(base_colors, stock_color_map)
    
    slice_colors <- unname(color_map[assets])
    
    plot_ly(
      data = df_port,
      labels = ~legend_label,
      values = ~value,
      type = "pie",
      source = "portfolio_donut",
      sort = FALSE,
      direction = "clockwise",
      hole = 0.80,
      textinfo = "none",
      hoverinfo = "none",
      marker = list(
        colors = slice_colors,
        line = list(color = "#FFFFFF", width = 2)
      ),
      pull = 0
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 0.7,
          xanchor = "left",
          y = 0.5,
          yanchor = "middle",
          itemclick = FALSE,
          itemdoubleclick = FALSE
        ),
        margin = list(l = 0, r = 0, t = 25, b = 0),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displayModeBar = FALSE,
        displaylogo = FALSE,
        scrollZoom = FALSE,
        doubleClick = FALSE
      ) %>%
      onRender("
  function(el, x) {
    var originalColors = null;

    function setLegendTextState(activeIndex) {
      var legendTexts = el.querySelectorAll('.legend .traces .legendtext');
      legendTexts.forEach(function(txt, i) {
        if (activeIndex === null || i === activeIndex) {
          txt.style.fill = '#111827';
          txt.style.opacity = 1;
        } else {
          txt.style.fill = '#9CA3AF';
          txt.style.opacity = 1;
        }
      });
    }

    function dimColor(c) {
      if (c.indexOf('rgba') === 0) {
        return c.replace(/rgba\\(([^)]+),[^,]+\\)$/, 'rgba($1,0.22)');
      }
      return c;
    }

    el.on('plotly_hover', function(d) {
      var pt = d.points[0];
      var n = pt.data.labels.length;

      if (!originalColors) {
        originalColors = pt.data.marker.colors.slice();
      }

      var newCols = originalColors.map(function(c, i) {
        if (i === pt.pointNumber) return c;
        return dimColor(c);
      });

      var pull = Array(n).fill(0);
      pull[pt.pointNumber] = 0.02;

      Plotly.restyle(el, {
        'pull': [pull],
        'marker.colors': [newCols],
        'opacity': 1
      }, [0]);

      setLegendTextState(pt.pointNumber);
    });

    el.on('plotly_unhover', function(d) {
      if (!originalColors) return;

      Plotly.restyle(el, {
        'pull': [Array(originalColors.length).fill(0)],
        'marker.colors': [originalColors]
      }, [0]);

      setLegendTextState(null);
    });

    setLegendTextState(null);
  }
")
  })
  
  output$stock_cards <- renderUI({
    df_cards <- ticker_summary() %>%
      filter(ticker %in% shown_tickers()) %>%
      arrange(ticker)
    
    row_list <- lapply(seq_len(nrow(df_cards)), function(i) {
      tk <- df_cards$ticker[i]
      pr <- df_cards$close[i]
      pct <- df_cards$pct_change[i]
      cr <- df_cards$change_color[i]
      
      actionButton(
        inputId = paste0("card_", tk),
        label = div(
          class = "asset-row-inner",
          div(class = "asset-name", tk),
          div(
            class = "asset-price",
            fmt_dollar(pr)
          ),
          div(
            class = "asset-ytd",
            style = paste0("color:", cr, ";"),
            fmt_pct_signed(pct)
          )
        ),
        class = "asset-row-button"
      )
    })
    
    div(class = "asset-list", do.call(tagList, row_list))
  })
  
  output$list_side_box <- renderUI({
    wl <- watchlist_tickers()
    
    wl_df <- watchlist_summary() %>%
      filter(ticker %in% wl) %>%
      arrange(match(ticker, wl))
    
    owned_df <- holdings() %>%
      left_join(
        ticker_summary() %>%
          select(ticker, close, pct_change, change_color),
        by = "ticker"
      ) %>%
      arrange(ticker)
    
    owned_items <- lapply(seq_len(nrow(owned_df)), function(i) {
      tk <- owned_df$ticker[i]
      pr <- owned_df$close[i]
      pct <- owned_df$pct_change[i]
      col <- owned_df$change_color[i]
      sh <- owned_df$shares[i]
      
      actionButton(
        inputId = paste0("holding_", tk),
        label = tagList(
          div(
            class = "watchlist-left",
            div(class = "watchlist-ticker", tk),
            div(
              class = "holding-shares-label",
              paste0(
                sh, " ", ifelse(sh == 1, "share", "shares")
              )
            )
          ),
          div(
            class = "watchlist-chart",
            plotOutput(outputId = paste0("holding_plot_", tk), height = "30px", width = "100%")
          ),
          div(
            class = "watchlist-right",
            div(class = "watchlist-price", fmt_dollar(pr)),
            div(class = "watchlist-pct", style = paste0("color:", col, ";"), fmt_pct_signed(pct))
          )
        ),
        class = "card-button watchlist-item"
      )
    })
    
    watchlist_items <- lapply(seq_len(nrow(wl_df)), function(i) {
      tk <- wl_df$ticker[i]
      pr <- wl_df$close[i]
      pct <- wl_df$pct_change[i]
      col <- wl_df$change_color[i]
      
      actionButton(
        inputId = paste0("watchlist_", tk),
        label = tagList(
          div(
            class = "watchlist-left",
            div(class = "watchlist-ticker", tk)
          ),
          div(
            class = "watchlist-chart",
            plotOutput(outputId = paste0("watchlist_plot_", tk), height = "30px", width = "100%")
          ),
          div(
            class = "watchlist-right",
            div(class = "watchlist-price", fmt_dollar(pr)),
            div(class = "watchlist-pct", style = paste0("color:", col, ";"), fmt_pct_signed(pct))
          )
        ),
        class = "card-button watchlist-item"
      )
    })
    
    div(
      class = "side-box",
      div(class = "side-box-title", "home"),
      if (nrow(owned_df) > 0) div(class = "side-section-title", "Stocks"),
      if (nrow(owned_df) > 0) do.call(tagList, owned_items),
      div(class = "side-section-title", "Watchlist"),
      do.call(tagList, watchlist_items)
    )
  })
  
  #---- Server: detail outputs -------------------------------------------------
  
  output$detail_ticker <- renderText({
    req(selected_ticker())
    selected_ticker()
  })
  
  output$detail_price <- renderText({
    hm <- hovered_metrics()
    fmt_dollar(hm$close)
  })
  
  output$detail_price_header <- renderText({
    latest_hist <- detail_history() %>% slice_tail(n = 1)
    fmt_dollar(latest_hist$close[[1]])
  })
  
  output$detail_header_center <- renderUI({
    req(selected_ticker())
    tagList(
      div(class = "top-bar-value", textOutput("detail_price_header", inline = TRUE)),
      div(class = "top-bar-title", selected_ticker())
    )
  })
  
  output$detail_metrics <- renderUI({
    hm <- hovered_metrics()
    
    period_label <- switch(
      selected_range(),
      "1W"  = "Past week",
      "1M"  = "Past month",
      "3M"  = "Past 3 months",
      "YTD" = "Year to date",
      "1Y"  = "Past year",
      "5Y"  = "Past 5 years",
      "ALL" = "Max time"
    )
    
    div(
      class = "detail-metrics-inline",
      div(
        class = "detail-metric",
        style = paste0("color:", hm$change_color, ";"),
        fmt_dollar_signed(hm$abs_change)
      ),
      div(
        class = "detail-metric",
        style = paste0("color:", hm$change_color, ";"),
        paste0("(", fmt_pct_signed(hm$pct_change), ")")
      ),
      div(
        class = "detail-metric detail-metric-period",
        period_label
      )
    )
  })
  
  output$detail_plot <- renderPlotly({
    dfp <- detail_history()
    ds <- detail_summary()
    req(nrow(ds) == 1)
    
    plot_ly(
      data = dfp,
      x = ~date,
      y = ~close,
      type = "scatter",
      mode = "lines",
      source = "asset",
      hoverinfo = "x",
      line = list(color = ds$change_color[[1]], width = 1.5),
      cliponaxis = FALSE
    ) %>%
      layout(
        showlegend = FALSE,
        hovermode = "x",
        hoverlabel = list(
          bgcolor = "rgba(255,255,255,0)",
          bordercolor = "rgba(255,255,255,0)",
          font = list(color = "black")
        ),
        xaxis = list(
          title = "",
          side = "top",
          fixedrange = TRUE,
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          showspikes = TRUE,
          spikemode = "across",
          spikesnap = "cursor",
          spikecolor = "lightgrey",
          spikethickness = 1,
          spikedash = "solid"
        ),
        yaxis = list(
          title = "",
          fixedrange = TRUE,
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE
        ),
        margin = list(l = 0, r = 0, t = 25, b = 0, pad = 0),
        shapes = list(list(
          type = "line",
          x0 = min(dfp$date), x1 = max(dfp$date),
          y0 = ds$baseline[[1]], y1 = ds$baseline[[1]],
          line = list(color = "lightgrey", dash = "dot", width = 1)
        ))
      ) %>%
      config(
        displayModeBar = FALSE,
        displaylogo = FALSE,
        scrollZoom = FALSE,
        doubleClick = FALSE
      )
  }) %>%
    bindCache(selected_ticker(), selected_range())
  
  output$range_buttons <- renderUI({
    ranges <- c("1W", "1M", "3M", "YTD", "1Y", "5Y", "ALL")
    
    ordered_ranges <- tibble::tibble(
      range = ranges,
      start = as.Date(vapply(
        ranges,
        function(rg) as.character(get_range_start(rg)),
        FUN.VALUE = character(1)
      ))
    ) %>%
      arrange(desc(start)) %>%
      pull(range)
    
    btns <- lapply(ordered_ranges, function(rg) {
      btn_class <- if (selected_range() == rg) {
        "range-button range-button-active"
      } else {
        "range-button"
      }
      
      actionButton(
        inputId = paste0("range_", tolower(rg)),
        label = rg,
        class = btn_class
      )
    })
    
    div(class = "range-button-row", do.call(tagList, btns))
  })
  
  output$detail_about_title <- renderText({
    req(selected_ticker())
    paste("About", selected_ticker())
  })
  
  output$detail_about_company <- renderUI({
    req(selected_ticker())
    
    about_row <- stock_data %>%
      filter(ticker == selected_ticker()) %>%
      slice_tail(n = 1)
    
    req(nrow(about_row) == 1)
    
    sector_value <- about_row$sector[[1]]
    index_value  <- about_row$index[[1]]
    
    div(
      class = "detail-about-wrap",
      div(
        class = "detail-about-row",
        div(class = "detail-about-pill", "10 Most Popular"),
        div(class = "detail-about-pill", "100 Most Popular"),
        div(class = "detail-about-pill", sector_value),
        div(class = "detail-about-pill", index_value)
      )
    )
  })
  
  output$detail_stats_title <- renderText({
    req(selected_ticker())
    paste(selected_ticker(), "Key Statistics")
  })
  
  output$detail_key_stats <- renderUI({
    req(selected_ticker())
    
    df_max <- today
    if (month(df_max) == 2 && day(df_max) == 29) {
      past_year <- df_max + days(1) - years(1)
    } else {
      past_year <- df_max - years(1) + days(1)
    }
    
    df_hist <- stock_data %>%
      filter(ticker == selected_ticker(), date >= past_year)
    
    req(nrow(df_hist) > 0)
    
    latest_row <- df_hist %>% slice_tail(n = 1)
    
    high_today <- latest_row$high[[1]]
    low_today <- latest_row$low[[1]]
    open_price <- latest_row$open[[1]]
    volume_today <- latest_row$volume[[1]] / 1000000
    
    high_52w <- max(df_hist$close, na.rm = TRUE)
    low_52w <- min(df_hist$close, na.rm = TRUE)
    vol_52w <- sd((df_hist$close / df_hist$prior_close - 1) * 100, na.rm = TRUE)
    
    div(
      class = "detail-stats-wrap",
      div(
        class = "detail-stat-row",
        div(
          class = "detail-stat-box",
          div(class = "detail-stat-label", "High today"),
          div(class = "detail-stat-value", fmt_dollar(high_today))
        ),
        div(
          class = "detail-stat-box",
          div(class = "detail-stat-label", "Low today"),
          div(class = "detail-stat-value", fmt_dollar(low_today))
        ),
        div(
          class = "detail-stat-box",
          div(class = "detail-stat-label", "Open price"),
          div(class = "detail-stat-value", fmt_dollar(open_price))
        ),
        div(
          class = "detail-stat-box",
          div(class = "detail-stat-label", "Volume"),
          div(class = "detail-stat-value-small", fmt_int(volume_today))
        )
      ),
      div(
        class = "detail-stat-row",
        div(
          class = "detail-stat-box",
          div(class = "detail-stat-label", "52 Week high"),
          div(class = "detail-stat-value", fmt_dollar(high_52w))
        ),
        div(
          class = "detail-stat-box",
          div(class = "detail-stat-label", "52 Week low"),
          div(class = "detail-stat-value", fmt_dollar(low_52w))
        ),
        div(
          class = "detail-stat-box",
          div(class = "detail-stat-label", "52 Week volatility"),
          div(class = "detail-stat-value-small", fmt_pct(vol_52w))
        ),
        div(class = "detail-stat-box detail-stat-box-empty")
      )
    )
  })
  
  output$detailratings_title <- renderText({
    req(selected_ticker())
    "Analyst ratings"
  })
  
  output$detail_ratings <- renderUI({
    req(selected_ticker())
    
    df_ratings <- ratings_data %>%
      filter(ticker == selected_ticker())
    
    req(nrow(df_ratings) > 0)
    
    rating_col <- "rating"  # change this if your column is named differently
    
    counts <- df_ratings %>%
      mutate(rating_group = case_when(
        .data[[rating_col]] %in% c("Buy", "Strong Buy") ~ "Buy",
        .data[[rating_col]] %in% c("Hold", "Neutral") ~ "Hold",
        .data[[rating_col]] %in% c("Sell", "Strong Sell") ~ "Sell",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(rating_group)) %>%
      count(rating_group, name = "n") %>%
      right_join(
        tibble(rating_group = c("Buy", "Hold", "Sell")),
        by = "rating_group"
      ) %>%
      mutate(n = ifelse(is.na(n), 0L, n))
    
    total_n <- sum(counts$n)
    req(total_n > 0)
    
    counts <- counts %>%
      mutate(
        pct = 100 * n / total_n,
        pct_label = paste0(format(round(pct, 1), nsmall = 1), "%")
      )
    
    buy_pct <- counts %>% filter(rating_group == "Buy") %>% pull(pct)
    buy_pct_big <- paste0(round(buy_pct), "%")
    
    make_bar <- function(label, pct, pct_label) {
      label_class <- if (label == "Buy") {
        "detail-ratings-label detail-ratings-label-buy"
      } else {
        "detail-ratings-label"
      }
      
      fill_class <- if (label == "Buy") {
        "detail-ratings-fill detail-ratings-fill-buy"
      } else {
        "detail-ratings-fill detail-ratings-fill-neutral"
      }
      
      track_class <- if (label == "Buy") {
        "detail-ratings-track detail-ratings-track-buy"
      } else {
        "detail-ratings-track"
      }
      
      value_class <- if (label == "Buy") {
        "detail-ratings-value detail-ratings-value-buy"
      } else {
        "detail-ratings-value"
      }
      
      value_style <- if (pct > 95) {
        paste0("left:", pct, "%; transform: translate(calc(-100% + 2px), -50%); padding-inline: 2px;")
      } else {
        paste0("left:", pct, "%; transform: translate(-2px, -50%); padding-inline: 2px;")
      }
      
      div(
        class = "detail-ratings-bar-row",
        div(class = label_class, label),
        div(
          class = "detail-ratings-track-wrap",
          div(
            class = track_class,
            div(
              class = fill_class,
              style = paste0("width:", pct, "%;")
            ),
            div(
              class = value_class,
              style = value_style,
              pct_label
            )
          )
        )
      )
    }
    
    div(
      class = "detail-ratings-wrap",
      div(
        class = "detail-ratings-row",
        div(
          class = "detail-ratings-summary",
          div(class = "detail-ratings-summary-pct", buy_pct_big),
          div(
            class = "detail-ratings-summary-n",
            paste0("of ", total_n, " ratings")
          )
        ),
        div(
          class = "detail-ratings-bars",
          make_bar(
            "Buy",
            counts %>% filter(rating_group == "Buy") %>% pull(pct),
            counts %>% filter(rating_group == "Buy") %>% pull(pct_label)
          ),
          make_bar(
            "Hold",
            counts %>% filter(rating_group == "Hold") %>% pull(pct),
            counts %>% filter(rating_group == "Hold") %>% pull(pct_label)
          ),
          make_bar(
            "Sell",
            counts %>% filter(rating_group == "Sell") %>% pull(pct),
            counts %>% filter(rating_group == "Sell") %>% pull(pct_label)
          )
        )
      )
    )
  })
  
  output$detail_side_box <- renderUI({
    req(selected_ticker())
    
    latest_hist <- detail_history() %>% slice_tail(n = 1)
    market_price <- latest_hist$close[[1]]
    
    shares_val <- suppressWarnings(as.integer(order_shares_val() %||% ""))
    shares_val <- ifelse(is.na(shares_val), 0, shares_val)
    est_cost <- shares_val * market_price + commission
    
    current_shares <- order_shares_val()
    review_state <- order_review_state()
    locked_state <- review_state %in% c("invalid_amount", "insufficient_funds")
    
    message_block <- NULL
    button_label <- "Review order"
    button_id <- "review_order"
    button_class <- "review-order-btn"
    
    if (review_state == "invalid_amount") {
      message_block <- div(
        class = "order-message-wrap",
        div(class = "order-message-title", "Invalid Amount"),
        div(class = "order-message-text", "Please enter an amount greater than 0.")
      )
      button_label <- "Back"
      button_id <- "order_back"
      button_class <- "review-order-btn-back"
    }
    
    if (review_state == "insufficient_funds") {
      message_block <- div(
        class = "order-message-wrap",
        div(class = "order-message-title", "Not Enough Available Funds"),
        div(
          class = "order-message-text",
          "You do not have enough available funds to place this order."
        )
      )
      button_label <- "Back"
      button_id <- "order_back"
      button_class <- "review-order-btn-back"
    }
    
    div(
      class = "side-box",
      div(class = "side-box-title", paste("Buy", selected_ticker())),
      
      div(
        class = "order-inputs-wrap",
        div(
          class = "order-row",
          div(class = "order-label", "Invest In"),
          div(
            class = if (locked_state) "order-control-lock" else NULL,
            div(
              id = "order_mode",
              pickerInput(
                inputId = "order_mode",
                label = NULL,
                choices = c("Shares" = "Shares"),
                selected = "Shares",
                width = "100%",
                options = pickerOptions(
                  iconBase = "fas",
                  tickIcon = "fa-check",
                  showTick = TRUE
                )
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.order_mode == 'Shares'",
          div(
            class = "order-row",
            div(class = "order-label", "Shares"),
            div(
              class = if (locked_state) "order-control-lock" else NULL,
              div(
                id = "order_shares_wrap",
                tags$input(
                  id = "order_shares",
                  type = "text",
                  value = current_shares,
                  placeholder = "0",
                  class = "form-control",
                  inputmode = "numeric",
                  pattern = "[0-9]*",
                  oninput = "this.value = this.value.replace(/[^0-9]/g, '')",
                  onblur = "if(this.value !== '') this.value = String(Math.round(Number(this.value)));"
                )
              )
            )
          )
        )
      ),
      
      div(
        class = "order-row",
        div(class = "order-label", "Market Price"),
        div(class = "order-value", style = "font-weight:700;", fmt_dollar(market_price))
      ),
      div(
        class = "order-row order-row-divider",
        div(class = "order-label", "Commissions"),
        div(class = "order-value", fmt_dollar(commission))
      ),
      div(
        class = "order-row",
        div(class = "order-label", "Total Cost"),
        div(class = "order-value", style = "font-weight:700;", fmt_dollar(est_cost))
      ),
      
      div(
        class = if (locked_state) "review-order-wrap review-order-wrap-lower" else "review-order-wrap",
        message_block,
        actionButton(
          button_id,
          button_label,
          class = button_class
        )
      ),
      
      div(class = "detail-side-balance", paste0(fmt_dollar(cash_balance()), " available"))
    )
  })
  
  #---- Server: dynamic plots --------------------------------------------------
  
  observe({
    for (tk in shown_tickers()) {
      local({
        tkk <- tk
        output[[paste0("plot_", tkk)]] <- renderPlot({
          mini_chart(tkk)
        }) %>%
          bindCache(tkk, selected_range())
      })
    }
  })
  
  observe({
    for (tk in watchlist_tickers()) {
      local({
        tkk <- tk
        output[[paste0("watchlist_plot_", tkk)]] <- renderPlot({
          watchlist_chart(tkk)
        }, bg = "transparent")
      })
    }
  })
  
  observe({
    hks <- holdings()$ticker
    
    for (tk in hks) {
      local({
        tkk <- tk
        output[[paste0("holding_plot_", tkk)]] <- renderPlot({
          watchlist_chart(tkk)
        }, bg = "transparent")
      })
    }
  })
  
  observe({
    hks <- holdings()$ticker
    
    for (tk in hks) {
      local({
        tkk <- tk
        btn_id <- paste0("holding_", tkk)
        
        observeEvent(input[[btn_id]], ignoreInit = TRUE, {
          log_interaction(btn_id, "Shares owned", page = "asset", asset = tkk)
          session$sendCustomMessage("saveListScroll", list())
          selected_ticker(tkk)
          selected_range(period)
          current_screen("asset")
          session$sendCustomMessage("scrollDetailToTop", list())
        })
      })
    }
  })
  
  #---- Server: list interactions ---------------------------------------------
  
  observe({
    for (tk in shown_tickers()) {
      local({
        tkk <- tk
        btn_id <- paste0("card_", tkk)
        
        observeEvent(input[[btn_id]], ignoreInit = TRUE, {
          log_interaction(btn_id, "Explore asset", page = "asset", asset = tkk)
          session$sendCustomMessage("saveListScroll", list())
          selected_ticker(tkk)
          selected_range(period)
          current_screen("asset")
          session$sendCustomMessage("scrollDetailToTop", list())
        })
      })
    }
  })
  
  observe({
    for (tk in watchlist_tickers()) {
      local({
        tkk <- tk
        btn_id <- paste0("watchlist_", tkk)
        
        observeEvent(input[[btn_id]], ignoreInit = TRUE, {
          log_interaction(btn_id, "Watchlist asset", page = "asset", asset = tkk)
          session$sendCustomMessage("saveListScroll", list())
          selected_ticker(tkk)
          selected_range(period)
          current_screen("asset")
          session$sendCustomMessage("scrollDetailToTop", list())
        })
      })
    }
  })
  
  observeEvent(input$done_home, {
    log_interaction("done_home", "I'm done investing", page = current_screen())
    show_done_modal()
  })
  
  #---- Server: detail interactions -------------------------------------------
  
  observeEvent(input$done_asset, {
    log_interaction("done_asset", "I'm done investing", page = current_screen(), asset = selected_ticker())
    show_done_modal()
  })
  
  observeEvent(input$go_home, {
    log_interaction("go_home", "Home", page = current_screen(), asset = selected_ticker())
    
    if (current_screen() == "asset") {
      reset_order_inputs()
      selected_range(period)
      current_screen("home")
      session$sendCustomMessage("scrollListToTop", list(behavior = "instant"))
    } else {
      session$sendCustomMessage("scrollListToTop", list(behavior = "smooth"))
    }
  })
  
  observeEvent(input$back_to_home, {
    log_interaction("back_to_home", "Back", page = current_screen(), asset = selected_ticker())
    reset_order_inputs()
    selected_range(period)
    current_screen("home")
    session$sendCustomMessage("restoreListScroll", list())
  })
  
  observeEvent(input$range_1w, { log_interaction("range_1w", "Range 1W", page = current_screen(), asset = selected_ticker());  selected_range("1W") })
  observeEvent(input$range_1m, { log_interaction("range_1m", "Range 1M", page = current_screen(), asset = selected_ticker());  selected_range("1M") })
  observeEvent(input$range_3m, { log_interaction("range_3m", "Range 3M", page = current_screen(), asset = selected_ticker());  selected_range("3M") })
  observeEvent(input$range_ytd, { log_interaction("range_ytd", "Range YTD", page = current_screen(), asset = selected_ticker());  selected_range("YTD") })
  observeEvent(input$range_1y, { log_interaction("range_1y", "Range 1Y", page = current_screen(), asset = selected_ticker());  selected_range("1Y") })
  observeEvent(input$range_5y, { log_interaction("range_5y", "Range 5Y", page = current_screen(), asset = selected_ticker());  selected_range("5Y") })
  observeEvent(input$range_all, { log_interaction("range_all", "Range ALL", page = current_screen(), asset = selected_ticker());  selected_range("ALL") })
  
  observeEvent(input$review_order, {
    req(selected_ticker())
    
    latest_hist <- detail_history() %>% slice_tail(n = 1)
    market_price <- latest_hist$close[[1]]
    shares_val <- suppressWarnings(as.integer(order_shares_val() %||% ""))
    shares_val <- ifelse(is.na(shares_val), 0, shares_val)
    est_cost <- shares_val * market_price + commission
    
    log_interaction("review_order", "Review order", page = current_screen(), asset = selected_ticker())
    
    if (shares_val <= 0) {
      order_review_state("invalid_amount")
      return()
    }
    
    if (est_cost > cash_balance()) {
      order_review_state("insufficient_funds")
      return()
    }
    
    order_review_state("idle")
    
    showModal(
      modalDialog(
        div(
          class = "review-modal",
          div(
            class = "modal-body",
            actionButton(
              "close_review_modal",
              label = NULL,
              icon = icon("xmark"),
              class = "review-modal-close"
            ),
            div(class = "review-modal-title", paste("Buy", selected_ticker())),
            div(class = "review-modal-balance", paste(fmt_dollar(cash_balance()), "available")),
            
            div(
              class = "review-modal-row review-modal-divider",
              div(class = "review-modal-label", "Number of Shares"),
              div(class = "review-modal-value", shares_val)
            ),
            div(
              class = "review-modal-row review-modal-divider",
              div(class = "review-modal-label", "Market Price"),
              div(class = "review-modal-value", fmt_dollar(market_price))
            ),
            div(
              class = "review-modal-row",
              div(class = "review-modal-label", "Total Cost"),
              div(class = "review-modal-value", fmt_dollar(est_cost))
            ),
            
            div(class = "review-modal-subheading", "Order Summary"),
            div(
              class = "review-modal-text",
              paste0(
                "You are placing a market order to buy ",
                shares_val,
                " ",
                ifelse(shares_val == 1, "share", "shares"),
                " of ",
                selected_ticker(),
                ". Your order will be executed at the current market price. ",
                "Are you sure? Once submitted, this choice cannot be changed."
              )
            ),
            
            actionButton(
              "submit_order",
              "Submit",
              class = "review-submit-btn"
            )
          )
        ),
        title = NULL,
        footer = NULL,
        easyClose = FALSE,
        fade = TRUE
      )
    )
  })
  
  observeEvent(input$close_review_modal, {
    val <- input$order_shares %||% ""
    
    log_interaction("close_review_modal",
                    "Close review modal",
                    page = current_screen(),
                    asset = selected_ticker(),
                    shares = suppressWarnings(as.numeric(val))
    )
    removeModal()
  })
  
  observeEvent(input$submit_order, {
    req(selected_ticker())
    
    latest_hist <- detail_history() %>% slice_tail(n = 1)
    market_price <- latest_hist$close[[1]]
    shares_val <- suppressWarnings(as.numeric(order_shares_val() %||% 0))
    shares_val <- ifelse(is.na(shares_val), 0, shares_val)
    
    req(shares_val > 0)
    
    est_cost <- shares_val * market_price + commission
    req(cash_balance() >= est_cost)
    
    # Log the trade as an interaction
    log_interaction(
      "submit_order",
      "Submit order",
      page = current_screen(),
      asset = selected_ticker(),
      shares = shares_val,
      unit_price = market_price,
      total_cost = est_cost
    )
    
    # --- existing holdings update and navigation ---
    current_holdings <- holdings()
    tk <- selected_ticker()
    
    if (tk %in% current_holdings$ticker) {
      current_holdings <- current_holdings %>%
        mutate(
          shares = if_else(ticker == tk, shares + shares_val, shares)
        )
    } else {
      current_holdings <- bind_rows(
        current_holdings,
        tibble(ticker = tk, shares = shares_val)
      )
    }
    
    holdings(current_holdings)
    cash_balance(cash_balance() - est_cost)
    
    reset_order_inputs()
    removeModal()
    selected_range(period)
    current_screen("home")
    session$sendCustomMessage("restoreListScroll", list())
  })
  
  observeEvent(input$order_shares, {
    val <- input$order_shares %||% ""
    order_shares_val(val)
    
    if (!is.null(val) && nchar(trimws(val)) > 0) {
      log_interaction("order_shares_change",
                      "Change order shares",
                      page = current_screen(),
                      asset = selected_ticker(),
                      shares = suppressWarnings(as.numeric(val)))
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$order_back, {
    # Determine which error they are backing out from
    error_type <- order_review_state()  # "invalid_amount" or "insufficient_funds"
    
    log_interaction("order_back", paste("Back from error:", error_type), page = current_screen(), asset = selected_ticker())
    order_review_state("idle")
  }, ignoreInit = TRUE)
  
  observe({
    shinyjs::toggle("screen_home", condition = current_screen() == "home")
    shinyjs::toggle("screen_asset", condition = current_screen() == "asset")
  })
  
  observe({
    locked <- order_review_state() %in% c("invalid_amount", "insufficient_funds")
    
    if (locked) {
      shinyjs::disable("order_shares")
      shinyjs::runjs("$('#order_mode').find('select').prop('disabled', true);")
      shinyjs::runjs("$('#order_mode').find('select').selectpicker('refresh');")
    } else {
      shinyjs::enable("order_shares")
      shinyjs::runjs("$('#order_mode').find('select').prop('disabled', false);")
      shinyjs::runjs("$('#order_mode').find('select').selectpicker('refresh');")
    }
  })
  
  #---- Server: app navigation -------------------------------------------------
  
  observeEvent(input$cancel_done, {
    log_interaction("cancel_done",
                    "Cancel",
                    page = current_screen(),
                    asset = if (current_screen() == "asset") selected_ticker() else NA_character_
    )
    removeModal()
  })
  
  observeEvent(input$confirm_done, {
    log_interaction("confirm_done",
                    "Yes, complete investment",
                    page = current_screen(),
                    asset = if (current_screen() == "asset") selected_ticker() else NA_character_
    )
    
    # Append interactions if any
    if (nrow(interactions()) > 0) {
      tryCatch(
        sheet_append(interactions_sheet, interactions()),
        error = function(e) {
          # Optionally log to console / file for debugging
          message("Failed to append to Google Sheet: ", conditionMessage(e))
        }
      )
    }
    
    # Build file names (include IDs and timestamp to avoid collisions)
    id_suffix <- paste(
      prolific_pid()  %||% "noPID",
      study_id()      %||% "noSTUDY",
      session_id()    %||% "noSESSION",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      sep = "_"
    )
    
    interactions_file <- file.path(logsDir, paste0("interactions_", id_suffix, ".csv"))
    
    # Write logs to disk
    if (nrow(interactions()) > 0) {
      readr::write_csv(interactions(), interactions_file)
    }
    
    base_url <- next_survey
    
    if (live_mode()) {
      target_url <- paste0(
        base_url,
        "?PROLIFIC_PID=", prolific_pid(),
        "&STUDY_ID=", study_id(),
        "&SESSION_ID=", session_id()
      )
    } else {
      # direct login: either no IDs or test IDs
      target_url <- base_url
    }
    
    shinyjs::runjs(sprintf("window.location.href = '%s';", target_url))
    stopApp()
  })
  
  #---- Server: exit -----------------------------------------------------------
  
  session$onSessionEnded(function() {
    log_interaction("session_ended",
                    "Session ended (no confirm)",
                    page = current_screen())
    stopApp()
  })
}

shinyApp(ui, server)