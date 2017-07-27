library(forecast)
library(tidyquant)
library(timekit)
library(sweep)

# Get alcohol data using tidyquant
alcohol_sales_tbl <- tq_get("S4248SM144NCEN", 
                            get  = "economic.data", 
                            from = "2007-01-01",
                            to   = "2016-12-31")
alcohol_sales_tbl

# Visualize
alcohol_sales_tbl %>%
    ggplot(aes(x = date, y = price)) +
    geom_line(size = 1, color = palette_light()[[1]]) +
    geom_smooth(method = "loess") +
    labs(title = "US Alcohol Sales: Monthly", x = "", y = "Millions") +
    scale_y_continuous(labels = scales::dollar) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_tq()


##FORECASTINB WORKFLOW
    #' The forecasting workflow involves a few basic steps:
    #' 
    #' Step 1: Coerce to a ts object class.
    #' Step 2: Apply a model (or set of models)
    #' Step 3: Forecast the models (similar to predict)
    #' Step 4: Use sw_sweep() to tidy the forecast.
    #' 

# STEP 1: Coerce to a ts object claa
#               We use tk_ts from timekit to do this

alcohol_sales_ts <- tk_ts(alcohol_sales_tbl, start = 2007, freq = 12, silent = TRUE)
alcohol_sales_ts

has_timekit_idx(alcohol_sales_ts)


# STEP 2: Model fitting
#       We fit an ets() model from forecast package
#
#       exponential timeseries smoothening, error, trend & seasonal model

fit_ets <- alcohol_sales_ts %>%
    ets()

# Check model parameters
sw_tidy(fit_ets)

# Check model performance quality parameters
sw_glance(fit_ets)

# Check actual, fitted and residuals
sw_augment(fit_ets)

# Plot the fit and residuals
augment_fit_ets <- sw_augment(fit_ets)%>%
    ggplot(aes(x = index, y = .resid)) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_smooth(method = "loess") +
    scale_x_yearmon(n = 10) +
    labs(title = "US Alcohol Sales: ETS Residuals", x = "") + 
    theme_tq()

augment_fit_ets

# Decompose the series
decomp_fit_ets <- sw_tidy_decomp(fit_ets)
decomp_fit_ets 

# Plot it
decomp_fit_ets %>%
    gather(key = key, value = value, -index) %>%
    mutate(key = forcats::as_factor(key)) %>%
    ggplot(aes(x = index, y = value, group = key)) +
    geom_line(color = palette_light()[[2]]) +
    geom_ma(ma_fun = SMA, n = 12, size = 1) +
    facet_wrap(~ key, scales = "free_y") +
    scale_x_yearmon(n = 10) +
    labs(title = "US Alcohol Sales: ETS Decomposition", x = "") + 
    theme_tq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# STEP3: Forecast

fcast_ets <- fit_ets %>%
    forecast(h = 12)

# STEP4: Tidy and plot the Forecast

sw_sweep(fcast_ets, fitted = TRUE)

# This returns a long format tibble with "actual","fitted" and "forecast" in key column


# timekit_ids=TRUE makes the date column appear as it was in original data
sw_sweep(fcast_ets, timekit_idx = TRUE) %>%
    head()
sw_sweep(fcast_ets, timekit_idx = TRUE) %>%
    tail()

# We can plot it as follows
sw_sweep(fcast_ets, timekit_idx = TRUE, fitted = TRUE) %>%
    ggplot(aes(x = index, y = price, color = key)) +
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
                fill = "#D5DBFF", color = NA, size = 0) +
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
                fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
    geom_line(size = 1) +
    labs(title = "US Alcohol Sales, ETS Model Forecast", x = "", y = "Millions", 
         subtitle = "Irregular Time Index") +
    scale_y_continuous(labels = scales::dollar) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_color_tq() +
    scale_fill_tq() +
    theme_tq() 
