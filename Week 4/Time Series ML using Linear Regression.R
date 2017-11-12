# Time series forecasting using Linear regression model

library(tidyquant)
library(timetk)

# Get sales data from FRED
# Beer, Wine, Distilled Alcoholic Beverages, in Millions USD
beer_sales_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2010-01-01", to = "2016-12-31")

beer_sales_tbl

# Plot Beer Sales and a 12 month moving average
beer_sales_tbl %>%
    ggplot(aes(date, price)) +
    geom_line(col = palette_light()[1]) +
    geom_point(col = palette_light()[1]) +
    geom_ma(ma_fun = SMA, n = 12, size = 1) +
    theme_tq() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = "Beer Sales: 2007 through 2016")

# Get index of the data and summarize the data - Useful for checking periodicity and missing obs

# tk_index() gives the index of the data
# tk_get_timeseries_summary() gives the summary of the time series data
beer_sales_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    glimpse()

# For machine learning in timeseries, the features which are used to build a model 
# are obtained from the timestamp signature
# 
# the function tk_augument_timeseries_signature() 
# expands the timestamp information into columns which are used as feature

beer_sales_tbl_aug <- beer_sales_tbl %>%
    tk_augment_timeseries_signature()

beer_sales_tbl_aug

## FITTING A MODEL
# 
# We can fit any machine learning model to this data, but here we fit the
# linear regression lm() model for demo purposes

fit_lm <- lm(price ~ ., data = select(beer_sales_tbl_aug, -c(date, diff)))

summary(fit_lm)

## FORECASTING the future

# Start by building the future time series
# use tk_make_future_timeseries() to get future dates
beer_sales_idx <- beer_sales_tbl %>%
    tk_index()

future_idx <- beer_sales_tbl %>%
    tk_index() %>%
    tk_make_future_timeseries(n_future = 12)

future_idx

# Create the data set by breaking down the timestamp signature
new_data_tbl <- future_idx %>%
    tk_get_timeseries_signature()

new_data_tbl

# Predict
pred <- predict(fit_lm, newdata = select(new_data_tbl, -c(index, diff)))
pred

predictions_tbl <- tibble(
    date  = future_idx,
    value = pred
)

predictions_tbl


# Compare Actuals vs Predictions

# Get actual data again from FRED
actuals_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2017-01-01", to = "2017-12-31")

# Plot Beer Sales Forecast
beer_sales_tbl %>%
    ggplot(aes(x = date, y = price)) +
    # Training data
    geom_line(color = palette_light()[[1]]) +
    geom_point(color = palette_light()[[1]]) +
    # Predictions
    geom_line(aes(y = value), color = palette_light()[[2]], data = predictions_tbl) +
    geom_point(aes(y = value), color = palette_light()[[2]], data = predictions_tbl) +
    # Actuals
    geom_line(color = palette_light()[[1]], data = actuals_tbl) +
    geom_point(color = palette_light()[[1]], data = actuals_tbl) +
    # Aesthetics
    theme_tq() +
    labs(title = "Beer Sales Forecast: Time Series Machine Learning",
         subtitle = "Using basic multivariate linear regression")



# Error estimation
error_tbl <- left_join(actuals_tbl, predictions_tbl) %>%
    rename(actual = price, pred = value) %>%
    mutate(
        error     = actual - pred,
        error_pct = error / actual
    ) 
error_tbl

# Calculating test error metrics
test_residuals <- error_tbl$error
test_error_pct <- error_tbl$error_pct * 100 # Percentage error

me   <- mean(test_residuals, na.rm=TRUE)
rmse <- mean(test_residuals^2, na.rm=TRUE)^0.5
mae  <- mean(abs(test_residuals), na.rm=TRUE)
mape <- mean(abs(test_error_pct), na.rm=TRUE)
mpe  <- mean(test_error_pct, na.rm=TRUE)

tibble(me, rmse, mae, mape, mpe) %>% glimpse()

# For more check this out
# https://business-science.github.io/timetk/articles/TK03_Forecasting_Using_Time_Series_Signature.html
