library("readxl")
library("itsmr")
library("tseries")
library("forecast")

# Data taken from https://www.investing.com/currencies/eur-chf-historical-data

# CHANGE ALL = TO <-

# -------------------------------------------------------------------------------------
# GET DATA
# -------------------------------------------------------------------------------------

setwd("C:/Users/Aritz/Desktop/1.2/Time Series Analysis/Project/Euro swiss/")
dataset_name = "EUR_CHF Historical Data.csv"
data = read.csv(dataset_name, header = T)

first_year = 1976 # decide when to start (if I start from 1976, the log might make a difference in differencing method)
# After a decade of preparations, the euro was launched on 1 January 1999: for the first three years it was an 'invisible' currency, only used for accounting purposes and electronic payments. Coins and banknotes were launched on 1 January 2002, and in 12 EU countries the biggest cash changeover in history took place.
if (first_year==1976){
  selected_data = data$Price
}else{
  selected_data = data$Price[1:(length(data$Price)-(first_year-1976)*12)]
}

series = ts(rev(selected_data), start=first_year, freq=12)
t <- 1:length(series)
par(mfrow=c(1,1))
ts.plot(series, lwd=2, main='EUR/CHF', ylab='Rate', xlab='')

# # Save multiple objects in .RData format
# save(data, series, file = "data.RData")
# # To load the data again
# load("data.RData")

# -------------------------------------------------------------------------------------
# REMOVE TREND & SEASONALITY
# -------------------------------------------------------------------------------------
# I will first work with 1976

# Visually: NO seasonality, YES trend

# Since it behaves more like a random walk, it is better to use differencing, instead of linear regression
# But we will test multiple ways to remove trend: one big linear regression, and differencing (of the log)
# Additionally, we will test removing the 2015 outlier (for the differencing removed)
# So 3 residuals in total

# One big linear model
t <- 1:length(series)
t2 = t^2 # linear + quadratic
fit = lm(series~t+t2)
fit_res <- fit$residuals
fit_res = ts(fit_res,start=first_year,frequency=12)
# Plot of regression
par(mfrow=c(2,2))
ts.plot(series, lwd=2, main='EUR/CHF', ylab='Rate', xlab='')
lines(ts(fit$fitted.values,start=first_year,frequency=12), col='blue',lwd=2)
# Plot
ts.plot(fit_res, main='Trend removed by: linear model ', ylab='Residuals', xlab='')
par(mfrow=c(2,1))

# # differencing
diff_res_no_log = diff(series)
# plotc(diff_res)

# differencing of the log
diff_res = diff(log(series))

# Removing the outlier from January 2015 (only for the differencing one)
diff_res_out = diff_res
diff_res_no_log_out = diff_res_no_log
if (first_year < 2016){
  window(diff_res_out, 2015.0, 2015.0) = 0
  window(diff_res_no_log_out, 2015.0, 2015.0) = 0
}
# Center the differencing residuals because we fit a 0 mean ARMA(p,q) model
mu = mean(diff_res)
diff_res = diff_res - mu
mu_out = mean(diff_res_out)
diff_res_out = diff_res_out - mu_out

# Plot
ts.plot(diff_res, main='Trend removed by: differencing', ylab='Residuals', xlab='')
ts.plot(diff_res_out, main='Trend removed by: differencing (outlier removed)', ylab='Residuals', xlab='')

# Why did we use Differencing of log?
# To get a more constant variance
par(mfrow=c(2,1))
ts.plot(diff_res_no_log_out, main='Trend removed by: differencing of the original series', ylab='Residuals', xlab='')
ts.plot(diff_res_out, main='Trend removed by: differencing of \n the log transformation of the series', ylab='Residuals', xlab='')

# -------------------------------------------------------------------------------------
# WORK WITH THE RESIDUALS
# -------------------------------------------------------------------------------------

par(mfrow=c(3,2))
# ACF & PACF
acf(fit_res, main='Linear model'); pacf(fit_res, main='Linear model')
acf(diff_res, main='Differencing'); pacf(diff_res, main='Differencing')
acf(diff_res_out, main='Differencing (outlier removed)'); pacf(diff_res_out, main='Differencing (outlier removed)')


# Also, we should test if our data can now be considered as a white-noise
Box.test(x = fit_res, type = "Ljung-Box") 
Box.test(x = diff_res, type = "Ljung-Box") 
Box.test(x = diff_res_out, type = "Ljung-Box") 

# p-value < 0.05 for linear model. We can reject the hypothesis H0 that this is a white-noise.
# p-value > 0.05 for diff. We cannot reject H0 that this is a white-noise. It is also visible that there are no significant spikes (at most 1 spike in the PACF)
# For Differencing it is almost a white noise, so ARMA will probably have little p,q and small values for phi,theta

# More about stationarity (EXTRA)
# Augmented Dickey–Fuller (ADF) t-statistic test for unit root. 
# A series with a trend line will have a unit root and result in a large p-value
# adf.test(fit_res); adf.test(diff_res); adf.test(diff_res_out)
# We have small p value for all except for fit_res (we expected it to be non-stationary)
# Check also Kwiatkowski-Phillips-Schmidt-Shin (KPSS) for level or trend stationarity
# kpss.test(fit_res, null="Trend")

# Expected ARMA(p,q) models
# fit_res ARMA(1,0) with phi close to 1
# diff_res_center maybe ARMA(1,0)
# diff_res_out_center maybe ARMA(1,0)

# -------------------------------------------------------------------------------------
# FIT MODELS
# -------------------------------------------------------------------------------------

# I recommend to install the R package 'forecast'  and to use the auto.arima(...) function
# Fit model (is it as expected?) and check diagnostics plots (valid model?)
fit_model = auto.arima(fit_res, seasonal=FALSE)
# Model: ARIMA(2,1,2)(2,0,0)_12 with seasonal part, but no significant spikes found at lag 12. Better restrict seasonal parameters
tsdiag(fit_model) # residuals look like white noise
diff_model = auto.arima(diff_res, seasonal=FALSE)
tsdiag(diff_model) # residuals look like white noise
diff_out_model = auto.arima(diff_res_out, seasonal=FALSE) # Also note that if you take the original white noise series and form a (1,0,1) model there are an infinite # of solutions that can arise . each of which will have two coefficients of the same value but with different signs. That's what's happening here
tsdiag(diff_out_model) # residuals look like white noise
# We will see how the predictions are virtually 0

# cat('Linear Model model from', first_year, ':', names(fit_model$coef),'\n')
# cat('Diff model from', first_year, ':', names(diff_model$coef),'\n')
# cat('Diff (without outlier) Model model from', first_year, ':', names(diff_out_model$coef),'\n')

# Regarding the diagnostics plots:
# The ACF of the residuals is OK ! Only one bar (out of 20) which goes slightly outside the bound +-1.96/sqrt(n)
# In addition to the ACF; we can see all Ljung-box tests for all possible lags.
# The p-value is larger than 5% in each case, meaning that we cannot reject the Hypothesis H0 that these residuals are a white-noise. 
# So that's good. These residuals seem to be indeed a white-noise (good ACF; large pvalues on Ljungbox tests)

# Causality
phi <- fit_model$model$phi
abs( polyroot(c(1,-phi)) ) 
# OK if all the roots are outside the unit circle. Yes, causal.

# -------------------------------------------------------------------------------------
# STATIONARY PREDICTIONS
# -------------------------------------------------------------------------------------

# Since the model is 'valid' now, we can use it for forecasting:
h <- 12 * 1 # one year
n = length(t)
fit_pred <- predict(object=fit_model, n.ahead = h)
diff_pred <- predict(object=diff_model, n.ahead = h)
diff_out_pred <- predict(object=diff_out_model, n.ahead = h)
first_year_plot = max(first_year, 2018)

# Let us plot the forecasts
par(mfrow=c(1,1))
# Series
ts.plot(fit_res, xlim=c(first_year_plot,first_year+(n+h)/12), ylim=c(-0.35,0.15), lwd=2, main='Linear model', ylab='Residuals', xlab='')
# Fitted
lines(x=seq(first_year-1/12,first_year+(n-2)/12, 1/12), fitted(fit_model), col='blue', lwd=2, lty=2)
# Predictions (plotted after bounds)
pred_time = c(2022.250,seq(first_year+n/12,first_year+(n-1)/12+h/12,1/12))
# Upper and lower bounds (95% CI depending on normality of residuals)
fit_pred_up = fit_pred$pred + 1.96 * fit_pred$se
fit_pred_down = fit_pred$pred - 1.96 * fit_pred$se
fit_bound = rbind(cbind(pred_time[-1],fit_pred_up), cbind(rev(pred_time[-1]),rev(fit_pred_down)))
polygon(fit_bound, col="grey80", border = NA)
# Predictions (above bounds)
lines(x=pred_time, y = c(fit_res[n],fit_pred$pred), col="red" , lwd=2)
# warning: These itervals are 95% conf. intervals ONLY if the residuals are normally distributed.
# Let's check here with a normal qq-plot
# qqnorm(y = fit_model$residuals); qqline(y = fit_model$residuals)
# Not normal, so taking +/- 1.96 prediction_standard deviation would a priori not yield a 95% confidence interval for the prediction.
# Normality of the residuals should also be checked with a Shapiro-Wilk test.
p = shapiro.test(fit_model$residuals)$p.value
# The test do rejects (p-value < 0.05) the hypothesis H0 that these residuals are normally distributed.
# So based on that; the intervals computed previously cannot be considered as 95% confidence intervals.
if (p<0.05){
  normality_fit = FALSE
} else {
  normality_fit = TRUE
}
# Legend
legend("topleft", legend = c("Fitted", "Predicted", paste0("Upper/Lower bounds (", normality_fit," 95% CI)")), lty=c(2,1,1), col=c("blue", "red", "grey80"), lwd=c(2,2,5))


# Same for the other two models
# Series
ts.plot(diff_res, xlim=c(first_year_plot,first_year+(n+h)/12), ylim=c(-0.05,0.05), lwd=2, main='Differencing', ylab='Residuals', xlab='')
# Fitted
lines(fitted(diff_model), col='blue', lwd=2, lty=2) # fitted are basically 0
# Upper and lower bounds (95% CI depending on normality of residuals)
diff_pred_up = diff_pred$pred + 1.96 * diff_pred$se
diff_pred_down = diff_pred$pred - 1.96 * diff_pred$se
diff_bound = rbind(cbind(pred_time[-1],diff_pred_up), cbind(rev(pred_time[-1]),rev(diff_pred_down)))
polygon(diff_bound, col="grey80", border = NA)
# Predictions (above bounds)
lines(x=pred_time, y = c(diff_res[n-1], diff_pred$pred) , col="red" , lwd=2)
# 95% CI? Check normality
# qqnorm(y = fit_model$residuals); qqline(y = fit_model$residuals)
# Not normal, so taking +/- 1.96 prediction_standard deviation would a priori not yield a 95% confidence interval for the prediction.
p = shapiro.test(diff_model$residuals)$p.value
if (p<0.05){
  normality_diff = FALSE
} else {
  normality_diff = TRUE
}
# Legend
legend("topleft", legend = c("Fitted", "Predicted", paste0("Upper/Lower bounds (", normality_diff," 95% CI)")), lty=c(2,1,1), col=c("blue", "red", "grey80"), lwd=c(2,2,5))

# Series
ts.plot(diff_res, xlim=c(first_year_plot,first_year+(n+h)/12), ylim=c(-0.05,0.05), lwd=2, main='Differencing (outlier removed) residuals', ylab='Residuals', xlab='')
# Fitted
lines(fitted(diff_out_model), col='blue', lwd=2, lty=2) # fitted are basically 0
# Upper and lower bounds (95% CI depending on normality of residuals)
diff_pred_out_up = diff_out_pred$pred + 1.96 * diff_out_pred$se
diff_pred_out_down = diff_out_pred$pred - 1.96 * diff_pred$se
diff_bound = rbind(cbind(pred_time[-1],diff_pred_out_up), cbind(rev(pred_time[-1]),rev(diff_pred_out_down)))
polygon(diff_bound, col="grey80", border = NA)
# Predictions (above bounds)
lines(x=pred_time, y = c(diff_res_out[n-1], diff_out_pred$pred), col="red" , lwd=2)
# 95% CI? Check normality
# qqnorm(y = fit_model$residuals); qqline(y = fit_model$residuals)
# Not normal, so taking +/- 1.96 prediction_standard deviation would a priori not yield a 95% confidence interval for the prediction.
p = shapiro.test(diff_out_model$residuals)$p.value
if (p<0.05){
  normality_diff_out = FALSE
} else {
  normality_diff_out = TRUE
}
# Legend
legend("topleft", legend = c("Fitted", "Predicted", paste0("Upper/Lower bounds (", normality_diff_out," 95% CI)")), lty=c(2,1,1), col=c("blue", "red", "grey80"), lwd=c(2,2,5))

# -------------------------------------------------------------------------------------
# INITIAL PREDICTIONS
# -------------------------------------------------------------------------------------

# Now that the stationary time series is predicted; we need to obtain predictions for the initial time series.
# This requires to see how the trend was removed.

# Series
ts.plot(series, xlim=c(first_year_plot,first_year+(n+h)/12), ylim=c(0.8,1.25), lwd=2, main='EUR/CHF predictions', ylab='Rate', xlab='')

# Linear model
# Trend
t_predict = (n+1):(n+h)
trend = fit$coefficients[1] + t_predict * fit$coefficients[2] + t_predict^2 * fit$coefficients[3]
# Our final prediction is equal to the prediction of the stationary time series + 'trend' (plotted after all bounds) 
fit_final = trend+fit_pred$pred
# Upper and lower bounds (95% CI depending on normality of residuals)
fit_pred_up_final = fit_final + 1.96 * fit_pred$se
fit_pred_down_final = fit_final - 1.96 * fit_pred$se
fit_bound_final = rbind(cbind(pred_time[-1],fit_pred_up_final), cbind(rev(pred_time[-1]),rev(fit_pred_down_final)))

# Make transparent colors for bounds
## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

transparent_red <- t_col("pink", perc = 5, name = "lt.pink")
polygon(fit_bound_final, col=transparent_red, border = NA)

# Diff
diff_pred_mu = diff_pred$pred+mu  
diff_pred_cum = cumsum(diff_pred_mu) 
diff_final = series[n]+diff_pred_cum
# Upper and lower bounds (95% CI depending on normality of residuals)
diff_pred_up_final = diff_final + 1.96 * diff_pred$se
diff_pred_down_final = diff_final - 1.96 * diff_pred$se
diff_bound_final = rbind(cbind(pred_time[-1],diff_pred_up_final), cbind(rev(pred_time[-1]),rev(diff_pred_down_final)))
transparent_blue <- t_col("blue", perc = 60, name = "lt.blue")
polygon(diff_bound_final, col=transparent_blue, border = NA)

# Diff (outlier removed)
diff_out_pred_mu = diff_out_pred$pred+mu_out 
diff_out_pred_cum = cumsum(diff_out_pred_mu) 
diff_out_final = series[n]+diff_out_pred_cum
# Upper and lower bounds (95% CI depending on normality of residuals)
diff_out_pred_up_final = diff_out_final + 1.96 * diff_out_pred$se
diff_out_pred_down_final = diff_out_final - 1.96 * diff_out_pred$se
diff_out_bound_final = rbind(cbind(pred_time[-1],diff_out_pred_up_final), cbind(rev(pred_time[-1]),rev(diff_out_pred_down_final)))
transparent_green <- t_col("green", perc = 60, name = "lt.green")
polygon(diff_out_bound_final, col=transparent_green, border = NA)

# Lines
lines(x = pred_time, y = c(series[n],fit_final) , col = 'red' , lwd = 2)
lines(x = pred_time, y = c(series[n],diff_final) , col = 'blue' , lwd = 2)
lines(x = pred_time, y = c(series[n],diff_out_final) , col = "darkgreen" , lwd = 2)

# Legend
legend("bottomleft", title=as.expression(bquote(bold("Trend removal method"))), legend = c("Linear model", paste0("Linear model bounds (", normality_fit," 95% CI)"), "Differencing", paste0("Differencing bounds (", normality_diff," 95% CI)"), "Differencing (outlier removed)", paste0("Differencing (outlier removed) bounds (", normality_diff_out," 95% CI)")), lty=c(1,1,1,1,1,1), lwd=c(2,5,2,5,2,5), col=c("red", transparent_red, "blue", transparent_blue, "darkgreen", transparent_green))

# Great differences between linear model and diff

# -------------------------------------------------------------------------------------
# FOR DISCUSION: TESTING DIFFERENT STARTING YEARS
# -------------------------------------------------------------------------------------

# Function to add prediction line and bounds for a determined year
draw_predictions = function(first_year, color, percentage, bound_color, method){
  # Get series
  if (first_year==1976){
    selected_data = data$Price
  }else{
    selected_data = data$Price[1:(length(data$Price)-(first_year-1976)*12)]
  }
  series = ts(rev(selected_data), start=first_year, freq=12)
  n = length(series)
  # Remove trend
  if (method=='lm'){
    # Linear model
    t <- 1:length(series)
    t2 = t^2 # linear + quadratic
    fit = lm(series~t+t2)
    fit_res <- fit$residuals
    fit_res = ts(fit_res,start=first_year,frequency=12)
  } else if(method=='diff'){
    # Differencing of the log
    diff_res = diff(log(series))
    # Removing the outlier from January 2015
    diff_res_out = diff_res
    if (first_year < 2016){
      window(diff_res_out, 2015.0, 2015.0) = 0
    }
    # Center
    mu_out = mean(diff_res_out)
    diff_res_out = diff_res_out - mu_out
  }
  # Check if we can work with residuals
  # ACF, PACF, LJUNG-BOX TEST
  # Fit models
  fit_model = auto.arima(fit_res, seasonal = FALSE) #; tsdiag(fit_model)
  diff_out_model = auto.arima(diff_res_out, seasonal = FALSE)# ; tsdiag(diff_out_model)
  # Are models valid?
  # Stationary predictions
  fit_pred <- predict(object=fit_model, n.ahead = h)
  diff_out_pred <- predict(object=diff_out_model, n.ahead = h)
  # Normality assumption
  p = shapiro.test(fit_model$residuals)$p.value
  if (p<0.05){
    normality[i] = FALSE
  } else {
    normality[i] = TRUE
  }
  p = shapiro.test(diff_out_model$residuals)$p.value
  if (p<0.05){
    normality[i] = FALSE
  } else {
    normality[i] = TRUE
  }
  # Time series prediction
  if (method=='lm'){
    t_predict = (n+1):(n+h)
    trend = fit$coefficients[1] + t_predict * fit$coefficients[2] + t_predict^2 * fit$coefficients[3]
    fit_final = trend+fit_pred$pred
    fit_pred_up_final = fit_final + 1.96 * fit_pred$se
    fit_pred_down_final = fit_final - 1.96 * fit_pred$se
    fit_bound_final = rbind(cbind(pred_time[-1],fit_pred_up_final), cbind(rev(pred_time[-1]),rev(fit_pred_down_final)))
    transparent <- t_col(bound_color, perc = percentage, name = "lt.pink")
    polygon(fit_bound_final, col=transparent, border = NA)
  } else if (method=='diff'){
    diff_out_pred_mu = diff_out_pred$pred+mu_out
    diff_out_pred_cum = cumsum(diff_out_pred_mu)
    diff_out_final = series[n]+diff_out_pred_cum
    diff_out_pred_up_final = diff_out_final + 1.96 * diff_out_pred$se
    diff_out_pred_down_final = diff_out_final - 1.96 * diff_out_pred$se
    diff_out_bound_final = rbind(cbind(pred_time[-1],diff_out_pred_up_final), cbind(rev(pred_time[-1]),rev(diff_out_pred_down_final)))
    transparent <- t_col(bound_color, perc = percentage, name = "lt.green")
    polygon(diff_out_bound_final, col=transparent, border = NA)
  }
  # Predictions
  if (method=='lm'){
    lines(x = pred_time, y = c(series[n],fit_final) , col = color , lwd = 2)
  } else if(method=='diff'){
    lines(x = pred_time, y = c(series[n],diff_out_final) , col = color , lwd = 2)
  }
  
  if (color=='darkgreen'){ # add legend after the last one
    legend("bottomleft", title=as.expression(bquote(bold("Series starting year"))), legend = c("1976", paste0("1976 bounds (", normality[1]," 95% CI)"), "2002", paste0("2002 bounds (", normality[2]," 95% CI)"), "2018", paste0("2018 bounds (", normality[3]," 95% CI)")), lty=c(1,1,1,1,1,1), lwd=c(2,5,2,5,2,5), col=c("red", transparent_red, "blue", transparent_blue, "darkgreen", transparent_green))
  }
}

# Differencing might be the best
# We test different starting years and see how the models and predictions change

years = c(1976, 2002, 2018)
colors = c('red', 'blue', 'darkgreen')
bound_colors = c('pink', 'blue', 'green')
percentages = c(5,60,60)
normality = logical(length(years))

# Main plot 
ts.plot(series, xlim=c(first_year_plot,first_year+(n+h)/12), ylim=c(0.8,1.25), lwd=2, main='EUR/CHF predictions \n Trend removal method: linear model', ylab='Rate', xlab='')
# For each year add line and bounds (linear model)
for (i in 1:length(years)){
  draw_predictions(years[i],colors[i], percentages[i], bound_colors[i], method='lm')
}
# We see how bounds narrow down for a shorter series

# Main plot 
ts.plot(series, xlim=c(first_year_plot,first_year+(n+h)/12), ylim=c(0.8,1.25), lwd=2, main='EUR/CHF predictions \n Trend removal method: differencing', ylab='Rate', xlab='')
# For each year add line and bounds (diff)
for (i in 1:length(years)){
  draw_predictions(years[i],colors[i], percentages[i], bound_colors[i], method='diff')
}
