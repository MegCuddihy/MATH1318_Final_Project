library(TSA)
library(FSAdata)
library(lmtest)
library(fUnitRoots)

# Testing seasonality
ms.seasonality <- function(data, title, xlabel, ylabel, lwr_range, upr_range,fig_num){
  par(mfrow=c(1,1))
  for (f in (lwr_range:upr_range)){
    ts_data <- ts(data, frequency = f)
    plot(ts_data, type='l', main=paste0("Figure ",fig_num,": ","Testing seasonality/cyclicity in ",title, " for frequency f = ", f), ylab = ylabel, cex.main=0.8)
    pch_label <- c()
    for (l in (1:f)){ pch_label <- append(pch_label, as.character(l)) }
    points(y = ts_data, x = time(ts_data), pch=pch_label)
    fig_num = fig_num + 1
  }
  return(fig_num)
}


ms.autocorrelation <- function(ts_object, lag, title, xlabel, ylabel,fig_num){
  par(mfrow=c(1,1))
  plot(y = ts_object, x = zlag(ts_object, lag), ylab = ylabel, xlab=xlabel, main=paste0("Figure ",fig_num,": ",title), cex.main=0.8)
  y = ts_object
  x = zlag(ts_object, lag)
  index = 2:length(ts_object)
  print(paste0("Autocorrelation of: ",round(cor(y[index], x[index]),4)))
  fig_num = fig_num + 1
  return(fig_num)
}


# ACF PACF side by side plot
acf_pacf <- function(data, fig_num, title=NULL, lagmax = NULL){
  par(mfrow=c(1,2))
  acf(data, main=if (is.null(title)) paste0("Figure ",fig_num,"A: ACF") else paste0("Figure ",fig_num,"A: ACF of ", title), lag.max = lagmax, cex.main=0.8)
  pacf(data, main=if (is.null(title)) paste0("Figure ",fig_num,"B: PACF") else paste0("Figure ",fig_num,"B: PACF of ", title), lag.max = lagmax, cex=0.8)
  fig_num = fig_num + 1
  return(fig_num)
}

# Unit Root Test
test_adf <- function(data, which_function){
  if (which_function=="funitroot"){
    order <- ar(diff(data))$order
    adfTest(data, lags = order, title=NULL, description = NULL)
  } else if (which_function=="tseries"){
    adf.test(data, alternative = c("stationary", "explosive"), k = trunc((length(data)-1)^(1/3)))
  }
}

# DIFF TEST
test_diff <- function(data, title, fig_num){
  acf_pacf(data, fig_num)
  fig_num = fig_num + 1
  par(mfrow=c(1,1))
  plot(data, type='o', main=paste0("Figure ",fig_num,": A timeseries plot ",title), ylab="(in millions)", cex.main=0.8)
  fig_num = fig_num + 1
  print(test_adf(data, "funitroot"))
  return(fig_num)
}

# Normalization - Boxcox
boxcox <- function(data, lambda) {
  data.boxcox <- (data ^ lambda - 1) / lambda 
  return(data.boxcox)
}

# Reverse Boxcox
reverse.boxcox <- function(data, lambda) {
  data.inv <- (lambda * data + 1) ^(1/lambda)
  return(data.inv)
}

# Testing normality
ts_normality_test <- function(data, title, fig_num){
  
  # Distribution of standardised residuals
  hist(data, main=paste0("Figure ",fig_num,": Distribution for ",title), cex.main=0.8)
  fig_num = fig_num + 1
  
  # QQ PLot of residuals
  qqnorm(data, main=paste0("Figure ",fig_num,": Normal Q-Q Plot for ",title), cex.main=0.8)
  qqline(data, col = 2, lwd = 1, lty = 2)
  fig_num = fig_num + 1
  
  # Shapiro Test
  print(shapiro.test(data))
  return(fig_num)
}


# Collecting Possible Orders
possible.orders <- function(data, title, fig_num, arm = NULL, mam=NULL){
  fig_num <- acf_pacf(data, fig_num)
  par(mfrow=c(1,1))
  eacf(data, ar.max = arm, ma.max=mam)
  options(warn=-1)
  res <- armasubsets(y=data, nar=arm, nma = arm, y.name='test', ar.method='ols')
  plot(res, main=paste0("Figure ",fig_num,": BIC test for possible orders"), cex.main=0.8) 
  fig_num = fig_num + 1
  return(fig_num)
}


# Parameter Estimate, Hypothesis Test and Model Diagnostics
parameter.diagnostic <- function(data,method,p,d,q,title,fig_num){
  
  # which model is this parameter estimation, hypothesis test and diagnostic test for?
  print(paste0("model.",as.character(p),as.character(d),as.character(q)))
  
  # create the model
  if ('mle' %in% method){
    model.mle <- arima(data,order=c(p,d,q),method='ML')
    print('Hypothesis test for Maximum Likelihood Estimate method of coefficient estimation')
    print(coeftest(model.mle))
  }
  
  if ('lse' %in% method){
    model.lse <- arima(data,order=c(p,d,q),method='CSS')
    print('Hypothesis test for Least Squares Estimate method of coefficient estimation')
    print(coeftest(model.lse))
  }
  
  # prepare grid for plot
  par(mfrow=c(3,2))
  
  # plot of standardised residuals
  plot(rstandard(model.mle), ylab="Standardised Residuals", type="o", main=paste0("Figure ",fig_num,"A: Time Series Plot of standardised residuals for\n",title), cex.main=0.8)
  abline(h=0)
  
  # save the residuals to an object
  e <- residuals(model.mle)
  
  # Test normalcy by plotting the qq plot
  qqnorm(e, main=paste0("Figure ",fig_num,"B: Normal Q-Q Plot for residuals of\n",title), cex.main=0.8)
  qqline(e)
  
  # Test normalcy by histogram
  hist(e, xlab = 'Standardized Residuals', main=paste0("Figure ",fig_num,"C: Histogram of residuals"), cex.main=0.8)
  
  # Perform ACF/ PACF on the residual of the models
  acf(residuals(model.mle), main=paste0("Figure ",fig_num,"D: ACF of the residuals of the model") , cex.main=0.8)

  # Plot Box-Ljung test
  tsdiag(model.mle, gof=15, omit.initial=F)
  
  # progress fig_num
  fig_num = fig_num + 1
  
  # Execute a Box-Ljung test
  print(Box.test(residuals(model.mle), lag=6, type="Ljung-Box",fitdf=0))
  
  
  # Test normalcy with the shapiro test
  print(shapiro.test(e))
  
  
  return(model.mle)
}

# Sort score
sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}


standresidual_analysis <- function(model,object_ts,xlabel,title){
  plot(y=rstudent(model),x=as.vector(time(object_ts)), xlab=xlabel,ylab='Standardized Residuals',type='o', main = paste0("Plot of Standardised Residuals of ",title))
  acf(rstudent(model), main="ACF of standardize residuals for model", cex.main=0.8)
}

test.res.normality <- function(model){
  
  # Distribution of standardised residuals
  hist(rstudent(model), xlab = 'Standardized Residuals', main='Histogram of model\'s standardised residuals', cex.main=0.8)
  
  # QQ PLot of residuals
  y <- rstudent(model)
  qqnorm(y)
  qqline(y, col = 2, lwd = 1, lty = 2)
  
  # Shapiro Test
  y <- rstudent(model)
  shapiro.test(y)
}

test.normality <- function(data){
  
  # Distribution of standardised residuals
  hist(data, main='Histogram of time series', cex.main=0.8)
  
  # QQ PLot of residuals
  y <- data
  qqnorm(y)
  qqline(y, col = 2, lwd = 1, lty = 2)
  
  # Shapiro Test
  y <- data
  shapiro.test(y)
}

