
# Libraries ---------------------------------------------------------------

require(zoo)
require(tseries)
require(Hmisc)
require(MCS)
require(lubridate)
require(fasttime)


# Estimates for roughness index -------------------------------------------

# We can pass either log(squared_vol) or log(vol) and obtain the exact same result.
# If either squared_vol or vol are directly passed, results change, but not much (persistence is
# significaatively more affected)

roughness_index_estim <- function(vol_ts,bandwidth, cut_alpha = TRUE){
  vrgm <- c()
  # Conputing variogram to estimate roughness index
  for (k in 1:bandwidth) {
    vrgm[k] <- mean(abs(Lag(vol_ts,shift = k) -vol_ts)^2,na.rm = TRUE)
  }
  # Data frame for linear model to compute roughness index
  rfsv_df <- data.frame(lag = 1:bandwidth,vrgm)
  # Linear model to compute roughness index
  rfsv_ols_lm <- lm(formula = log(vrgm) ~ log(lag),data = rfsv_df)
  # Extracting coefficients of linear model
  rfsv_ols_est <- coef(rfsv_ols_lm)
  # Estimating roughness index
  alpha_rough <- (rfsv_ols_est[2]-1)/2
  if (cut_alpha) {
    # Ensuring roughness index falls within its theoretical bounds
    alpha_rough <- ifelse(test = alpha_rough <= -(0.5 - 1e-7), yes = -(0.5 - 1e-7), no = alpha_rough)
  }
  return(alpha_rough)
}

# roughness_index_estim(vol_ts = log(sqrt(BV$eurusd)), bandwidth = 6)


# Persistence estimation with semi-parametric approach --------------------

# We can pass either log(vol) or log(sqared_vol) as vol_ts and get the same result.
# When passing either squared_vol or vol directly, the result changes

persistence_index_ols <- function(vol_ts, op_abs = TRUE){
  n <- length(vol_ts)
  # Defining lags to use to estimate persistence index
  first_lag <- ceiling(n^(1/4))
  last_lag <- ceiling(n^(1/3))
  # Extracting empirical ACF from volatility observations
  acf_vol_ts <- acf(x = vol_ts, plot = FALSE, lag.max = last_lag+1)$acf[first_lag:last_lag]
  # Creating data frame to run linear model to estimate persistence index
  acf_df <- data.frame(lag=first_lag:last_lag, acf_vol_ts)
  if (op_abs) {
    # Running linear regression to estimate persistence index
    persistence_lm <- lm(formula = log(abs(acf_vol_ts)) ~ log(lag), data = acf_df)
  } else{
    # Running linear regression to estimate persistence index
    persistence_lm <- lm(formula = log(acf_vol_ts) ~ log(lag), data = acf_df)
  }
  # Extracting estimate or persistence index
  beta_ols <- (-1)*coefficients(persistence_lm)[2]
  # Ensuring the persistence index is within its theoretical acceptable values
  beta_ols <- ifelse(test = beta_ols <= 0, yes = 0.0001, no = beta_ols)
  return(beta_ols)
}

# persistence_index_ols(vol_ts = log(sqrt(BV$eurusd)))

# Persistence index with parametric approach


# Cauchy model for log-vol ------------------------------------------------

acf_cauchy <- function(lags, a, b){
  e <- 2*a+1
  return((1 + abs(lags)^e)^(-b/e))
}

# acf_cauchy(lags = 0:20, a = -0.421237, b = 0.1539104)

acf_cauchy_mse <- function(b_mse, empirical_acf, lags_mse, a_mse){
  theoretical_acf <- acf_cauchy(lags = lags_mse, a = a_mse, b = b_mse)
  mse <- mean((empirical_acf - theoretical_acf)^2)
  return(mse)
}

# emp_acf <- acf(x = log(BV$eurusd))$acf[1:(last_lag+1)]

# acf_cauchy_mse(b_mse = 0.15, empirical_acf = emp_acf, lags_mse = 1:(last_lag+1), a_mse = alpha_rough)

# vol_ts_fit is either log(vol) or vol

# We can pass either log(vol) or log(squared_vol) as vol_ts and get the exact same result.
# When passing either squared_vol or vol directly, the result changes, but not drastically

fit_cauchy <- function(vol_ts_fit){
  # Estimating roughness index for the data (already ensured to be above -0.5)
  rough_index_ols <- roughness_index_estim(vol_ts = vol_ts_fit, bandwidth = 6)
  # Ensuring the roughness index is within the addtional Cauchy restriction (below 0.5)
  rough_index_ols <- ifelse(test = rough_index_ols >= (0.5-1e-7), yes = (0.5-1e-7), no = rough_index_ols)
  # Estimating persistence index for the data (already ensured to be within its acceptable values)
  beta_ols <- persistence_index_ols(vol_ts = vol_ts_fit, op_abs = TRUE)
  # Defining lags to be used to fit the Cauchy ACF to emprical ACF
  n <- length(vol_ts_fit)
  last_lag <- ceiling(n^(1/3))
  # Extracting empirical ACF of volatility observations
  emp_acf <- acf(x = vol_ts_fit, plot = FALSE, lag.max = last_lag+1)$acf[1:(last_lag+1)]
  # Fitiing Cauchy model: minimizing MSE between theoretical Cauchy ACF and empirical ACF
  opt <- nlminb(start = beta_ols, objective = acf_cauchy_mse, lower=1e-308,
                empirical_acf = emp_acf, lags_mse = 0:last_lag,
                a_mse = rough_index_ols, control = list(trace=0))
  # Extracting estimate of persistence index obtained from moments method
  beta_parametric <- opt$par
  # Keeping track of the convergence of the moments method
  conv_code <- opt$convergence
  # Returning the parameters of the Cauchy model and track of the convergence
  cauchy_param <- list(roughness_index = rough_index_ols,
                       persitence_index_ols = beta_ols,
                       persitence_index_cauchy = beta_parametric,
                       convergence_code = conv_code)
  return(cauchy_param)
}

# fit_cauchy(log(sqrt(BV$eurusd)))
# fit_cauchy(sqrt(BV$eurusd))


# Power BSS model for log-vol ---------------------------------------------

power_kernel <- function(x, a, g){
  e <- (-1)*(g+a)
  return((x^a)*((1+x)^e))
}

# power_kernel(x = 5, a = -0.421237, g = 0.1539104)

power_bss_acf_integrand <- function(x_int, h, a_int, g_int){
  # Integrand of the function that defines the Covariance function of the Power-BSS model
  return(power_kernel(x = x_int, a = a_int, g = g_int)*power_kernel(x = (x_int + abs(h)), a = a_int, g = g_int)) 
}

power_bss_cov_pre <- function(h_cov, a_cov, g_cov){
  # Computing integral that defines the Covariance function of the Power-BSS model
  integrate(f = power_bss_acf_integrand, lower = 0, upper = Inf, stop.on.error = FALSE,
            a_int = a_cov, g_int = g_cov, h = h_cov)$value
}

# power_bss_cov_pre(h_cov = 1, a_cov = -0.421237, g_cov = 0.575)

# VEctorizing the Covariance function for the Power-BSS model
power_bss_cov <- Vectorize(FUN = power_bss_cov_pre, vectorize.args = "h_cov")

power_bss_acf <- function(lags, a_acf, g_acf){
  # Computing covariance function of the Power-BSS model
  cov_power_bss <- power_bss_cov(h_cov = lags, a_cov = a_acf, g_cov = g_acf)
  # Parameters for the Beta function definining the variance of the Power-BSS model
  a_beta <- 2*a_acf+1
  b_beta <- 2*g_acf-1
  # Computing the variance of the Power-BSS model
  var_power_bss <- beta(a = a_beta, b = b_beta)
  # Computing the ACF of the Power-BSS model
  cor_power_bss <- cov_power_bss/var_power_bss
  # Ensuring the correlation is 1 at lag 0 (the function estimates it as number close to one,
  # but some precision is lost due to the numerical integration step used for the covariance
  # function of the Power-BSS model)
  if(length(which(lags==0)) > 0){
    cor_power_bss[which(lags==0, arr.ind = TRUE)] <- 1
  }
  return(cor_power_bss)
}

# power_bss_acf(lags = c(0,0:20,0,0,0), a_acf = -0.421237, g_acf = 0.575)

acf_power_bss_mse <- function(g_mse, empirical_acf, lags_mse, a_mse){
  # Computing theoretical ACF of the Power-BSS model for the given parameters
  theoretical_acf <- power_bss_acf(lags = lags_mse, a = a_mse, g_acf = g_mse)
  # Commputing MSE between theoretical ACF and empirical ACF
  mse <- mean((empirical_acf - theoretical_acf)^2)
  return(mse)
}

# acf_power_bss_mse(g_mse = 0.575, empirical_acf = emp_acf, lags_mse = 1:(last_lag+1), a_mse = alpha_rough)

# We can pass either log(vol) or log(squared_vol) as vol_ts and get the exact same result.
# When passing either squared_vol or vol directly, the result changes

fit_power_bss <- function(vol_ts_fit){
  # Estimating roughness index for the data (already ensured to be above 0.5)
  rough_index_ols <- roughness_index_estim(vol_ts = vol_ts_fit, bandwidth = 6)
  # Ensuring the roughness index is within the addtional Cauchy restriction (below 0.5)
  rough_index_ols <- ifelse(test = rough_index_ols >= (0.5-1e-7), yes = (0.5-1e-7), no = rough_index_ols)
  # Estimating persistence index for the data (already ensured to be within its acceptable values)
  beta_ols <- persistence_index_ols(vol_ts = vol_ts_fit)
  # Estimating gamma parameter of the Power-BSS model from OLS estimate of persistence index. This will
  # serve as a good first guess for the fitting procedure later
  g_ols <- ifelse(test = beta_ols>1,yes = beta_ols, no = ((beta_ols+1)/2))
  # Ensuring first guess is within acceptable theoretical values (0.5 < g_ols < Inf)
  g_ols <- ifelse(test = g_ols <= (0.5+1e-7), yes = (0.5+1e-7), no = g_ols)
  # Defining lags to be used to fit the Cauchy ACF to emprical ACF
  n <- length(vol_ts_fit)
  last_lag <- ceiling(n^(1/3))
  # Extracting empirical ACF from volatility observations
  emp_acf <- acf(x = vol_ts_fit, plot = FALSE, lag.max = last_lag+1)$acf[1:(last_lag+1)]
  # Fitting Power-BSS model: minimizing square distances between theoretical and empirical ACF's
  # Restrained optimization is used to ensure (0.5 < g_parametrix < Inf)
  opt <- nlminb(start = g_ols, objective = acf_power_bss_mse, hessian = TRUE,lower= (0.5 + 1e-32),
                empirical_acf = emp_acf, lags_mse = 0:last_lag,
                a_mse = rough_index_ols, control = list(trace=0))
  # Extracting estimate of gamma parameter of Power-BSS model
  g_parametric <- opt$par
  # Estimating Persistence index from gamma parameter of Power-BSS model
  beta_parametric <- ifelse(test = g_parametric >= 1, yes = g_parametric, no = (2*g_parametric-1))
  cauchy_param <- list(roughness_index = rough_index_ols,
                       persitence_index_ols = beta_ols,
                       persitence_index_bss=beta_parametric,
                       gamma_par_bss=g_parametric,
                       convergence_code=opt$convergence)
  return(cauchy_param)
}

# fit_power_bss(vol_ts_fit = log(sqrt(BV$eurusd)))


# Gamma BSS model for log(squared_vol) ------------------------------------

gamma_kernel <- function(x, a, l){
  return((x^a)*exp((-1)*l*x))
}

# gamma_kernel(x = 5, a = -0.421237, l =  0.15)

gamma_bss_cov <- function(h_cov, a_cov, l_cov){
  # Ensuring the Lambda parameter is positive before computing the covariance of the Gamma-BSS model 
  # if(l_cov < 0 ){
  #   l_cov <- 0.00001
  # }
  # Computing the threee factors ton conform the covariance of the Gamma-BSS model
  f1 <- gamma(x = (a_cov + 1))/sqrt(x = pi)
  f2 <- (abs(h_cov)/(2*l_cov))^(a_cov + 0.5)
  f3 <- besselK(x = (l_cov*abs(h_cov)), nu = (a_cov + 0.5))
  # Returning the product of the three factors
  return(f1*f2*f3)
}

# gamma_bss_cov(h_cov = 1, a_cov = -0.421237, l_cov = 0.0009)

gamma_bss_acf <- function(lags, a_acf, l_acf){
  # Computing the covariance of the Gamma-BSS model
  cov_gamma_bss <- gamma_bss_cov(h_cov = lags, a_cov = a_acf, l_cov = l_acf)
  e <- (-1)*(2*a_acf + 1)
  # Computing the variance of the Gamma-BSS model
  var_gamma_bss <- ((2*l_acf)^e) * gamma(x = (2*a_acf + 1))
  # Computing the ACF of the Gamma-BSS model
  cor_gamma_bss <- cov_gamma_bss/var_gamma_bss
  # Ensuring the function returns a correlation of 1 for the lag of 0
  cor_gamma_bss[is.nan(cor_gamma_bss)] <- 1
  return(cor_gamma_bss)
}

# gamma_bss_acf(lags = 1, a_acf = -0.421237, l_acf = 0.15)

acf_gamma_bss_mse <- function(l_mse, empirical_acf, lags_mse, a_mse){
  # Computing theoretical ACF of the Gamma-BSS model
  theoretical_acf <- gamma_bss_acf(lags = lags_mse, a = a_mse, l_acf = l_mse)
  # Computing MSE between theoretical and empirical ACF for the Gamma-BSS model 
  mse <- mean((empirical_acf - theoretical_acf)^2)
  return(mse)
}

# acf_gamma_bss_mse(l_mse = 1e-20, empirical_acf = emp_acf, lags_mse = 1:(last_lag+1), a_mse = alpha_rough)

# We can pass either log(vol) or log(squared_vol) as vol_ts and get the exact same result.
# When passing either squared_vol or vol directly, the result changes, but not drastically

fit_gamma_bss <- function(vol_ts_fit){
  # Estimating roughness index for the data (already ensured to be above 0.5)
  rough_index_ols <- roughness_index_estim(vol_ts = vol_ts_fit, bandwidth = 6)
  # Ensuring the roughness index is within the addtional Cauchy restriction (below 0.5)
  rough_index_ols <- ifelse(test = rough_index_ols >= (0.5-1e-7), yes = (0.5-1e-7), no = rough_index_ols)
  # Estimating persistence index for the data (already ensured to be within its acceptable values)
  beta_ols <- persistence_index_ols(vol_ts = vol_ts_fit)
  # Defining lags to be used to fit the Cauchy ACF to emprical ACF
  n <- length(vol_ts_fit)
  last_lag <- ceiling(n^(1/3))
  # Extracting empirical ACF from volatility observations
  emp_acf <- acf(x = vol_ts_fit, plot = FALSE, lag.max = last_lag+1)$acf[1:(last_lag+1)]
  # Fitting Power-BSS model: minimizing square distances between theoretical and empirical ACF's
  # Restrained optimization is used to ensure (0 < l_parametric < Inf)
  opt <- nlminb(start = beta_ols, objective = acf_gamma_bss_mse, lower=1e-308,
               empirical_acf = emp_acf, lags_mse = 0:last_lag,
               a_mse = rough_index_ols, control = list(trace=0))
  # Extracting moments estimate of the lambda parameter of the Gamma-BSS model
  l_parametric <- opt$par
  gamma_bss_param <- list(roughness_index = rough_index_ols,
                          persitence_index_ols = beta_ols,
                          l_bss=l_parametric,
                          convergence_code=opt$convergence)
  return(gamma_bss_param)
}

# fit_gamma_bss(vol_ts_fit = log(sqrt(BV$eurusd)))

