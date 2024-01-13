## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----results='hide'-----------------------------------------------------------
library(bayesianVARs)
variables <- c("GDPC1", "GDPCTPI",  "FEDFUNDS", "EXUSUKx", "S&P 500")
train_data <- 100 * usmacro_growth[1:230, variables]
test_data <- 100 * usmacro_growth[231:234, variables]

## ----results='hide'-----------------------------------------------------------
prior_phi <- specify_prior_phi(data = train_data, 
                               lags = 2L, 
                               prior = "HS", 
                               global_grouping = "olcl-lagwise")
prior_sigma <- specify_prior_sigma(data = train_data, 
                                   type = "factor", 
                                   factor_factors = 4L)
                                   
mod <- bvar(train_data, lags = 2L, draws = 10000, burnin = 2000, 
            prior_phi = prior_phi, prior_sigma = prior_sigma, 
            sv_keep = "all")

## ----out.width="80%", fig.align='center', fig.cap="Visualization of estimated in-sample prediction intervals. The red solid line depicts the median, the red shaded region the 90% credible interval and the black dotted line the observed data used for estimation.", fig.pos="t"----
plot(mod, quantiles = c(0.05,0.5,0.95), dates = rownames(mod$Yraw))

## ----out.width="50%", fig.align='center', fig.cap="Posterior summary of the VAR coefficients. Left: Heatmap of the posterior median. Right: Heatmap of the posterior interquartile range.", fig.pos="t"----
phi <- coef(mod)
oldpar <- par(mfrow=c(1,2))
posterior_heatmap(phi,median)
posterior_heatmap(phi, IQR)
par(oldpar)

## ----results='hide'-----------------------------------------------------------
pred <- predict(mod, ahead = 1:4, LPL = TRUE, Y_obs = test_data)

## ----out.width="80%", fig.align='center', fig.cap="Fan-charts visualizing the last 15 out of 230 observations used for estimation through black solid lines, the median of the $h$-step ahead predictive distribution through red solid lines and the 50%/90% credible intervals of the $h$-step ahead predictive distribution through red shaded regions for $h=1,\\dots,4$.", fig.pos="t"----
plot(pred, first_obs = 216, 
     dates = c(rownames(train_data[-c(1:215),]), rownames(test_data)))

## -----------------------------------------------------------------------------
pred$LPL

