## ----message=FALSE------------------------------------------------------------
library(bsvarSIGNs)
data(optimism)

## ----results="hide", cache=TRUE-----------------------------------------------
library(bayesianVARs)
prior_sigma <- specify_prior_sigma(
	M = ncol(optimism),
	type = "cholesky",
	cholesky_heteroscedastic = FALSE
)
mod <- bvar(optimism * 100, lags = 4L, prior_sigma = prior_sigma)

## ----echo=FALSE---------------------------------------------------------------
restrictions <- matrix(
	NA, ncol(optimism), ncol(optimism),
	dimnames=list(colnames(optimism), paste0("shock", 1:ncol(optimism)))
)
restrictions["productivity", "shock1"] <- 0
restrictions["stock_prices", "shock1"] <- 1

## -----------------------------------------------------------------------------
show(restrictions)

## -----------------------------------------------------------------------------
shock1 <- diag(ncol(optimism))[, 1, drop=FALSE]
show(shock1)

struct_restr <- specify_structural_restrictions(
	mod,
	restrictions_B0_inv_t = restrictions
)
shock1_irf <- irf(
	mod,
	ahead = 40,
	structural_restrictions = struct_restr,
	shocks = shock1
)

plot(shock1_irf, quantiles = c(0.16, 0.50, 0.84))

## ----echo=FALSE---------------------------------------------------------------
library(bayesianVARs)

## ----monetary-svar-restr1, cache=TRUE, results="hide"-------------------------
train_data <- 100 * usmacro_growth[
  , c("GDPC1", "GDPCTPI", "GS1", "M2REAL", "CPIAUCSL")
]
prior_sigma <- specify_prior_sigma(
  M = ncol(train_data),
  type = "cholesky",
  cholesky_heteroscedastic = FALSE
)
mod <- bvar(train_data, lags = 5L, prior_sigma = prior_sigma)

restrictions_B0 <- rbind(
  c(1 ,NA,0 ,NA,NA),
  c(0 ,1 ,0 ,NA,NA),
  c(0 ,0 ,1 ,NA,NA),
  c(0 ,0 ,NA,1 ,NA),
  c(0 ,0 ,0 ,0 ,1 )
)
restrictions <- specify_structural_restrictions(
  mod,
  restrictions_B0 = restrictions_B0
)

## ----monetary-svar-restr1-error, error=TRUE-----------------------------------
try({
irf1 <- irf(mod, ahead = 4, structural_restrictions = restrictions)
})

## ----monetary-svar-more-samples, cache=TRUE, results="hide"-------------------
mod <- bvar(
  train_data, lags = 5L, draws = 20*3000, prior_sigma = prior_sigma
)

## -----------------------------------------------------------------------------
restrictions_B0_relaxed <- rbind(
  c(1 ,NA,0 ,NA,NA),
  c(0 ,1 ,0 ,NA,NA),
  c(0 ,NA,1 ,NA,NA),
  c(0 ,0 ,NA,1 ,NA),
  c(0 ,0 ,0 ,0 ,1 )
)

## -----------------------------------------------------------------------------
restrictions_relaxed <- specify_structural_restrictions(
  mod, restrictions_B0 = restrictions_B0_relaxed
)
irf_relaxed <- irf(
  mod, ahead = 4,
  structural_restrictions = restrictions_relaxed
)
B0_relaxed <- extractB0(irf_relaxed)

## -----------------------------------------------------------------------------
B0_31_approx_zero <- abs(B0_relaxed[3,2,]) < 0.01
sum(B0_31_approx_zero)

## -----------------------------------------------------------------------------
irf_conditional <- irf_relaxed[,,,B0_31_approx_zero]
class(irf_conditional) <- class(irf_relaxed)
plot(irf_conditional)

## ----cache=TRUE, results="hide"-----------------------------------------------
prior_sigma <- specify_prior_sigma(
  M = ncol(usmacro_growth),
  type = "factor",
  factor_factors = 2,
  factor_restrict = "none",
  factor_heteroskedastic = FALSE
)
mod <- bvar(100 * usmacro_growth, lags = 5L, prior_sigma = prior_sigma)

## ----echo=FALSE---------------------------------------------------------------
restrictions_long_run_ir <- matrix(
  NA, nrow=ncol(usmacro_growth), ncol=prior_sigma$prior_sigma_cpp$factor_factors,
  dimnames=list(vars=colnames(usmacro_growth), factors=NULL)
)

## -----------------------------------------------------------------------------
show(restrictions_long_run_ir)
restr <- specify_structural_restrictions(
  mod,
  restrictions_long_run_ir = restrictions_long_run_ir,
)

shock1 <- diag(2)[, 1, drop = FALSE]

## ----cache=TRUE, fig.height=3-------------------------------------------------
x <- irf(mod, hairy = FALSE, structural_restrictions = restr, shocks = shock1)
plot(x, vars = "PCECTPI")

## ----cache=TRUE, fig.height=3-------------------------------------------------
x <- irf(mod, hairy = TRUE, structural_restrictions = restr, shocks = shock1)
plot(x, vars = "PCECTPI", quantiles=0.68, default_hair_color = "#FF000005")

