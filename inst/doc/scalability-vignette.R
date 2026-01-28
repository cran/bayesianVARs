## -----------------------------------------------------------------------------
#| label: setup
library(bayesianVARs)


## -----------------------------------------------------------------------------
#| label: factor_toyexample
#| results: hide
#| eval: false
# set.seed(321)
# # Simulate data
# n <- 50L # number of observations
# M <- 3L # number of time series
# Y <- matrix(rnorm(n*M), n, M)
# colnames(Y) <- paste0("y", 1:M)
# # Estimate a VAR with a factor structure on the errors
# prior_sigma_f <- specify_prior_sigma(Y, type = "factor")
# mod_f1 <- bvar(Y, draws = 50L, burnin = 5L, prior_sigma = prior_sigma_f,
#             quiet = TRUE)


## -----------------------------------------------------------------------------
#| label: factor_toyexample_huge
#| eval: false
# mod_f2 <- bvar(Y, draws = 50L, burnin = 5L, prior_sigma = prior_sigma_f,
#                expert_huge = TRUE, quiet = TRUE)


## -----------------------------------------------------------------------------
#| label: cholesky_toyexample
#| results: hide
#| eval: false
# # Estimate a VAR with a Cholesky structure on the errors
# prior_sigma_c <- specify_prior_sigma(Y, type = "cholesky")
# mod_c <- bvar(Y, draws = 50L, burnin = 5L, prior_sigma = prior_sigma_c,
#             quiet = TRUE)


## -----------------------------------------------------------------------------
#| label: sim
#| eval: false
# set.seed(123)
# ps <- 1:5 # lags
# M <- 50L # number time series
# n <- 100L # number of observations for estimation
# factors <- c(1L,50L) # number of factors
# huge <- c(FALSE, TRUE) # expert_huge algorithm
# svs <- c(FALSE, TRUE) # stochastic volatility
# factor_combinations <- expand.grid(p = ps, M = M, n = n, r = factors,
#                                 expert_huge = huge, sv = svs, type = "factor",
#                                 stringsAsFactors = FALSE)
# cholesky_combinations <- expand.grid(p = ps, M = M, n = n, r = 0,
#                                 expert_huge = FALSE, sv = svs, type = "cholesky",
#                                 stringsAsFactors = FALSE)
# all_combinations <- rbind(factor_combinations, cholesky_combinations)
# Tobs <- all_combinations$n + all_combinations$p # number of total observations
# all_combinations <- cbind(all_combinations, Tobs = Tobs, user_time = as.numeric(NA))
# for(i in seq_len(nrow(all_combinations))){
#   p <- all_combinations$p[i]
#   M <- all_combinations$M[i]
#   Tob <- all_combinations$Tobs[i]
#   r <- all_combinations$r[i]
#   expert_huge <- all_combinations$expert_huge[i]
#   type <- all_combinations$type[i]
#   sv <- all_combinations$sv[i]
# 
#   # simulate data
#   Y <- matrix(rnorm(Tob*M), Tob, M)
#   colnames(Y) <- paste0("y", 1:M)
#   # prior configuration
#   prior_sigma <- specify_prior_sigma(data = Y, type = type, factor_factors = r,
#                                      factor_heteroskedastic = sv,
#                                      cholesky_heteroscedastic = sv,
#                                      quiet = TRUE)
#   # generate posterior draws
#   res <- bvar(Y, lags = p, draws = 10L, burnin = 0L,
#                             prior_intercept = FALSE,
#                             prior_sigma = prior_sigma,
#        expert_huge = expert_huge, quiet = TRUE)
#   # store user time
#   all_combinations$user_time[i] <- res[["bench"]]["user.self"]
# }


## -----------------------------------------------------------------------------
#| label: load_data
#| echo: false
load("huge_all_combinations.RData")
ps <- unique(all_combinations$p) # lags
M <- unique(all_combinations$M) # number time series
n <- unique(all_combinations$n) # number of observations for estimation
factors <- unique(all_combinations$r) # number of factors
huge <- unique(all_combinations$expert_huge) # expert_huge algorithm
svs <- unique(all_combinations$sv)


## -----------------------------------------------------------------------------
#| label: fig-comp_times
#| layout-ncol: 2
#| layout-nrow: 2
#| fig-cap: "Computing times for generating 10 draws from the joint posterior distribution of all parameters and latent variables. The grey vertical line in the top left plot indicates the scenario where 'expert_huge' and '!expert_huge' are expected to have more or less equal running times. The plots in the bottom panel depict the relative change when moving from a homoscedastic VAR (constant error variance-covariance matrix w.r.t. time) to a heteroscedastic VAR (time varying error variance-covariance matrix featuring stochastic volatility)."
#| fig-subcap: 
#|  - "Factor stochastic volatility specification"
#|  - "Cholesky stochastic volatility specification"
#|  - "Factor specification: Relative change when moving from homoscedasticity to heteroscdedasticity"
#|  - "Cholesky specification: Relative change when moving from homoscedasticity to heteroscdedasticity"
for(type0 in c("factor", "cholesky")){
  tmp <- subset(all_combinations, type == type0 & sv == TRUE)
  plot(0,0, xlim = range(tmp$p), ylim = range(tmp$user_time),
     type="n", xlab = "p", ylab = "time in seconds")
    if(type0=="cholesky"){
      lines(tmp$p,
          tmp$user_time)
    }else{
      for(expert in huge){
        for(rr in factors){
          lines(with(tmp, subset(p, r == rr & expert_huge == expert & type == "factor")),
          with(tmp, subset(user_time, r == rr & expert_huge == expert & type == "factor")),
          col = expert + 1L, lty = if(rr==1L) 1 else 2)
        }
      }
      abline(v = n/M, col = "lightgrey")
      legend("topleft", legend = c("expert_huge & 1 factor", "expert_huge & 50 factors", 
                                   "!expert_huge & 1 factor", "!expert_huge & 50 factors"), 
             col = c(2L, 2L, 1L, 1L),
             lty = c(1L, 2L, 1L, 2L), bty="n")
    }
}

for(type0 in c("factor", "cholesky")){
  tmp <- subset(all_combinations, type == type0)
  plot(0,0, xlim = range(tmp$p), ylim = c(-1,1),
     type="n", xlab = "p", ylab = "relative change")
  if(type0 == "cholesky"){
    tmp_sv <- subset(tmp, sv == TRUE)
    tmp_nosv <- subset(tmp, sv == FALSE)
    thediff <- tmp_sv$user_time - tmp_nosv$user_time
    normalizer <- tmp_nosv$user_time
    lines(tmp_sv$p, thediff/normalizer)
  }else{
    for(expert in huge){
      for(rr in factors){
        tmp_sv <- subset(tmp, sv == TRUE & r == rr & expert_huge == expert)
        tmp_nosv <- subset(tmp, sv == FALSE & r == rr & expert_huge == expert)
        thediff <- tmp_sv$user_time - tmp_nosv$user_time
        normalizer <- tmp_nosv$user_time
        lines(tmp_sv$p, thediff/normalizer,
          col = expert + 1L, lty = if(rr==1L) 1 else 2)
      }
    }
    legend("topright", legend = c("expert_huge & 1 factor", "expert_huge & 50 factors", 
                                 "!expert_huge & 1 factor", "!expert_huge & 50 factors"), 
           col = c(2L, 2L, 1L, 1L),
             lty = c(1L, 2L, 1L, 2L), bty = "n")
  }
  abline(h=0, col = "lightgrey", lty=2)

}

