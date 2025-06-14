options(repos = c(CRAN = "https://cloud.r-project.org"))
# install.packages("httr")
# install.packages("jsonlite")
library(httr)
library(jsonlite)

# --- Process Bitcoin Data ---
library(ggplot2)
bitcoin <- merged_df
names(bitcoin) <- c("Date", "FG", "Price")
bitcoin$Date <- as.Date(bitcoin$Date)
bitcoin <- bitcoin[order(bitcoin$Date), ]
log_returns <- diff(log(bitcoin$Price))
logd <- log_returns - mean(log_returns)

# Create a dataframe for log returns (for ggplot)
log_df <- data.frame(
  Date = bitcoin$Date[-1],
  logd = logd
)

set.seed(1)

bitcoin <- merged_df
names(bitcoin) <- c("Date", "FG", "Price")

# Convert the Date column to Date type (assuming it's in "YYYY-MM-DD" format)
bitcoin$Date <- as.Date(bitcoin$Date)

# Order the data by Date (if not already sorted)
bitcoin <- bitcoin[order(bitcoin$Date), ]

# ---- GARCH Analysis ----
library(quantmod)
library(FinTS)
library(rugarch)

bitcoin_ret <- diff(log(bitcoin$Price))
bitcoin_ret_demeaned <- bitcoin_ret - mean(bitcoin_ret)
# 1. Fetch & prep
# getSymbols("BTC-USD", from = "2020-01-01")
# ret <- diff(log(Cl(`BTC-USD`)))[-1]
ret <- bitcoin_ret

# 2. Preliminary tests
print( ArchTest(ret, lags = 12) )
print( Box.test(ret^2, lag = 12, type = "Ljung-Box") )

## GARCH Analysis
library(quantmod)
library(FinTS)
library(rugarch)

bitcoin_ret <- diff(log(bitcoin$Price))
bitcoin_ret_demeaned <- bitcoin_ret - mean(bitcoin_ret)
# 1. Fetch & prep
# getSymbols("BTC-USD", from = "2020-01-01")
# ret <- diff(log(Cl(`BTC-USD`)))[-1]
ret <- bitcoin_ret

# 2. Preliminary tests
print( ArchTest(ret, lags = 12) )
print( Box.test(ret^2, lag = 12, type = "Ljung-Box") )

# 1. Set up the grid of (p,q)
maxP <- 3
maxQ <- 3
grid <- expand.grid(p = 1:maxP, q = 1:maxQ)

# prepare columns for fit statistics
grid$aic    <- NA_real_
grid$bic    <- NA_real_
grid$logLik <- NA_real_

# 2. Loop over each (p,q), fit garch, record stats
for(i in seq_len(nrow(grid))) {
  p <- grid$p[i]
  q <- grid$q[i]
  
  fit <- try(
    tseries::garch(ret, order = c(p, q), trace = FALSE),
    silent = TRUE
  )
  
  if (!inherits(fit, "try-error")) {
    # extract log‐likelihood, AIC, BIC
    ll       <- as.numeric(stats::logLik(fit))
    grid$logLik[i] <- ll
    grid$aic[i]    <- AIC(fit)     # = -2*ll + 2*k
    grid$bic[i]    <- BIC(fit)     # = -2*ll + log(n)*k
  }
}

# 3. Rank by AIC (or BIC)
grid_ordered_aic <- grid[order(grid$aic), ]
grid_ordered_bic <- grid[order(grid$bic), ]

# 4. View the top 5 models by AIC
print(head(grid_ordered_aic, 5), digits = 4)

# 5. Refit & diagnose your chosen “best” model, say (p*, q*)
best_garch <- grid_ordered_aic[1, ]
cat("Best by AIC → p =", best_garch$p, " q =", best_garch$q, " loglik =", best_garch$logLik, "\n")

best_fit <- tseries::garch(ret,
                           order = c(best_garch$p, best_garch$q),
                           trace = FALSE)

# 6. Check residual diagnostics
# pull out the standardized residuals (a plain vector/ts)
stdresid <- residuals(best_fit)

# Box test
Box.test(stdresid^2, lag = 12, type = "Ljung-Box")

# ARCH LM test (Engle’s test)
print(FinTS::ArchTest(stdresid, lags = 12))

acf(residuals(best_fit),
    lag.max   = 30,
    main      = "ACF of Std. Residuals",
    sub = "Figure 5. QQ plot of Standardized Residuals using GARCH(3,1)",
    na.action = na.omit)

acf(residuals(best_fit)^2,
    lag.max   = 30,
    main      = "ACF of Squared Std. Residuals",
    sub = "Figure 6. QQ plot of Standardized Residuals using GARCH(3,1)",
    na.action = na.omit)

qqnorm(stdresid, 
       main = "QQ Plot of Standardized Residuals",
       sub = "Figure 7. QQ plot of Standardized Residuals using GARCH(3,1)")

qqline(stdresid, 
       col  = "steelblue", 
       lwd  = 2)


