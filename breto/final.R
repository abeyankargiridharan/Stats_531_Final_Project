library(timeSeriesDataSets)
library(httr)
library(jsonlite)
library(ggplot2)

# # Call the API
# response <- GET("https://api.alternative.me/fng/?limit=2000")
# 
# # Parse JSON content
# content_json <- content(response, as = "text", encoding = "UTF-8")
# fng_data <- fromJSON(content_json)
# 
# # Convert the 'data' field to a data frame
# fng_df <- fng_data$data
# head(fng_df)
# 
# fng_df$date <- as.POSIXct(as.numeric(fng_df$timestamp), origin = "1970-01-01", tz = "UTC")
# fng_df$value <- as.numeric(fng_df$value)
# 
# fng_final <- fng_df[, c("date", "value")]
# names(fng_final) <- c("time", "FG")  # Rename for consistency
# head(fng_final)
# print(min(fng_final$time))
# 
# 
# ggplot(fng_final, aes(x = time, y = FG)) +
#   geom_line(color = "steelblue") +
#   labs(
#     title = "CNN Fear & Greed Index Over Time",
#     x = "Date",
#     y = "Fear & Greed Index"
#   ) +
#   theme_minimal()
# 
# nrow(fng_final)
# 
# start_date <- as.POSIXct("2020-01-01", tz = "UTC")
# end_date   <- as.POSIXct("2025-04-06", tz = "UTC")
# 
# # Subset the data
# fng_subset <- fng_final[fng_final$time >= start_date & fng_final$time <= end_date, ]
# 
# # Check the first few rows of the subset
# head(fng_subset)
# 
# # # Plot the subsetted data
# # ggplot(fng_subset, aes(x = time, y = FG)) +
# #   geom_line(color = "steelblue") +
# #   labs(
# #     title = paste0("CNN Fear & Greed Index (", start_date, " to ", end_date, ")"),
# #     x = "Date",
# #     y = "Fear & Greed Index"
# #   ) +
# #   theme_minimal()
# 
# # Read the bitcoin dataset (ensure strings are not converted to factors)
# bitcoin_ts <- read.csv('bitcoin_2020-01-01_2025-04-10.csv', stringsAsFactors = FALSE)
# 
# # Convert the 'Start' column to a POSIXct date object 
# bitcoin_ts$Start <- as.POSIXct(bitcoin_ts$Start, format = "%Y-%m-%d", tz = "UTC")
# 
# # Merge the F&G index dataset (fng_final) with the bitcoin dataset on matching dates
# merged_df <- merge(fng_subset, bitcoin_ts, by.x = "time", by.y = "Start")
# 
# # Select only the required columns and rename them:
# # "date" for the matching date, "fg_index" for the Fear & Greed Index, and "close" for the Bitcoin closing price.
# merged_df <- merged_df[, c("time", "FG", "Close")]
# names(merged_df) <- c("date", "fg", "close")
# 
# # Check the first few rows of the merged data
# head(merged_df)
# 
# 
# write.csv(merged_df, "bitcoin_fg.csv", row.names = FALSE)
# 

set.seed(2050320976)

### 1. Data Preparation and Plotting
# Read your Bitcoin data (assumes file "bitcoin.csv" exists)
# with columns "date" and "close"
bdat <- read.table("bitcoin_fg.csv", sep = ",", header = TRUE)


# First plot the FG Index with its y-axis (left)
par(mfrow = c(1, 1), mai = c(0.8, 0.8, 0.1, 0.3))
plot(as.Date(bdat$date), bdat$fg, type = "l", col = "blue",
     xlab = "Date", ylab = "FG Index", main = "FG Index and Bitcoin Price Over Time")

# Overlay the Bitcoin closing price with a new plot
par(new = TRUE)
plot(as.Date(bdat$date), bdat$close, type = "l", col = "red", 
     axes = FALSE, xlab = "", ylab = "")
# Add a right-side y-axis for Bitcoin
axis(side = 4, col = "red", col.axis = "red")
mtext("Bitcoin Close", side = 4, line = 3, col = "red")

# Add a legend
legend("topleft", legend = c("FG Index", "Bitcoin Close"), 
       col = c("blue", "red"), lty = 1)


# Plot the Bitcoin price series (regular and log-scale)
par(mfrow = c(1, 2), mai = c(0.8, 0.8, 0.1, 0.3))
plot(as.Date(bdat$date), bdat$close,
     xlab = "date", ylab = "Bitcoin Price", type = "l")
plot(as.Date(bdat$date), bdat$close, log = "y",
     xlab = "date", ylab = "Bitcoin Price", type = "l")

# Compute log-returns and demean them
bitcoin_ret <- diff(log(bdat$close))
bitcoin_ret_demeaned <- bitcoin_ret - mean(bitcoin_ret)
par(mfrow = c(1, 1))
plot(bitcoin_ret_demeaned, type="l",
     xlab="day (01/01/2020-04/06/2025)",ylab="demeaned bitcoin return")

### 2. Define the Model Components for pomp

# Set state names and parameter names
bitcoin_statenames <- c("H", "G", "Y_state")
bitcoin_rp_names   <- c("sigma_nu", "mu_h", "phi", "sigma_eta")
bitcoin_ivp_names  <- c("G_0", "H_0")
bitcoin_paramnames <- c(bitcoin_rp_names, bitcoin_ivp_names)

# rprocess: state evolution C snippet (used for both filtering and simulation)
rproc1 <- "
  double beta, omega, nu;
  omega = rnorm(0, sigma_eta * sqrt(1 - phi * phi) * sqrt(1 - tanh(G) * tanh(G)));
  nu = rnorm(0, sigma_nu);
  G += nu;
  beta = Y_state * sigma_eta * sqrt(1 - phi * phi);
  H = mu_h * (1 - phi) + phi * H + beta * tanh(G) * exp(-H/2) + omega;
"

# For simulation we simulate Y_state from the measurement error:
rproc2.sim <- "
  Y_state = rnorm(0, exp(H/2));
"
# For filtering we take Y_state from the covariate 'covaryt':
rproc2.filt <- "
  Y_state = covaryt;
"

bitcoin_rproc_sim <- paste(rproc1, rproc2.sim)
bitcoin_rproc_filt <- paste(rproc1, rproc2.filt)

# rinit: initialize the states
bitcoin_rinit <- "
  G = G_0;
  H = H_0;
  Y_state = rnorm(0, exp(H/2));
"

# rmeasure: measurement equation (here the observation is Y_state)
bitcoin_rmeasure <- "
  y = Y_state;
"

# dmeasure: log-likelihood for the observation
bitcoin_dmeasure <- "
  lik = dnorm(y, 0, exp(H/2), give_log);
"

# Parameter transformation (using log and logit for selected parameters)
library(pomp)
bitcoin_partrans <- parameter_trans(
  log = c("sigma_eta", "sigma_nu"),
  logit = "phi"
)

# Build the pomp object for filtering
bitcoin.filt <- pomp(
  data = data.frame(y = bitcoin_ret_demeaned,
                    time = 1:length(bitcoin_ret_demeaned)),
  statenames = bitcoin_statenames,
  paramnames = bitcoin_paramnames,
  times = "time",
  t0 = 0,
  covar = covariate_table(
    time = 0:length(bitcoin_ret_demeaned),
    covaryt = c(0, bitcoin_ret_demeaned),
    times = "time"
  ),
  rmeasure = Csnippet(bitcoin_rmeasure),
  dmeasure = Csnippet(bitcoin_dmeasure),
  rprocess = discrete_time(step.fun = Csnippet(bitcoin_rproc_filt), delta.t = 1),
  rinit = Csnippet(bitcoin_rinit),
  partrans = bitcoin_partrans
)

# Test parameter set (you may need to adjust these for Bitcoin)
params_test <- c(
  sigma_nu = exp(-4.5),
  mu_h = -0.25,
  phi = expit(4),      # expit(x) = 1 / (1 + exp(-x)); ensure it's defined or use plogis(4)
  sigma_eta = exp(-0.07),
  G_0 = 0,
  H_0 = 0
)

# Create a simulation pomp object with the simulation rprocess
sim1.sim <- pomp(bitcoin.filt,
                 statenames = bitcoin_statenames,
                 paramnames = bitcoin_paramnames,
                 rprocess = discrete_time(step.fun = Csnippet(bitcoin_rproc_sim), delta.t = 1)
)

sim1.sim <- simulate(sim1.sim, seed = 1, params = params_test)

# For filtering, update the pomp object with covariate information from the simulated observations
sim1.filt <- pomp(sim1.sim,
                  covar = covariate_table(
                    time = c(timezero(sim1.sim), time(sim1.sim)),
                    covaryt = c(obs(sim1.sim), NA),
                    times = "time"
                  ),
                  statenames = bitcoin_statenames,
                  paramnames = bitcoin_paramnames,
                  rprocess = discrete_time(step.fun = Csnippet(bitcoin_rproc_filt), delta.t = 1)
)

### 3. Set Up Parallel Computation and Control Parameters

# Choose run level (adjust these run levels as needed)
run_level <- 3
bitcoin_Np <-           switch(run_level,   50, 1e3, 2e3)
bitcoin_Nmif <-         switch(run_level,    5, 100, 200)
bitcoin_Nreps_eval <-   switch(run_level,    4,  10,  20)
bitcoin_Nreps_local <-  switch(run_level,    5,  20,  20)
bitcoin_Nreps_global <- switch(run_level,    5,  20, 100)

# Set up parallel processing
library(doParallel)
cores <- as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE", unset = NA))
if (is.na(cores)) cores <- detectCores()
registerDoParallel(cores)
library(doRNG)
registerDoRNG(34118892)

### 4. Particle Filtering and Log-likelihood Evaluation

stew(file = paste0("pf1_bitcoin_", run_level, ".rda"), {
  t.pf1 <- system.time(
    pf1 <- foreach(i = 1:bitcoin_Nreps_eval,
                   .packages = "pomp") %dopar% 
      pfilter(sim1.filt, Np = bitcoin_Np)
  )
})
L.pf1 <- logmeanexp(sapply(pf1, logLik), se = TRUE)
print(L.pf1)

### 5. Iterated Filtering (mif2) on Local Replicates

# Define random walk standard deviations for parameters (tuning parameters)
bitcoin_rw.sd_rp  <- 0.02
bitcoin_rw.sd_ivp <- 0.1
bitcoin_cooling.fraction.50 <- 0.5
bitcoin_rw.sd <- rw_sd(
  sigma_nu  = bitcoin_rw.sd_rp,
  mu_h      = bitcoin_rw.sd_rp,
  phi       = bitcoin_rw.sd_rp,
  sigma_eta = bitcoin_rw.sd_rp,
  G_0       = ivp(bitcoin_rw.sd_ivp),
  H_0       = ivp(bitcoin_rw.sd_ivp)
)

stew(file = paste0("mif1_bitcoin_", run_level, ".rda"), {
  t.if1 <- system.time({
    if1 <- foreach(i = 1:bitcoin_Nreps_local,
                   .packages = "pomp", .combine = c) %dopar% 
      mif2(bitcoin.filt,
           params = params_test,
           Np = bitcoin_Np,
           Nmif = bitcoin_Nmif,
           cooling.fraction.50 = bitcoin_cooling.fraction.50,
           rw.sd = bitcoin_rw.sd)
    L.if1 <- foreach(i = 1:bitcoin_Nreps_local,
                     .packages = "pomp", .combine = rbind) %dopar% 
      logmeanexp(
        replicate(bitcoin_Nreps_eval, logLik(pfilter(bitcoin.filt,
                                                     params = coef(if1[[i]]), Np = bitcoin_Np))), se = TRUE)
  })
})
r.if1 <- data.frame(logLik = L.if1[, 1], logLik_se = L.if1[, 2],
                    t(sapply(if1, coef)))
if (run_level > 1)
  write.table(r.if1, file = "bitcoin_params.csv",
              append = TRUE, col.names = FALSE, row.names = FALSE)

pairs(~logLik + sigma_nu + mu_h + phi + sigma_eta,
      data = subset(r.if1, logLik > max(logLik) - 20))


### 6. Global Search Over a Box of Parameter Values

bitcoin_box <- rbind(
  sigma_nu  = c(0.005, 0.05),
  mu_h      = c(-1, 0),
  phi       = c(0.95, 0.99),
  sigma_eta = c(0.5, 1),
  G_0       = c(-2, 2),
  H_0       = c(-1, 1)
)

stew(file = paste0("box_eval_bitcoin_", run_level, ".rda"), {
  if.box <- foreach(i = 1:bitcoin_Nreps_global,
                    .packages = "pomp", .combine = c) %dopar% 
    mif2(if1[[1]],
         params = apply(bitcoin_box, 1, function(x) runif(1, x)))
  L.box <- foreach(i = 1:bitcoin_Nreps_global,
                   .packages = "pomp", .combine = rbind) %dopar% {
                     logmeanexp(replicate(bitcoin_Nreps_eval, logLik(pfilter(
                       bitcoin.filt, params = coef(if.box[[i]]), Np = bitcoin_Np))),
                       se = TRUE)
                   }
})
# Optionally record elapsed time:
timing.box <- .system.time["elapsed"]

r.box <- data.frame(logLik = L.box[, 1], logLik_se = L.box[, 2],
                    t(sapply(if.box, coef)))
if (run_level > 1)
  write.table(r.box, file = "bitcoin_params.csv",
              append = TRUE, col.names = FALSE, row.names = FALSE)
summary(r.box$logLik, digits = 5)

pairs(~logLik + log(sigma_nu) + mu_h + phi + sigma_eta + H_0,
      data = subset(r.box, logLik > max(logLik) - 10))