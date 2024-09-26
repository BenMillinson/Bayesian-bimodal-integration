# This R script covers bayesian bi-modal (audio-visual) integration using rstan.
# This code includes a simplistic combination approach:
# The model assumes that both visual and auditory inputs contribute equally to 
# the observed data (x_obs). It defines the likelihood using the sum of the means 
# from the visual (mu_vis) and auditory (mu_aud) distributions, 
# with a common standard deviation (sigma).
# The visual and auditory contributions are combined simply by adding their means, 
# without considering their uncertainties (e.g., how noisy or reliable each input is)
# Code includes precision weighted approach:
# A precision-weighted approach is used in models to optimally combine multiple
# sources of information (e.g., auditory and visual inputs) based on the reliability 
# or "precision" of each source. Precision refers to the inverse of variance, 
# so sources with higher precision (i.e., lower variance or more reliable) should 
# contribute more to the final estimate or decision.

library(rstan)
library(ggplot2)
library(loo)

# simplistic combination  -------------------------------------------------------------
#P(x|A,V) model

# Define Stan model code as a character string
# where the model and priors are defined
stan_code <- "
data {
  int<lower=0> N;             // Number of data points
  vector[N] x_obs;            // Observed data
  vector[N] x_vis;            // Visual input
  vector[N] x_aud;            // Auditory input
  real<lower=0> sigma_vis;    // Standard deviation for visual input
  real<lower=0> sigma_aud;    // Standard deviation for auditory input
}

parameters {
  real mu_vis;                // Mean of the visual input distribution
  real mu_aud;                // Mean of the auditory input distribution
  real<lower=0> sigma;        // Common standard deviation for the combined model
}

model {
  // Define likelihood
  x_obs ~ normal(mu_vis + mu_aud, sigma);
  
  // Priors
  mu_vis ~ normal(0, 1);
  mu_aud ~ normal(0, 1);
  sigma ~ normal(0, 1);
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(x_obs[i] | mu_vis + mu_aud, sigma);
  }
}
"

# Save the Stan model code to a file
writeLines(stan_code, con = "model.stan")

# Example data creation for illustration (you need to use actual data)
# Ensure data is a data.frame and contains necessary columns
# Here I create mock data for demonstration:
set.seed(123)
N <- 100  # Number of data points
data <- data.frame(
  x_obs = rnorm(N, mean = 5, sd = 2),
  x_vis = rnorm(N, mean = 3, sd = 1),
  x_aud = rnorm(N, mean = 4, sd = 1.5)
)

# Ensure columns are numeric and there are no missing values
data$x_obs <- as.numeric(data$x_obs)
data$x_vis <- as.numeric(data$x_vis)
data$x_aud <- as.numeric(data$x_aud)

# Remove rows with missing values
data <- na.omit(data)

# Prepare data for Stan
data_s <- list(
  N = nrow(data),
  x_obs = data$x_obs,
  x_vis = data$x_vis,
  x_aud = data$x_aud,
  sigma_vis = sd(data$x_vis),
  sigma_aud = sd(data$x_aud)
)

# Compile the Stan model
stan_model_fits <- stan_model(file = "model.stan")

# Fit the Stan model
# For a faster example, we use iter = 1000 and chains = 2, but this can be adjusted
fit <- sampling(stan_model_fits, data = data_s, iter = 1000, chains = 2)

# Print summary of the fit
print(fit)


#------cross validation

# Extract log-likelihood values
log_lik <- extract(fit)$log_lik

# Compute LOO
loo_result <- loo(log_lik)

# Print LOO results
print(loo_result)

# Plot posterior distributions for parameters
post_samples <- extract(fit)
combined_samples <- post_samples$mu_vis + post_samples$mu_aud

# Convert to data frame
combined_df_stan <- data.frame(Sample = combined_samples)

# Plot the combined distribution
ggplot(combined_df_stan, aes(x = Sample)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Combined Posterior Distribution (Stan)",
       x = "Value",
       y = "Density") +
  theme_minimal()

# Print model summary
print(summary(fit))


#summarise posterior


# Calculate mean and median
mean_mu_vis <- mean(post_samples$mu_vis)
median_mu_vis <- median(post_samples$mu_vis)

mean_mu_aud <- mean(post_samples$mu_aud)
median_mu_aud <- median(post_samples$mu_aud)

# Print results
cat("Mean of mu_vis:", mean_mu_vis, "\n")
cat("Median of mu_vis:", median_mu_vis, "\n")
cat("Mean of mu_aud:", mean_mu_aud, "\n")
cat("Median of mu_aud:", median_mu_aud, "\n")


# Simulate posterior predictive data
sim_data <- rnorm(nrow(data), mean = mean_mu_vis + mean_mu_aud, sd = sd(post_samples$sigma))

# Compare with observed data
ggplot() +
  geom_density(aes(x = data$x_obs), fill = "blue", alpha = 0.5) +
  geom_density(aes(x = sim_data), fill = "red", alpha = 0.5) +
  labs(title = "Posterior Predictive Check",
       x = "Value",
       y = "Density") +
  theme_minimal()


#-------hypothesis testing


# Check if mu_vis is significantly different from zero
p_val_mu_vis <- mean(post_samples$mu_vis > 0)  # One-sided test
cat("P-value for mu_vis > 0:", p_val_mu_vis, "\n")


# precision-weighted combination model ------------------------------------


# Simulate some data for demonstration
set.seed(123)
N <- 100
x_vis <- rnorm(N, mean = 5, sd = 2)  # Simulated visual input
x_aud <- rnorm(N, mean = 7, sd = 3)  # Simulated auditory input

# Precision values (inverse of variance, which is 1 / variance)
precision_vis <- 1 / var(x_vis)
precision_aud <- 1 / var(x_aud)

# Prepare data for Stan
data_stan <- list(
  N = N,
  x_vis = x_vis,
  x_aud = x_aud,
  precision_vis = precision_vis,
  precision_aud = precision_aud
)

# Save the Stan model to a file 

stan_code <- "
data {
  int<lower=0> N;            // Number of data points
  vector[N] x_vis;           // Visual input
  vector[N] x_aud;           // Auditory input
  real precision_vis;        // Precision of visual input (inverse of variance)
  real precision_aud;        // Precision of auditory input (inverse of variance)
}

parameters {
  real mu;                   // Mean of the combined distribution
  real<lower=0> sigma;       // Standard deviation of the combined distribution
}

model {
  // Priors for the parameters
  mu ~ normal(0, 10);        // Prior for the mean (adjust as needed)
  sigma ~ normal(0, 10);     // Prior for the standard deviation (adjust as needed)
  
  // Define the combined mean using precision-based weighting
  vector[N] x_bim;
  x_bim = (precision_vis / (precision_vis + precision_aud)) * x_vis +
          (precision_aud / (precision_vis + precision_aud)) * x_aud;
  
  // Likelihood: the visual and auditory inputs follow a normal distribution
  x_vis ~ normal(x_bim, sigma);  // Replace mu with x_bim to use the precision-weighted mean
  x_aud ~ normal(x_bim, sigma);  // This also applies for the auditory input
}

generated quantities {
  // Compute log likelihood for LOO-CV
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(x_vis[n] | mu, sigma) + normal_lpdf(x_aud[n] | mu, sigma);
  }
}
"

# Save the Stan model code to a file
writeLines(stan_code, con = "model_stan")

# Compile the Stan model
stan_model <- stan_model(file = 'model_stan')

# Fit the Stan model (you can increase iterations and chains for actual inference)
fit <- sampling(stan_model, data = data_stan, iter = 1000, chains = 2)

# Extract log-likelihood values from generated quantities for LOO
log_lik <- extract_log_lik(fit)

# Calculate LOO
loo_result <- loo(log_lik)

# Print LOO results
print(loo_result)

# Extract posterior samples from the fitted model
post_samples <- extract(fit)

# Combine the posterior samples using precision-weighted means
combined_samples <- (data_stan$precision_vis / (data_stan$precision_vis + data_stan$precision_aud)) * post_samples$mu +
  (data_stan$precision_aud / (data_stan$precision_vis + data_stan$precision_aud)) * post_samples$mu

# Convert to data frame for plotting
combined_df_stan <- data.frame(Sample = combined_samples)

# Plot the combined distribution
ggplot(combined_df_stan, aes(x = Sample)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Combined Audio-Visual Posterior Distribution (Stan)",
       x = "Value",
       y = "Density") +
  theme_minimal()

