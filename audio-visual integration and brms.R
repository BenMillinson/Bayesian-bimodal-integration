# This R script shows bi-modal integration (in this case audio-visual) using brms,
# a powerful package for computing bayesian regression
# This script goes from the equation shown in the "conceptualising bi-modal
# integration" script and implements it into a bayesian regression framework.
# This is then compared to a more 'real' bayesian framework with similar conceptualisation,
# but different assumptions. These are further discussed in the last section.



# step 1: set up environment ----------------------------------------------


# Load necessary libraries
library(brms)
library(ggplot2)

# Set a seed for reproducibility
set.seed(123)

# Simulate some experimental data
N <- 500 # Number of trials
sigma_vis <- 1.5 # Variance of the visual stimulus
sigma_aud <- 1.0 # Variance of the auditory stimulus

# Generate the true stimulus values (e.g., positions or intensities)
x_true <- rnorm(N, mean = 0, sd = 1)

# Generate noisy sensory inputs
x_vis <- x_true + rnorm(N, mean = 0, sd = sigma_vis)
x_aud <- x_true + rnorm(N, mean = 0, sd = sigma_aud)

# Simulated observed responses (using the OI model to generate participant responses)
# Assume participants are performing optimal integration
x_obs <- (1/sigma_vis^2) / (1/sigma_vis^2 + 1/sigma_aud^2) * x_vis +
  (1/sigma_aud^2) / (1/sigma_vis^2 + 1/sigma_aud^2) * x_aud +
  rnorm(N, mean = 0, sd = 0.2) # Small additional noise

# Data frame for easier handling
data <- data.frame(x_true, x_vis, x_aud, x_obs)
data


# step 2: implement a bayesian framework ----------------------------------

x_obs <- (1/sigma_vis^2) / (1/sigma_vis^2 + 1/sigma_aud^2) * x_vis +
  (1/sigma_aud^2) / (1/sigma_vis^2 + 1/sigma_aud^2) * x_aud +
  rnorm(N, mean = 0, sd = 0.2) # Small additional noise


# Fit the Bayesian linear regression model using brms
# Specify the formula for the linear regression model
formula_oi <- bf(x_obs ~ x_vis + x_aud)

# Define priors
priors_oi <- c(
  prior(normal(0, 1), class = "b"), # Prior for the coefficients
  prior(normal(0, 1), class = "Intercept") # Prior for the intercept
)

# Fit the model using brms
fit_oi <- brm(formula_oi, data = data, 
              prior = priors_oi,
              chains = 4, cores = 4, iter = 2000)

# Print a summary of the model
print(summary(fit_oi))

# Summary of the model
summary(fit_oi)
plot(fit_oi)


# general baysian model (bmrs) ---------------------------------------------------

# Define priors for the coefficients, intercept, and the random effects (if any)
priors_gb <- c(
  prior(normal(0, 1), class = "b"), # Prior for the coefficients
  prior(normal(0, 1), class = "Intercept") # Prior for the intercept
)

# Fit the model using brms
fit_gb <- brm(
  x_obs ~ x_vis + x_aud, 
  data = data, 
  prior = priors_gb,
  chains = 4, 
  cores = 4, 
  iter = 2000
)

# Summary of the model
summary(fit_gb)
plot(fit_gb)


# step 4: model comparison ------------------------------------------------

# Perform LOO cross-validation
loo_oi <- loo(fit_oi)
loo_gb <- loo(fit_gb)

# Compare models
loo_compare(loo_oi, loo_gb)


# visualise models --------------------------------------------------------

# Extract fitted values from both models
pred_oi <- fitted(fit_oi)
pred_gb <- fitted(fit_gb)

# Extract the mean predictions
data$pred_oi <- pred_oi[, "Estimate"]
data$pred_gb <- pred_gb[, "Estimate"]

# Plot observed vs. predicted for both models
ggplot(data, aes(x = x_obs)) +
  geom_point(aes(y = pred_oi), color = "blue", alpha = 0.9) +
  geom_point(aes(y = pred_gb), color = "red", alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(y = "Predicted Response", x = "Observed Response",
       title = "Observed vs. Predicted Responses",
       subtitle = "Blue: OI Model, Red: GB Model") +
  theme_minimal()


# visualising explained variance in models ------------------------------------------

# Load necessary libraries
library(brms)
library(ggplot2)
library(dplyr)


# Extract posterior predictive means from the fitted models
posterior_predict_gb <- fitted(fit_gb, newdata = data, scale = "response")
posterior_predict_gb
posterior_predict_oi <- fitted(fit_oi, newdata = data, scale = "response")
fit_oi
posterior_predict_oi

# Extract the mean predictions from the posterior samples
mean_predict_gb <- apply(posterior_predict_gb, 1, mean)
mean_predict_oi <- apply(posterior_predict_oi, 1, mean)

# Create a data frame for visualization
prediction_data <- data.frame(
  Observed = data$x_obs,
  GB_Prediction = mean_predict_gb,
  OI_Prediction = mean_predict_oi
)

# Fit a linear model to compare predictions from the two models
lm_model <- lm(OI_Prediction ~ GB_Prediction, data = prediction_data)
summary(lm_model)

# Calculate Mean Squared Error (MSE) between the two model predictions
mse <- mean((prediction_data$GB_Prediction - prediction_data$OI_Prediction)^2)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Print MSE and RMSE
print(paste("Mean Squared Error (MSE):", mse))
print(paste("Root Mean Squared Error (RMSE):", rmse))

# Plot the relationship between predictions from the two models
ggplot(prediction_data, aes(x = GB_Prediction, y = OI_Prediction)) +
  geom_point(aes(color = Observed), alpha = 0.8) +
  labs(
    title = "Comparison of Model Predictions",
    x = "GB Model Predictions",
    y = "OI Model Predictions",
    color = "Observed Values"
  ) +
  theme_minimal() +
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # Perfect prediction line
  #geom_smooth(method = "lm", se = FALSE, color = "blue") + # Linear model fit
  scale_color_viridis_c() # Use Viridis color scale for observed values



# model differences (theory) --------------------------------------------------


Reasons to Use the OI Model in Bayesian Analysis:
  
  Specific Hypothesis Testing:
  The OI model is based on a well-defined theoretical framework for optimal sensory integration. If your hypothesis is that sensory integration follows this optimal rule, directly implementing this model can provide clear insights into the validity of this hypothesis.
Precision Integration:
  The model uses known variances to weight the sensory inputs, which directly aligns with how precision (or inverse variance) influences integration. This can be particularly useful if you have good estimates of sensory precision and want to test how well this model fits the data.
Simpler Model:
  The OI model is relatively straightforward and provides a clear, interpretable framework for combining sensory information. It might be more parsimonious compared to more complex Bayesian models.
Focused Analysis:
  If you are specifically interested in how sensory inputs are combined optimally, the OI model directly addresses this question without introducing additional complexity.



Reasons to Use a GB Model:
  
  Flexibility:
  The GB model does not assume a specific integration mechanism. Instead, it models the relationship between inputs and outputs using flexible priors and can adapt to the data without imposing a specific theory of integration.
Model Exploration:
  If you are unsure about the specific way inputs are combined or if you want to explore different models, the GB model allows for testing a range of hypotheses about the relationships between sensory inputs and responses.
Complexity and Context:
  In some cases, the data may suggest that a more complex or different integration strategy is at play. The GB model can accommodate a variety of relationships and interactions between inputs.
Broader Hypotheses:
  The GB model is useful when you want to test more general hypotheses or when the integration mechanism is not well-defined. It provides a more general framework for understanding how inputs are related to the output.


