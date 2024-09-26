# This R script shows bi-modal integration from respective score performance.
# This is essentially what Bayesian integration is doing, however, this model
# presumes we know the parameters of modality performance.
# The integrated perception is then visualised as a function of auditory
# and visual performance.
# The integrated performance is then tested against actual performance,
# using a simulated data set.
# The equation is then be implemented into hypothesis testing frameworks.
# This equation has a wide range of applications, and its intended use 
# relates to perception within space. 


#Equation from Deneve & Pouget, 2004
https://doi.org/10.1016/j.jphysparis.2004.03.011


library(tidyverse)


# Set seed for reproducibility
set.seed(123)

# Generate example data
n <- 100  # Number of data points

# Simulated estimates from visual and auditory modalities
x_vis <- rnorm(n, mean = 50, sd = 5)  # Visual estimates (mean = 50, sd = 5)
x_aud <- rnorm(n, mean = 50, sd = 7)  # Auditory estimates (mean = 50, sd = 7)

# Variances (σ^2) for visual and auditory modalities
sigma_vis2 <- 5^2  # Variance of visual estimate
sigma_aud2 <- 7^2  # Variance of auditory estimate

# Calculate the combined estimate (x̂_bim)
# (Deneve & Pouget, 2004)
x_bim <- (1/sigma_vis2) / (1/sigma_vis2 + 1/sigma_aud2) * x_vis +
  (1/sigma_aud2) / (1/sigma_vis2 + 1/sigma_aud2) * x_aud

# Generate actual AV data to be similar to x_bim
# Centered around x_bim with a similar standard deviation
noise_sd <- 2  # Lower standard deviation for reduced noise
x_av_actual <- rnorm(n, mean = x_bim, sd = noise_sd)

# Combine all data into a data frame
data <- data.frame(x_vis, x_aud, x_bim, x_av_actual)

# Display the first few results
head(data)

# visualising xbim --------------------------------------------------------

#visualising xbim:

# Load necessary library
library(lattice)
library(RColorBrewer)


# Number of samples
n <- 2000

# Generate visual and auditory estimates
x_vis <- rnorm(n, mean = 50, sd = 5)  # Visual estimates (mean = 50, sd = 5)
x_aud <- rnorm(n, mean = 50, sd = 7)  # Auditory estimates (mean = 50, sd = 7)

# Variances (σ^2) for visual and auditory modalities
sigma_vis2 <- 5^2  # Variance of visual estimate
sigma_aud2 <- 7^2  # Variance of auditory estimate

# Calculate the combined estimate (x̂_bim)
x_bim <- (1 / sigma_vis2) / (1 / sigma_vis2 + 1 / sigma_aud2) * x_vis +
  (1 / sigma_aud2) / (1 / sigma_vis2 + 1 / sigma_aud2) * x_aud

# Create a data frame for plotting
data_plot <- data.frame(
  x_vis = x_vis,
  x_aud = x_aud,
  x_bim = x_bim
)

# Create 3D scatter plot using lattice
cloud(x_bim ~ (1 / sigma_vis2) / (1 / sigma_vis2 + 1 / sigma_aud2) * x_vis +
        (1 / sigma_aud2) / (1 / sigma_vis2 + 1 / sigma_aud2) * x_aud, 
      data = data_plot,
      main = "",
      xlab = "(V)", ylab = "(A)", zlab = "(bim)",
      pch = 16,
      scales = list(arrows = FALSE, col = "black"),
      col = viridis::viridis(100)[as.integer(cut(x_bim, breaks = 100))])


# hypothesis testing ------------------------------------------------------

# Perform a linear regression between x_bim (predicted) and x_av_actual (actual)
model <- lm(x_av_actual ~ x_bim, data = data)

# Display the summary of the linear regression
summary(model)

# Plot the relationship with a regression line
library(ggplot2)

ggplot(data, aes(x = x_bim, y = x_av_actual)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_minimal() +
  labs(title = "Relationship Between Predicted and Actual AV Estimates",
       x = "Predicted AV (x_bim)",
       y = "Actual AV (x_av_actual)") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black")


# Combine the data into a long format for ANOVA and comparison
data_long <- data.frame(
  estimate = c(x_vis, x_aud, x_bim, x_av_actual),
  modality = rep(c("Visual", "Auditory", "Bimodal", "AV Actual"), each=n)
)

head(data_long)

# Perform ANOVA
anova_result <- aov(estimate ~ modality, data=data_long)

# Display the ANOVA results
summary(anova_result)

#perform MLM
view(data_long)
library(lme4)

MLM <- lmer(estimate ~ modality + (1|modality), data=data_long)
summary(MLM)

#perform t test

t_test_result <- t.test(x_bim, x_av_actual)

# Print t-test result
print(t_test_result)

