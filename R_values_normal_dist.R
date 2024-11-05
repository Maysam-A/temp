# Load necessary library
install.packages("ggplot2")
library("ggplot2")# Given values

######

sample_mean_r <- 116
sample_sd_r <- 77
n_set <- 30

# Calculate the standard error of the mean of means
standard_error_r <- sample_sd_r / sqrt(n_set)

# Calculate the 99% confidence interval using z-distribution
alpha <- 0.01
critical_value <- qnorm(1 - alpha/2)
margin_of_error_r <- critical_value * standard_error_r

confidence_interval_r <- c(sample_mean_r - margin_of_error_r, sample_mean_r + margin_of_error_r)

# Create a data frame for the normal distribution
x_r <- seq(sample_mean_r - 4 * standard_error_r, sample_mean_r + 4 * standard_error_r, length.out = 100)
y_r <- dnorm(x, mean = sample_mean_r, sd = standard_error_r)
data_r <- data.frame(x = x_r, y = y_r)

# Plot the sample distribution and confidence interval
ggplot(data_r, aes(x = x_r, y = y_r)) +
  geom_line() +
  geom_vline(xintercept = sample_mean_r, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = confidence_interval, linetype = "dashed", color = "blue") +
  labs(title = "Sample Distribution with 99% Confidence Interval",
       x = "Sample Mean",
       y = "Density") +
  theme_minimal()

######

sample_mean_g <- 113
sample_sd_g <- 75
n_set <- 30

# Calculate the standard error of the mean of means
standard_error_g <- sample_sd_g / sqrt(n_set)

# Calculate the 99% confidence interval using z-distribution
alpha <- 0.01
critical_value <- qnorm(1 - alpha/2)
margin_of_error_g <- critical_value * standard_error_g

confidence_interval_g <- c(sample_mean_g - margin_of_error_g, sample_mean_g + margin_of_error_g)

# Create a data frame for the normal distribution
x_g <- seq(sample_mean_g - 4 * standard_error_g, sample_mean_g + 4 * standard_error_g, length.out = 100)
y_g <- dnorm(x, mean = sample_mean_g, sd = standard_error_g)
data_g <- data.frame(x = x_g, y = y_g)

# Plot the sample distribution and confidence interval
ggplot(data_g, aes(x = x_g, y = y_g)) +
  geom_line() +
  geom_vline(xintercept = sample_mean_g, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = confidence_interval, linetype = "dashed", color = "blue") +
  labs(title = "Sample Distribution with 99% Confidence Interval",
       x = "Sample Mean",
       y = "Density") +
  theme_minimal()



######

sample_mean_b <- 110
sample_sd_b <- 83
n_set <- 30

# Calculate the standard error of the mean of means
standard_error_b <- sample_sd_b / sqrt(n_set)

# Calculate the 99% confidence interval using z-distribution
alpha <- 0.01
critical_value <- qnorm(1 - alpha/2)
margin_of_error_b <- critical_value * standard_error_b

confidence_interval_b <- c(sample_mean_b - margin_of_error_b, sample_mean_b + margin_of_error_b)

# Create a data frame for the normal distribution
x_b <- seq(sample_mean_b - 4 * standard_error_b, sample_mean_b + 4 * standard_error_b, length.out = 100)
y_b <- dnorm(x, mean = sample_mean_b, sd = standard_error_b)
data_b <- data.frame(x = x_b, y = y_b)

# Plot the sample distribution and confidence interval
ggplot(data_b, aes(x = x_b, y = y_b)) +
  geom_line() +
  geom_vline(xintercept = sample_mean_b, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = confidence_interval, linetype = "dashed", color = "blue") +
  labs(title = "Sample Distribution with 99% Confidence Interval",
       x = "Sample Mean",
       y = "Density") +
  theme_minimal()



######

sample_mean_RGB <- round((sample_mean_r+sample_mean_g+sample_mean_b)/3,digits = 0)
sample_sd_RGB <- round((sample_sd_r+sample_mean_g+sample_mean_b)/3,digits = 0)
n_set <- 30

# Calculate the standard error of the mean of means
standard_error_RGB <- sample_sd_RGB / sqrt(n_set)

# Calculate the 99% confidence interval using z-distribution
alpha <- 0.01
critical_value <- qnorm(1 - alpha/2)
margin_of_error_RGB <- critical_value * standard_error_RGB

confidence_interval_RGB <- c(sample_mean_RGB - margin_of_error_RGB, sample_mean_RGB + margin_of_error_RGB)

# Create a data frame for the normal distribution
x_RGB <- seq(sample_mean_RGB - 4 * standard_error_RGB, sample_mean_RGB + 4 * standard_error_RGB, length.out = 100)
y_RGB <- dnorm(x, mean = sample_mean_RGB, sd = standard_error_RGB)
data_RGB <- data.frame(x = x_RGB, y = y_RGB )

# Plot the sample distribution and confidence interval
ggplot(data_RGB, aes(x = x_RGB, y = y_RGB)) +
  geom_line() +
  geom_vline(xintercept = sample_mean_RGB, linetype = "dashed", color = "orange", size = 1) +
  geom_vline(xintercept = confidence_interval, linetype = "dashed", color = "cyan") +
  labs(title = "Sample Distribution with 99% Confidence Interval",
       x = "Sample Mean",
       y = "Density") +
  theme_minimal()

#######

######
sample_mean_RGB <- round((sample_mean_r+sample_mean_g+sample_mean_b)/3,digits = 0)
sample_sd_RGB <- round((sample_sd_r+sample_mean_g+sample_mean_b)/3,digits = 0)
n_set <- 30

# Calculate the standard error of the mean of means
standard_error_RGB <- sample_sd_RGB / sqrt(n_set)

# Calculate the 99% confidence interval using z-distribution
alpha <- 0.01
critical_value <- qnorm(1 - alpha/2)
margin_of_error_RGB <- critical_value * standard_error_RGB

confidence_interval_RGB <- c(sample_mean_RGB - margin_of_error_RGB, sample_mean_RGB + margin_of_error_RGB)

# Create a data frame for the normal distribution
x_RGB <- seq(sample_mean_RGB - 4 * standard_error_RGB, sample_mean_RGB + 4 * standard_error_RGB, length.out = 100)
y_RGB <- dnorm(x, mean = sample_mean_RGB, sd = standard_error_RGB)
data_RGB <- data.frame(x = x_RGB, y = y_RGB )

# Plot the sample distribution and confidence interval
base <-{
ggplot(data_RGB, aes(x = x_RGB, y = y_RGB)) +
  geom_line() +
  geom_vline(xintercept = sample_mean_r, linetype = "solid", color = "red", size = 2) +
  geom_vline(xintercept = sample_mean_g, linetype = "solid", color = "green", size = 2) +
  geom_vline(xintercept = sample_mean_b, linetype = "solid", color = "blue", size = 2) +
  geom_vline(xintercept = sample_mean_RGB, linetype = "dashed", color = "black", size = 2) +
  geom_vline(xintercept = confidence_interval_RGB, linetype = "dashed", color = "black") +
  geom_vline(xintercept = confidence_interval_r, linetype = "dashed", color = "green") +
  geom_vline(xintercept = confidence_interval_g, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = confidence_interval_b, linetype = "dashed", color = "red") +
  labs(title = "Sample Distribution with 99% Confidence Interval",
       x = "Sample Mean",
       y = "Density") +
  theme(
    title= element_text(size = 22),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    panel.grid.major= element_line(colour= "grey"),
    panel.grid.minor= element_line(colour= "grey"), 
    panel.background= element_rect(fill= "white"),
    legend.position = "bottom",
  )
}

#base +   scale_colour_manual(labels = c("Red", "Green", "Blue", "RGB"), values = c("red","green","blue","black"))+
#cols <- c("sample_mean_r" = "red", "sample_mean_g" = "green", "sample_mean_b" = "blue", "sample_mean_RGB0" = "black")
#base + scale_colour_manual(values = cols, aesthetics = cols)