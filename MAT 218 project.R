#Cleaning data
library(dplyr)
library(tidyr)

# Check for unusual non-numeric entries in the ratings and no_of_ratings
unique(amazon_data$ratings[!grepl("^\\d+\\.?\\d*$", amazon_data$ratings)])
unique(amazon_data$no_of_ratings[!grepl("^\\d+$", gsub(",", "", amazon_data$no_of_ratings))])

# Assuming the fields are generally numeric with commas
# Convert ratings and no_of_ratings to numeric, handle commas and check for other non-numeric values
amazon_data$ratings <- as.numeric(gsub(",", "", amazon_data$ratings))
amazon_data$no_of_ratings <- as.numeric(gsub(",", "", amazon_data$no_of_ratings))

# Convert prices to numeric, remove '₹' and commas
amazon_data$discount_price <- as.numeric(gsub("[₹,]", "", amazon_data$discount_price))
amazon_data$actual_price <- as.numeric(gsub("[₹,]", "", amazon_data$actual_price))

# Remove rows with missing values in critical columns
clean_data <- amazon_data %>%
  filter(!is.na(ratings) & !is.na(no_of_ratings) & !is.na(discount_price) & !is.na(actual_price))

# Save the cleaned data
write.csv(clean_data, "clean_amazon_data.csv", row.names = FALSE)


#Vizualizations
library(ggplot2)

# Boxplot of discount prices grouped by ratings
ggplot(clean_data, aes(x=factor(ratings), y=discount_price, group=ratings)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Product Ratings",
       x = "Ratings",
       y = "Discount Price",
       caption = "Data from Amazon") +
  theme_minimal()



library(corrplot)

# Calculating correlation matrix
numerical_data <- clean_data[, c("ratings", "no_of_ratings", "discount_price", "actual_price")]
cor_matrix <- cor(numerical_data, use = "complete.obs")

# Correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 25,
         title = "
         Correlation Heatmap of Numerical Features",
         addCoef.col = "black") # Add correlation coefficients to the plot



library(ggplot2)

# Scatter plot of Number of Ratings vs. Actual Price
ggplot(clean_data, aes(x=no_of_ratings, y=actual_price, color=ratings)) +
  geom_point(alpha=0.5) + # Adjust point transparency for better visibility if there are many points
  scale_color_gradient(low = "blue", high = "red") + # Colors range from blue (low rating) to red (high rating)
  labs(title = "Number of Ratings vs. Actual Price by Ratings",
       x = "Number of Ratings",
       y = "Actual Price",
       color = "Ratings") +
  theme_minimal() +
  theme(legend.position = "right")


# Regression Models
library(car)  # for vif function

# Assume 'clean_data' is already loaded
model_data <- clean_data[, c("ratings", "no_of_ratings", "discount_price", "actual_price")]

# Checking for multicollinearity
vif_model <- lm(actual_price ~ ratings + no_of_ratings + discount_price, data = model_data)
vif(vif_model)


library(MASS)  # for boxcox function

# Performing Box-Cox Transformation on the dependent variable
bc_out <- boxcox(actual_price ~ ratings + no_of_ratings + discount_price, data = model_data, lambda = seq(-2, 2, by=0.1))

# Find the lambda that maximizes the log-likelihood
lambda_opt <- bc_out$x[which.max(bc_out$y)]
cat("Optimal lambda: ", lambda_opt, "\n")

# Transform the actual_price using the optimal lambda
model_data$actual_price_transformed <- ifelse(lambda_opt == 0, log(model_data$actual_price), (model_data$actual_price^lambda_opt - 1) / lambda_opt)

# Fit the linear regression model on transformed data if lambda was not zero, otherwise on log-transformed data
final_model <- lm(actual_price_transformed ~ ratings + no_of_ratings + discount_price, data = model_data)
summary(final_model)


# Plot diagnostics
par(mfrow=c(2,2))
plot(final_model)

