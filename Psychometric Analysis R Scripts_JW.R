# R Script for Psychometric Analysis of Adaptive Test Data (Kaplan)
 
# Install required packages
required_packages <- c("tidyverse", "reshape2", "mirt", "psych", "ltm", "ggplot2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(tidyverse)
library(reshape2)
library(mirt)
library(psych)
library(ltm)
library(ggplot2)

# Data loading and preparation
data <- read.csv("/Users/wjw/Simulated_Adaptive_Test_Data.csv", stringsAsFactors = FALSE)

# Dataset exploration
str(data)

cat("Number of rows:", nrow(data), "\n")
cat("Number of unique users:", length(unique(data$user_id)), "\n")
cat("Number of unique items:", length(unique(data$item_id)), "\n")

# Check for missing values
missing_vals <- colSums(is.na(data))
print(missing_vals)

# Initial data exploration
# Item type distribution
print(table(data$item_type))

# Section distribution
print(table(data$section))

# Skill distribution
print(table(data$skill))

# Difficulty distribution
print(summary(data$raw_difficulty))

# Ability distribution
print(summary(data$raw_ability_est))

# Item analysis and IRT modeling
# Filter to only include rows with scores
scored_data <- data %>% filter(!is.na(score))

# Create a user by item matrix (wide format)
standalone_items <- scored_data %>%
filter(item_type == "standalone") #focus on standalone items for simplicity

# Create wide format data
wide_data <- standalone_items %>%
dplyr::select(user_id, item_id, score) %>%
tidyr::spread(key = item_id, value = score)

# Check the dimensions
cat("Dimensions of wide_data:", dim(wide_data)[1], "users by", dim(wide_data)[2], "items\n")

# Remove the user_id column for IRT analysis
model_data <- wide_data[, -1]

# Only remove items with perfect scores or zeros
item_means <- colMeans(model_data, na.rm = TRUE)
extreme_items <- item_means == 0 | item_means == 1 | is.na(item_means)
model_data <- model_data[, !extreme_items]
cat("Remaining items after filtering:", ncol(model_data), "\n")

# Set model fitting options
mirt_control <- list(
convergence = 0.001,
maxiters = 1000
)

# Fit 1PL model
model_1pl <- mirt(model_data, model = 1, itemtype = "Rasch", 
verbose = TRUE, control = mirt_control)

# Fit 2PL model
model_2pl <- mirt(model_data, model = 1, itemtype = "2PL", 
verbose = TRUE, control = mirt_control)

# Check if models are created successfully
cat("\nModel objects created successfully?\n")
cat("1PL model:", ifelse(class(model_1pl)[1] == "SingleGroupClass", "Yes", "No"), "\n")
cat("2PL model:", ifelse(class(model_2pl)[1] == "SingleGroupClass", "Yes", "No"), "\n")

# Extract item parameters from 1PL model
params_1pl <- coef(model_1pl, simplify = TRUE)$items
cat("Parameters extracted from 1PL model. Dimensions:", dim(params_1pl)[1], "items by", dim(params_1pl)[2], "parameters\n")

# Extract 2PL parameters
params_2pl <- coef(model_2pl, simplify = TRUE)$items
cat("Parameters extracted from 2PL model. Dimensions:", dim(params_2pl)[1], "items by", dim(params_2pl)[2], "parameters\n")

# Calculate AIC and BIC for 1PL
AIC_1pl <- AIC(model_1pl)
BIC_1pl <- BIC(model_1pl)
logLik_1pl <- logLik(model_1pl)

# Calculate AIC and BIC for 2PL
AIC_2pl <- AIC(model_2pl)
BIC_2pl <- BIC(model_2pl)
logLik_2pl <- logLik(model_2pl)

# Compare models
cat("\nModel comparison (1PL vs 2PL):\n")
model_comparison <- anova(model_1pl, model_2pl)
print(model_comparison)

#1PL item analysis and calibration
# Calculate item difficulty parameters
item_params <- data.frame(params_1pl)
item_params$item_id <- rownames(params_1pl)
item_params$difficulty <- -item_params$d
print(summary(item_params$difficulty))
write.csv(item_params, "1pl_basic_parameters.csv", row.names=FALSE) # Save basic parameters

# Create difficulty categories and visualise distribution
# Calculate item difficulty categories
item_params$difficulty_category <- cut(item_params$difficulty, 
breaks=c(-Inf, -2, -1, 0, 1, 2, Inf),
labels=c("Very Easy", "Easy", "Medium Easy",
"Medium Hard", "Hard", "Very Hard"))
print(table(item_params$difficulty_category))

# Create difficulty distribution histogram
tryCatch({
pdf("1pl_difficulty_distribution.pdf", width=8, height=6)
hist(item_params$difficulty, breaks=20, 
col="skyblue", border="white",
main="Distribution of Item Difficulties (1PL Model)",
xlab="Difficulty Parameter (b)", ylab="Frequency")
abline(v=0, col="red", lty=2, lwd=2)  # Average ability line
abline(v=mean(item_params$difficulty), col="blue", lwd=2)  # Mean difficulty
legend("topright", c("Mean Difficulty", "Average Ability (θ=0)"),
col=c("blue", "red"), lty=c(1,2), lwd=2)
dev.off()
cat("Difficulty distribution plot created\n")
}, error = function(e) {
cat("Error creating difficulty distribution plot:", e$message, "\n")
})

# Sample Item Characteristic Curves
# Define ability range
theta <- seq(-4, 4, by=0.1)
# Select a few items with different difficulties
item_difficulties <- c(-2, -1, 0, 1, 2)  # Target difficulties
selected_items <- numeric(length(item_difficulties))
# Find items closest to target difficulties
for(i in 1:length(item_difficulties)) {
  idx <- which.min(abs(item_params$difficulty - item_difficulties[i]))
  selected_items[i] <- idx
}
# Create PNG file
png("icc_plot.png", width=1000, height=800, res=120)
plot(0, 0, type="n", xlim=c(-4, 4), ylim=c(0, 1),
     main="Item Characteristic Curves (1PL Model)",
     xlab="Ability (θ)", ylab="Probability of Correct Response")
# Add grid for readability
grid(lty=3, col="lightgray")
# Add lines for each selected item
colors <- c("red", "blue", "green", "purple", "orange")
for(i in 1:length(selected_items)) {
  idx <- selected_items[i]
  b <- item_params$difficulty[idx]
  p <- 1 / (1 + exp(-(theta - b)))
  lines(theta, p, col=colors[i], lwd=2)
  text(b, 0.5, paste("Item", item_params$item_id[idx]), pos=4, col=colors[i])
}
legend("bottomright", 
       legend=paste("b =", round(item_params$difficulty[selected_items], 2)),
       col=colors, lwd=2)
dev.off()
cat("ICC plot created\n")

# Calculate item fit statistics
tryCatch({
item_fit <- itemfit(model_1pl, fit_stats=c("X2", "G2", "infit"), na.rm=FALSE) # using fit statistics to work with missing data
# Add available statistics to the parameters
if("X2" %in% names(item_fit)) {
item_params$X2 <- item_fit$X2
}
if("p.X2" %in% names(item_fit)) {
item_params$p_X2 <- item_fit$p.X2
}
if("infit" %in% names(item_fit)) {
item_params$infit <- item_fit$infit
}
# Create a simplified misfit flag based on available statistics
item_params$misfit_flag <- "OK"
if("p.X2" %in% names(item_fit)) {
item_params$misfit_flag[item_fit$p.X2 < 0.05] <- "Potential misfit"
}
# Count misfitting items
misfit_count <- sum(item_params$misfit_flag != "OK")
cat("Number of misfitting items:", misfit_count, "\n")
write.csv(item_params, "1pl_parameters_with_fit.csv", row.names=FALSE) # Save parameters with fit statistics
}, error = function(e) {
cat("Error calculating fit statistics:", e$message, "\n")
cat("Proceeding without fit statistics...\n")
write.csv(item_params, "1pl_parameters_without_fit.csv", row.names=FALSE) # Save parameters without fit statistics
})
# Item infit analysis plot
tryCatch({
item_params$colored_flag <- ifelse(is.na(item_params$misfit_flag), "gray", 
ifelse(item_params$misfit_flag == "OK", "blue", "red"))

p <- plot_ly(data = item_params, x = ~difficulty, y = ~infit, type = 'scatter', mode = 'markers',
marker = list(size = 10, color = ~colored_flag), text = ~paste("Item ID:", item_id))
p <- layout(p, title = 'Interactive Item Fit Analysis (1PL Model)',
xaxis = list(title = 'Item Difficulty'),
yaxis = list(title = 'Infit Statistic'),
hovermode = "closest")
htmlwidgets::saveWidget(as_widget(p), "interactive_item_fit_plot.html")
cat("Interactive item fit analysis plot created and saved as HTML.\n")
}, error = function(e) {
cat("Error creating interactive item fit plot:", e$message, "\n")
})

# Gap Analysis
ability_ranges <- seq(-3, 3, by=0.5)
gap_analysis <- data.frame(
lower_bound = ability_ranges[-length(ability_ranges)],
upper_bound = ability_ranges[-1],
item_count = numeric(length(ability_ranges) - 1),
coverage = character(length(ability_ranges) - 1)
)
for(i in 1:nrow(gap_analysis)) {
count <- sum(item_params$difficulty >= gap_analysis$lower_bound[i] & 
item_params$difficulty < gap_analysis$upper_bound[i])
gap_analysis$item_count[i] <- count
if(count < 5) gap_analysis$coverage[i] <- "Critical gap"
else if(count < 10) gap_analysis$coverage[i] <- "Insufficient"
else if(count < 20) gap_analysis$coverage[i] <- "Adequate"
else gap_analysis$coverage[i] <- "Good"
}
print(gap_analysis)
write.csv(gap_analysis, "1pl_gap_analysis.csv", row.names=FALSE)

# Test information function
pdf("1pl_test_information_simple.pdf", width=8, height=6)
plot(model_1pl, type="info", main="Test Information Function (1PL Model)")
dev.off()
# Check if the file was created and its size
file.info("1pl_test_information_simple.pdf")$size

# Calibrated Item Bank
cat("1PL Item Calibration Summary:\n")
cat("Total items calibrated:", nrow(item_params), "\n")
cat("Difficulty range:", round(min(item_params$difficulty), 2), "to",
round(max(item_params$difficulty), 2), "\n")
cat("Mean difficulty:", round(mean(item_params$difficulty), 2), "\n")
cat("Standard deviation:", round(sd(item_params$difficulty), 2), "\n")
# Create ready-to-use item bank for adaptive testing
item_bank <- item_params %>%
dplyr::select(item_id, difficulty, difficulty_category) %>%
# Add a discrimination parameter (fixed at 1 for 1PL)
dplyr::mutate(discrimination = 1) %>%
dplyr::arrange(difficulty)
# Save the calibrated item bank
write.csv(item_bank, "1pl_calibrated_item_bank.csv", row.names=FALSE)
cat("\n1PL model calibration complete. Item bank saved to '1pl_calibrated_item_bank.csv'\n")

# Reliability Analysis
# Create a sequence of ability values
theta_values <- seq(-4, 4, by=0.1)
# Extract test information at each ability level
test_info <- testinfo(model_1pl, theta_values)
# Calculate standard errors at each ability level
standard_errors <- 1 / sqrt(test_info)
# Create a data frame for analysis and plotting
se_data <- data.frame(
theta = theta_values,
information = test_info,
se = standard_errors
)
# Summary statistics for standard errors
cat("Standard Error Summary Statistics:\n")
print(summary(se_data$se))
# Create SEM plot
png("standard_error_distribution.png", width=800, height=600, res=100)
plot(se_data$theta, se_data$se, type="l", col="blue", lwd=2,
main="Standard Error of Measurement Across Ability Levels",
xlab="Ability (θ)", ylab="Standard Error")
# Add reference lines
abline(h=0.2, col="green", lty=2)  # High precision threshold
abline(h=0.3, col="orange", lty=2) # Moderate precision threshold
# Add labels
legend("topleft",
legend=c("Standard Error", "High Precision (SE=0.2)", "Moderate Precision (SE=0.3)"),
col=c("blue", "green", "orange"), 
lty=c(1, 2, 2), 
lwd=c(2, 1, 1))
dev.off()

# Calculate conditional reliability at key ability levels
key_thetas <- c(-3, -2, -1, 0, 1, 2, 3)
conditional_rel <- data.frame(
theta = key_thetas,
information = testinfo(model_1pl, key_thetas)
)
# Apply reliability formula: rel = info/(info+1)
conditional_rel$reliability <- conditional_rel$information / (conditional_rel$information + 1)
conditional_rel$se <- 1 / sqrt(conditional_rel$information)
# Print conditional reliability results
cat("\nConditional Reliability at Key Ability Levels:\n")
print(conditional_rel[, c("theta", "reliability", "se")])
# Save the results to a CSV file
write.csv(conditional_rel, "conditional_reliability.csv", row.names=FALSE)

