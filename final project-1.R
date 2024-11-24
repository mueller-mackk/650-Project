####################################Step 1:Encoding and full model################################

#####goal: to check if we have multicolinearity issues
# install.packages("NHANES")
library("NHANES")
# install.packages("car")
library(car)

nrow(NHANES)

str(NHANES)
colSums(is.na(NHANES))
help(NHANES)

# Make SleepHrsNight categorical with 3 levels
NHANES$SleepHrsNight_cat <- cut(
  NHANES$SleepHrsNight,
  breaks = c(-Inf, 6, 8, Inf),
  labels = c("<6 hours", "6-8 hours", "8+ hours"),
  right = FALSE
)

# Make Depressed categorical with 4 levels
NHANES$Depressed_cat <- factor(
  NHANES$Depressed,
  levels = c("None", "Several", "Majority", "AlmostAll"),
  labels = c("1", "2", "3", "4")  # Ensure levels correspond to numeric representation
)

# Set the reference group for Depressed as "None"
NHANES$Depressed_cat <- relevel(NHANES$Depressed, ref = "None")

# All covariates 

###Age range

### "TotChol" aged 6 years or older. BMI aged 2 years or older.SleepTrouble aged 16 years and older.
### depression  aged 18 years or older. smoke100 aged 20 years or older. Physactivedays aged12 years or older.
###Diabetes aged 1 year or older.   "AlcoholYear" aged 18 years or older. Regular marij aged 18 to 59 years.
###HardDrugs aged 18 to 69 years. 


vars <- c("TotChol", "Age", "Gender", "Race1", "BMI", "SleepTrouble", 
          "Depressed_cat", "Smoke100", "PhysActiveDays", "Diabetes", "AlcoholYear",
          "RegularMarij", "HardDrugs", "Poverty")  # Include Poverty_ratio

# Remove missing values
NHANES_clean <- NHANES[complete.cases(NHANES[vars]), ]
nrow(NHANES_clean)


# Subset the data to include only participants aged 20 to 59
NHANES_clean_subset <- NHANES_clean[NHANES_clean$Age >= 20 & NHANES_clean$Age <= 59, ]
# Get the number of rows in the subset
nrow(NHANES_clean_subset)

# Creating Reference Groups
NHANES_clean_subset$Race1 <- relevel(NHANES_clean$Race1, ref = "White")
NHANES_clean_subset$Gender <- relevel(NHANES_clean$Gender, ref = "male")
NHANES_clean_subset$Smoke100 <- relevel(NHANES_clean$Smoke100, ref = "No")
NHANES_clean_subset$PhysActive <- relevel(NHANES_clean$PhysActive, ref = "No")
NHANES_clean_subset$Diabetes <- relevel(NHANES_clean$Diabetes, ref = "No")
NHANES_clean_subset$SleepTrouble <- relevel(NHANES_clean$SleepTrouble, ref = "No")
NHANES_clean_subset$RegularMarij <- relevel(NHANES_clean$RegularMarij, ref = "No")
NHANES_clean_subset$HardDrugs <- relevel(NHANES_clean$HardDrugs, ref = "No")

# Center Age & BMI
NHANES_clean_subset$Age_centered <- scale(NHANES_clean$Age, center = TRUE, scale = FALSE)
NHANES_clean_subset$BMI_centered <- scale(NHANES_clean$BMI, center = TRUE, scale = FALSE)

# Fit MLR model:
predicted_chol <- lm(
  TotChol ~ Age_centered + BMI_centered + Gender + Race1 + 
    Depressed_cat + Smoke100 + SleepTrouble + PhysActiveDays + 
    Diabetes + AlcoholYear + RegularMarij + HardDrugs + Poverty,  # Add Poverty_ratio
  data = NHANES_clean_subset
)

summary(predicted_chol)

# Checking VIF
vif_values <- vif(predicted_chol)
print(vif_values)

# partial regresion plot
# Install and load the 'car' package if you haven't already
# install.packages("car")
library(car)


# Fit the MLR model
predicted_chol <- lm(
  TotChol ~ Age_centered + BMI_centered + Gender + Race1 + 
    Depressed_cat + Smoke100 + SleepTrouble + PhysActiveDays + 
    Diabetes + AlcoholYear + RegularMarij + HardDrugs + Poverty,
  data = NHANES_clean_subset
)


### partial regression plot for linearity
# Create partial regression plots for continuous variables (Age_centered, BMI_centered, AlcoholYear, Poverty)
# The `avPlots()` function creates added variable plots (partial regression plots)
# linearity is valid
library(car)
avPlots(predicted_chol, terms = ~ Age_centered + BMI_centered + AlcoholYear + Poverty)
# linearity is valid base on plot


###  residuals vs. fitted values plot for constant variance
plot(predicted_chol, which = 1, main = "Residuals vs Fitted Values")
# constant variance assumption holds

###  note this is cross_sectional study, so we can assume indepence holds for this
### Dubrin-watson test for independence
#install.packages("lmtest")
library(lmtest)
# perform dw test 
dw_test_result <- dwtest(predicted_chol)
print(dw_test_result)


### Q-Q plot for checking normality note: By central limit theorem , the inference is valid regardless of normality
qqnorm(residuals(predicted_chol))
qqline(residuals(predicted_chol))
### we have few outliers , and tails looks suspecious

#
####################################Step 1:Encoding and full model################################



####################################Step 2:model selection################################

#####goal: to select the main effect from potnetial predictors
# Fit the full model (same as before)
full_model <- lm(
  TotChol ~ Age_centered + BMI_centered + Gender + Race1 + 
    Depressed_cat + Smoke100 + SleepTrouble + PhysActiveDays + 
    Diabetes + AlcoholYear + RegularMarij + HardDrugs + Poverty,  # Add Poverty_ratio
  data = NHANES_clean_subset
)

# Forward selection based on AIC
forward_model_aic <- step(
  lm(TotChol ~ 1, data = NHANES_clean_subset),    # Start with intercept-only model
  scope = list(lower = formula(TotChol ~ 1), upper = formula(full_model)),
  direction = "forward",
  criterion = "AIC"
)
# Extract final selected variables for forward selection based on AIC
forward_vars_aic <- names(coef(forward_model_aic))[-1]  # Remove intercept
print("Variables selected by forward selection (AIC):")
print(forward_vars_aic)

# Forward selection based on BIC
forward_model_bic <- step(
  lm(TotChol ~ 1, data =NHANES_clean_subset),    # Start with intercept-only model
  scope = list(lower = formula(TotChol ~ 1), upper = formula(full_model)),
  direction = "forward",
  criterion = "BIC"
)
# Extract final selected variables for forward selection based on BIC
forward_vars_bic <- names(coef(forward_model_bic))[-1]  # Remove intercept
print("Variables selected by forward selection (BIC):")
print(forward_vars_bic)

# Backward selection based on AIC
backward_model_aic <- step(
  full_model,  # Start with the full model
  direction = "backward",
  criterion = "AIC"
)
# Extract final selected variables for backward selection based on AIC
backward_vars_aic <- names(coef(backward_model_aic))[-1]  # Remove intercept
print("Variables selected by backward selection (AIC):")
print(backward_vars_aic)

# Backward selection based on BIC
backward_model_bic <- step(
  full_model,  # Start with the full model
  direction = "backward",
  criterion = "BIC"
)
# Extract final selected variables for backward selection based on BIC
backward_vars_bic <- names(coef(backward_model_bic))[-1]  # Remove intercept
print("Variables selected by backward selection (BIC):")
print(backward_vars_bic)

olsrr::ols_step_forward_p(lm(TotChol~Age_centered + BMI_centered + Gender + Race1 + 
                               Depressed_cat + Smoke100 + SleepTrouble + PhysActiveDays + 
                               Diabetes + AlcoholYear + RegularMarij + HardDrugs + Poverty,data=NHANES_clean_subset),penter=0.1,details=F)





####################################Step 2:model selection################################








####################################Step 3:model assumptions for the final model################################
# Fit the linear regression model using the selected variables from backward selection (BIC)

#####goal: to check model assumptions for our final linear model
backward_model_bic_selected <- lm(
  TotChol ~ Age_centered + BMI_centered + Race1 + 
    Smoke100 + Diabetes + AlcoholYear + RegularMarij + HardDrugs, 
  data = NHANES_clean_subset
)

# Summary of the model to get the coefficients, significance, etc.
summary(backward_model_bic_selected)

# check if we have multicolinearity
vif(backward_model_bic_selected)


### partial regression plot for linearity
# Create partial regression plots for continuous variables (Age_centered, BMI_centered, AlcoholYear, Poverty)
# The `avPlots()` function creates added variable plots (partial regression plots)
# linearity is valid
avPlots(backward_model_bic_selected, terms = ~ Age_centered + BMI_centered + AlcoholYear)
# linearity is valid base on plot


###  residuals vs. fitted values plot for constant variance
plot(backward_model_bic_selected, which = 1, main = "Residuals vs Fitted Values")
# constant variance assumption holds

###  note this is cross_sectional study, so we can assume indepence holds for this
### Dubrin-watson test for independence
#install.packages("lmtest")
library(lmtest)
# perform dw test 
dw_test_result_final <- dwtest(backward_model_bic_selected)
print(dw_test_result_final)


### Q-Q plot for checking normality note: By central limit theorem , the inference is valid regardless of normality
qqnorm(residuals(backward_model_bic_selected))
qqline(residuals(backward_model_bic_selected))
### we have few outliers , and tails looks suspecious

####################################Step 3:model assumptions for the final model################################




####################################Step 4:effect modification################################
# Fit the interaction model with HardDrugs and RegularMarij interacting with Race1


###is there  an effect modification association among all potential health factors with age and race
# Fit the interaction model
interaction_model <- lm(
  TotChol ~ Age_centered * Smoke100 + 
    Age_centered * Diabetes + 
    Age_centered * AlcoholYear + 
    Age_centered * RegularMarij + 
    Age_centered * HardDrugs +
    BMI_centered + Race1,
  data = NHANES_clean_subset
)

# Summary of the model
summary(interaction_model)

##### general linear hypothethsis test if  for 

###### same as home work , test for 5 betas euqal to 0 

####################################Step 4:effect modification################################






####################################Step 5:Outliers/Influence Diagnostics################################
#a.Calculate standardized residuals(Data points above ±3, these are usually considered outliers)
standardized_residuals <- rstandard(backward_model_bic_selected)

# Identify residuals exceeding ±3
outliers <- which(abs(standardized_residuals) > 3)
print("Potential Outliers:")
print(outliers)

# Plot standardized residuals
plot(standardized_residuals, main = "Standardized Residuals", 
     xlab = "Observation Index", ylab = "Standardized Residuals")
abline(h = c(-3, 3), col = "red", lty = 2)
#---------------------------------------------------------------------------------------
#b.# Calculate Cook's Distance(Used to detect the effect of data points on the overall model fit.)
cooks_distances <- cooks.distance(backward_model_bic_selected)

# Determine threshold
threshold <- 4 / nrow(NHANES_clean_subset)

# Identify influential points
influential_points <- which(cooks_distances > threshold)
print("Influential Points (Cook's Distance):")
print(influential_points)

# Plot Cook's Distance
plot(cooks_distances, main = "Cook's Distance", 
     xlab = "Observation Index", ylab = "Cook's Distance")
abline(h = threshold, col = "red", lty = 2)
#---------------------------------------------------------------------------------------
#c.# Calculate leverage values(The effect of each data point on the predicted value)
leverage_values <- hatvalues(backward_model_bic_selected)

# Determine threshold
p <- length(coef(backward_model_bic_selected)) - 1  # Number of predictors
n <- nrow(NHANES_clean_subset)  # Sample size
leverage_threshold <- 2 * (p + 1) / n

# Identify high-leverage points
high_leverage_points <- which(leverage_values > leverage_threshold)
print("High Leverage Points:")
print(high_leverage_points)

# Plot leverage values
plot(leverage_values, main = "Leverage Values", 
     xlab = "Observation Index", ylab = "Leverage")
abline(h = leverage_threshold, col = "blue", lty = 2)
#---------------------------------------------------------------------------------------
#d.# Generate an influence plot(Visualization of outliers and high leverage points)
library(car)
influencePlot(backward_model_bic_selected, id.method = "identify",
              main = "Influence Plot", sub = "Circle size is proportional to Cook's Distance")
####################################Step 5:Outliers/Influence Diagnostics################################
