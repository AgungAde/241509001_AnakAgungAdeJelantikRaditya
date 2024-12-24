# Name: Anak Agung Ade Jelantik Raditya / NIM : 2415091001 / Class: SI 1 IKI

# Simple Linear Regression Analysis: Study Hours and Exam Scores



# Dataset Source: Kaggle - Contoh 50 data siswa, jumlah jam belajar per minggu dan nilai ujian matematika
# In the first part, I load the CSV data which contains information on study hours and exam scores of students.
data <- read.csv("C:/Users/62877/Downloads/jam_belajar_nilai_ujian.csv")

# Checking the first few rows of the dataset to ensure it is loaded correctly
# This is useful to see the structure of the data and make sure the columns being used are correct.
head(data)

# Data Visualization - Scatter plot to check the relationship between Study Hours and Exam Scores
# Here, I create a plot to show the relationship between the number of study hours (X) and exam scores (Y),
# to see if any pattern can be identified.
ggplot(data, aes(x = Jam_Belajar, y = Nilai_Ujian)) +
  geom_point(color = "blue") +
  labs(title = "Relationship Between Study Hours and Exam Scores",
       x = "Study Hours (hours)",
       y = "Exam Scores")

# Step 3: Performing Linear Regression
# This regression model is used to understand the mathematical relationship between study hours and exam scores.
# I use the lm() function to create a regression model with Study Hours as the predictor and Exam Scores as the response.
model_regression <- lm(Nilai_Ujian ~ Jam_Belajar, data = data)

# Step 4: Assumption Tests for the Regression Model

## 4.1 Linearity Check - Creating a plot of data and regression line
# In this part, I create a plot that visualizes whether the data follows a linear pattern.
# If the regression line (red line) fits the data well, then the linearity assumption is valid.
plot(data$Jam_Belajar, data$Nilai_Ujian, 
     main = "Linearity Check", 
     xlab = "Study Hours (hours)", ylab = "Exam Scores")
abline(model_regression, col = "red")

## 4.2 Normality Check for Residuals - Histogram and Shapiro-Wilk Test for residual normality
# Here, I check the distribution of residuals from the regression model using a histogram and Shapiro-Wilk test.
# Normality of residuals is crucial for ensuring the validity of the regression model.
residuals <- residuals(model_regression)
hist(residuals, breaks = 10, main = "Residual Histogram", xlab = "Residual")
shapiro.test(residuals) # Shapiro-Wilk test to check if residuals are normally distributed

## 4.3 Homoskedasticity Check - Plotting residuals vs fitted values
# Here, I check if the variance of residuals is stable (homoskedasticity) by plotting residuals against the predicted values.
# If the residuals are randomly scattered around 0, then the homoskedasticity assumption is valid.
plot(model_regression$fitted.values, residuals, 
     main = "Residual vs Fitted Values", 
     xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Step 5: Model Analysis - Model Summary
# In this part, I call the summary() function to get detailed results of the regression model,
# such as the regression coefficients (intercept and slope), p-value, and R-squared to evaluate the model quality.
summary(model_regression)

# Step 6: Visualizing Regression Results
# Here, I create a scatter plot with a regression line to further visualize how the regression model predicts exam scores based on study hours.
scatter_plot <- ggplot(data, aes(x = Jam_Belajar, y = Nilai_Ujian)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot with Regression Line", 
       x = "Study Hours (hours)", 
       y = "Exam Scores")
print(scatter_plot)

# Step 7: Interpretation
# 1. Linearity:
#    - By looking at the scatter plot and the regression line, we can determine if the relationship between study hours and exam scores is linear.
#    - If the data points follow the straight line well, then the relationship is linear.

# 2. Model Summary:
#    - **Intercept (β₀)**: This value represents the predicted exam score when study hours = 0. 
#      If the intercept is 50, then the predicted exam score without studying is 50.
#    - **Slope (β₁)**: This value indicates the change in exam scores for every additional study hour. 
#      For example, if the slope is 5, then every additional hour of study increases the exam score by 5 points.
#    - **R-squared (R²)**: R² indicates how well the model explains the variation in exam scores. 
#      A higher R² means the regression model explains the data better.
#    - **p-value**: The p-value tests the significance of the relationship between study hours and exam scores. If p-value < 0.05, then study hours significantly affect exam scores.

# 3. Assumption Checks:
#    - **Linearity**: Checked using the scatter plot and the regression line.
#    - **Normality**: Checked using the Shapiro-Wilk test. If p-value > 0.05, residuals are normally distributed.
#    - **Homoskedasticity**: Checked using the residual vs fitted plot. Residuals that are randomly scattered around 0 support the homoskedasticity assumption.

# 4. Model Fit and Predictions:
#    - If the assumptions are valid and the p-value shows significance, the regression model can be used to predict exam scores based on study hours.
#      For example, if the slope is 5, we can predict that each additional hour of study increases the exam score by 5 points.

# 5. Conclusion:
#    - Based on the regression results and assumption checks, we can conclude that study hours significantly influence exam scores.
#      The model suggests that the more time spent studying, the higher the exam score will be.
