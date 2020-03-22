# Data Preprocessing

dataset = read.csv('50_Startups.csv')

# Categorical Data
dataset$State = factor(dataset$State,
                 levels = c('New York', 'California', 'Florida'),
                 labels = c(1, 2, 3))


library(caTools)
set.seed(123)

split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Multiple Linear Regression
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Test Set results
y_pred = predict(regressor, newdata = test_set)

# Removing independent variables with less significance i.e. p_value > 0.05
best_regressor = lm(formula = Profit ~ R.D.Spend,
               data = training_set)

# Test Set Results on only one Independent Variable
y_best_pred = predict(best_regressor, newdata = test_set)
