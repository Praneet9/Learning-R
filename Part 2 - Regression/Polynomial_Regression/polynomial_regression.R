# Data Preprocessing

dataset = read.csv('Position_Salaries.csv')
dataset = dataset[, 2:3]

# Linear Regression
lin_reg = lm(formula = Salary ~ .,
             data = dataset)

dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4

# Polynomial Regression
poly_reg = lm(formula = Salary ~ .,
              data = dataset)

# Visualizing Linear Regression Results
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Level vs Salary (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')

# Visualizing Polynomial Regression Results
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Level vs Salary (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

# Predicting New result with Linear Regression
y_pred = predict(lin_reg, data.frame(Level = 6.5))

# Predicting New result with Polynomial Regression
y_pred = predict(poly_reg, data.frame(Level = 6.5,
                                      Level2 = 6.5^2,
                                      Level3 = 6.5^3,
                                      Level4 = 6.5^4))