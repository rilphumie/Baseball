# Pollution and Mortality with Regression

# a)
# load data into R
mort <- read.csv("Mortality Lab - Data.csv")
# examine the data - notice some columns with no data. Let's clean it!
View(mort)

# Histogram for morality 
hist(mort$Mortality, freq = TRUE, col = "blue",
     xlab = "Mortality", main = "Distribution of Mortality")
# Histogram for NOx, before using the log() function
hist(mort$NOx, freq = TRUE, col = "black",
     xlab = "Nitrous Oxide", main = "Distribution of NOx")
# make a new column for log(NOx)
mort$logNOx <- log(mort$NOx)
# let's make another histogram using log(NOx)
hist(mort$logNOx, freq = TRUE, col = "red",
     xlab = "log(NOx)", main = "Distribution of log(NOx)")

# scatter plot of Mortality vs. log(NOx) - seems like there may be a positive, linear relationship
plot(mort$logNOx, mort$Mortality,  col = "black", xlab = "log(NOx)", 
     ylab = "mortality", main = "Mortality vs Nitrous Oxide")

# linear model for our data 
linear_model <- lm(Mortality ~ logNOx, data = mort)
summary(linear_model)
#Coefficients:
#           Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept) 905.613      16.672   54.319  <2e-16 ***
#  logNOx     15.099       6.419    2.352   0.0221 *  

# i) Provide the slope, y-intercept and full formula below.
# slope: 15.099
# y-intercept: 905.613
# full formula: y = 15.099x + 905.613

# ii) Paste your clearly labeled plot below.
# add the line from the linear model above to the scatter plot "Mortality vs Nitrous Oxide"
abline(linear_model, col = "red", lwd = 2)

# iii) Note why mortality is the dependent variable.
# Mortality is dependent - we are trying to learn about if mortality
# is affected by the NOx - mortality is the outcome

# b) Please check that your results from above match the mathematical derivation 
# by manually calculating the slope and y-intercept for simple linear regression analysis in R. 

# formula for slope
slope <- sum((mort$Mortality - mean(mort$Mortality))*
               (mort$logNOx-mean(mort$logNOx)))/
                sum(((mort$logNOx-mean(mort$logNOx)))^2)
print(slope) # 15.09896
# formula for y-intercept 
yint <- mean(mort$Mortality) - slope * mean(mort$logNOx)
print(yint) # 905.6132

# c) below is predicted mortality for each row in its own column in the table 
# make object for coefficients  
coefficients <- coefficients(linear_model)
mort$mort_predicted <- coefficients[1] + coefficients[2]*mort$logNOx
# column for the residuals, recall the formula: observed - predicted
mort$residuals <- mort$Mortality - mort$mort_predicted


mort[mort$city == "Akron, OH", "residuals"]
# -24.63192

# d) Perform residual/normality analysis on these results.
# create a function for standardizing 
standardize<-function(x) {
  return((x-mean(x))/sd(x))
}
# 1) visually test the normality of the log(NOx) variable
# normalizing the logNOx
norm_logNOx <- standardize(mort$logNOx)
# create a normal Q-Q plot 
qqnorm(norm_logNOx)
# add a line to see if it fits the Q-Q plot 
abline(0, 1, lwd = 2 ,col= "red" )
# looks normal - assumption met

# 2) visually test the normality of the residuals
# normalizing the residuals 
norm_res <- standardize(mort$residuals)
# create a normal Q-Q plot
qqnorm(norm_res)
# add a line to see if it fit the Q-Q plot
abline(0, 1, lwd = 2, col= "blue")
# looks normal - assumption met
# OR
plot(linear_model) 

# 3) visually test the constant variance assumption
# there appears to be no pattern in the residuals - assumption met
plot(mort$mort_predicted, mort$residuals)

# e) What percentage of the variation in mortality was 
# explained by log(NOx)? (Recall the coefficient of determination) 
# find the correlation between logNOx and Mortality, and sqaure it
cor(mort$logNOx, mort$Mortality)^2
# 0.08709559
# OR
summary(linear_model)
summary(linear_model)$r.squared
# 0.08709559
