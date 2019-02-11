# Question 1
# Create the dataset and outlier
sleephours <- runif(99, 2, 12)
savingaccount <- sleephours*10 + rnorm(99, mean = 0, sd = 2.6)
trangdata <- data.frame("hoursofsleep" = sleephours,
                        "savings" = savingaccount)
outlier <- c(12, -3000)
trangdata2 <- rbind(trangdata, outlier)
# Create linear models for both datasets
fitline <- lm(savings ~ hoursofsleep, data=trangdata)
fitline2 <- lm(savings ~ hoursofsleep, data=trangdata2)
summary(fitline)
summary(fitline2)
# Plot both datasets with their fit lines
plot(trangdata2$hoursofsleep, trangdata2$saving, main = "Regression model for sleeping hours and savings",
     xlab = "Hours of sleep", ylab = "Savings", type = "p", pch = 1, cex = 0.3)
abline(fitline, col = "blue")
abline(fitline2, col = "red")

# Question 2
library(MASS)
library(Matching)
library(arm)
data(lalonde)
# Get the control data
lalondecontrol <- lalonde[lalonde$treat==0,]
# Get the linear model
fitlalonde <- lm(re78 ~ age + educ + re74 + re75
                 + educ*re74 + educ*re75 + age*re74
                 + age*re75 + re74*re75, data = lalondecontrol)
# 2.1
# Simulate the coefficients and sigmas
set.seed(223)
sim_results <- sim(fitlalonde, n.sims = 10000)
show(sim_results)
# Get the medians of educ, re74, re75
a <- median(lalondecontrol$educ)
b <- median(lalondecontrol$re74)
c <- median(lalondecontrol$re75)
# Store 10000 simulated results by age in a matrix
result_matrix <- matrix(NA, nrow=39, ncol=10000)
for (theAge in c(17:55)) {
  # Run the loop for each age
  onePerson <- c(1, theAge, a, b, c, a*b, a*c, theAge*b, theAge*c, b*c)
  eachAge <- c(rep(0, 10000))
  for (i in 1:10000) {
    # Get predicted result for each simulation
    predicted_results <- sum(sim_results@coef[i,]*onePerson) + rnorm(1, 0, sim_results@sigma[i])
    eachAge[i] <- predicted_results
  }
  result_matrix[theAge - 16,] <- eachAge
}
# Confidence intervals
quantile_table <- matrix(NA, nrow=39, ncol = 2)
colnames(quantile_table) <- c("2.5%", "97.5%")
rownames(quantile_table) <- c(17:55)
for (x in 1:39) {
  # Get the 95% confidence interval for each age
  q <- quantile(result_matrix[x,],c(0.025, 0.975))
  quantile_table[x,] <- q
}
quantile_table
# Plot the graph for confidence intervals by age
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-10000,20000), 
     main = "95% prediction interval for re78 by age, other variables held at their medians", xlab = "Age of Respondents", 
     ylab = "95% prediction interval for re78")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = quantile_table[age - 16, 1],
    x1 = age,
    y1 = quantile_table[age - 16, 2],
    lwd = 2)
}

# 2.2
# Simulate the coefficients and sigmas
set.seed(139)
sim_results2 <- sim(fitlalonde, n.sims = 10000)
show(sim_results2)
# Get the 90% quantile of educ, re74, re75
a2 <- quantile(lalondecontrol$educ, 0.9)
b2 <- quantile(lalondecontrol$re74, 0.9)
c2 <- quantile(lalondecontrol$re75, 0.9)
# Store 10000 simulated results by age in a matrix
result_matrix2 <- matrix(NA, nrow=39, ncol=10000)
for (theAge2 in c(17:55)) {
  # Run the loop for each age
  onePerson2 <- c(1, theAge, a2, b2, c2, a2*b2, a2*c2, theAge*b2, theAge*c2, b2*c2)
  eachAge2 <- c(rep(0, 10000))
  for (i2 in 1:10000) {
    # Get predicted result for each simulation
    predicted_results2 <- sum(sim_results2@coef[i2,]*onePerson2) + rnorm(1, 0, sim_results2@sigma[i2])
    eachAge2[i2] <- predicted_results2
  }
  result_matrix2[theAge2 - 16,] <- eachAge2
}
result_matrix2[1:5,1:5]

# Confidence intervals
quantile_table2 <- matrix(NA, nrow=39, ncol = 2)
colnames(quantile_table2) <- c("2.5%", "97.5%")
rownames(quantile_table2) <- c(17:55)
for (x2 in 1:39) {
  # Get the 95% confidence interval for each age
  q2 <- quantile(result_matrix2[x2,],c(0.025, 0.975))
  quantile_table2[x2,] <- q2
}
quantile_table2
# Plot the graph for confidence intervals by age
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-10000,20000), 
     main = "95% prediction intervals for re78 by age, other variables held at their 90% percentiles", xlab = "Age of Respondents", 
     ylab = "95% prediction intervals for re78")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = quantile_table2[age - 16, 1],
    x1 = age,
    y1 = quantile_table2[age - 16, 2],
    lwd = 2)
}

# Question 3
# Get the linear model
library(foreign)
nsw <- read.dta("~/Downloads/nsw.dta")
fitnsw <- lm(re78 ~ treat, data = nsw)
summary(fitnsw)
length(nsw$data_id)
# Bootstrapping
table_coef <- c(rep(0,10000))
set.seed(123)
for (e in 1:10000) {
  # Bootstrap 10000 times
  s <- sample(c(1:722), size = 722, replace = TRUE)
  onelm <- lm(re78 ~ treat, data = nsw, subset = s)
  table_coef[e] <- summary(onelm)$coefficients[2]
}
quantile(table_coef,c(0.025, 0.975))
hist(table_coef, col = "yellow", main = "Histogram of the coefficents", xlab = "The coefficients", ylab = "Frequency")
confint(fitnsw)[2,]
compareTable <- rbind(quantile(table_coef,c(0.025, 0.975)), confint(fitnsw)[2,])
rownames(compareTable) <- c("bootstrap", "analytical")
compareTable

# Question 4
RSquaredFunc <- function(x, y) {
  # Get the predicted Ys based on the linear model
  predicted_ys <- predict(lm(y~x))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
  # Calculate residual sum of squares and total sum of squares
  rss <- sum((y-predicted_ys)^2)
  tss <- sum((y-c(rep(mean(y),length(x))))^2)
  # Calculate R-squared
  return (1 - (rss/tss))
}
print("RSquared using the new function is") 
RSquaredFunc(nsw$re75, nsw$re78)
print("RSquared using the formula is")
summary(lm(nsw$re78 ~ nsw$re75))$r.squared

# Question 5
glm.fit <- glm(treat ~ age + education + black + hispanic + married + nodegree + re75, data = nsw, family = binomial)
predict_prob <- predict(glm.fit, nsw, type = "response")
probfortreat <- predict_prob[which(nsw$treat==1)]
probforcontrol <- predict_prob[which(nsw$treat==0)]
summary(probforcontrol)
summary(probfortreat)
hist(probfortreat, col = "red", main = "Histogram for probabilities of the treatment group being treated", xlab = "Probabilities of the the treatment being treated", ylab = "Frequency")
hist(probforcontrol, col = "blue", main = "Histogram for probabilities of the control group being treated", xlab = "Probabilities of the control group being treated", ylab = "Frequency")
