#uploading the data
cholesterol_data <- read.delim("cholesterol.txt", sep = " ")

#some basic summary statistics and exploring the structure of the data
summary(cholesterol_data)
str(cholesterol_data)

#histograms showing cholesterol levels before and after 8 weeks
par(mfrow = c(1,2))  # Split the plot into two panels
hist(cholesterol_data$Before, main = "Histogram of Cholesterol Levels Before", xlab = "Cholesterol Level", col = "lightblue", border = "black")
hist(cholesterol_data$After8weeks, main = "Histogram of Cholesterol Levels After 8 Weeks", xlab = "Cholesterol Level", col = "lightcoral", border = "black")
#pearson´s correlation test
cor.test(cholesterol_data$Before, cholesterol_data$After8weeks, method = "pearson")
#plotting correlation
plot(cholesterol_data$Before, cholesterol_data$After8weeks,
     main = "Scatter Plot: Before vs After 8 Weeks",
     xlab = "Before",
     ylab = "After 8 Weeks",
     pch = 16, col = "blue")

abline(lm(cholesterol_data$After8weeks ~ cholesterol_data$Before), col = "red", lwd = 2)

"""
Two-paired t-test to see whether the levels of cholesterol are really different before and after the diet.
This test is suitable because the data is paired - the cholesterol levels were measured in the same
individuals, after a certain time frame
"""

t.test(cholesterol_data$Before, cholesterol_data$After8weeks, paired = TRUE)

"""
The Mann-Whitney test can be applied here since there is no reason to believe that before should 
always be higher or lower under the null hypothesis.
"""
set.seed(123) 

#mean difference
observed_diff <- mean(cholesterol_data$Before - cholesterol_data$After8weeks)

nPerm <- 10000
perm_diffs <- numeric(nPerm)

for(i in seq_len(nPerm)){
  #for each subject we randomly flip the sign of the difference
  flip_signs <- sample(c(1, -1), size = length(cholesterol_data$Before), replace = TRUE)
  perm_diffs[i] <- mean((cholesterol_data$Before - cholesterol_data$After8weeks) * flip_signs)
}

#two-sided p-value = proportion of permuted diffs at least as extreme as observed
p_value_perm <- mean(abs(perm_diffs) >= abs(observed_diff))

p_value_perm

"""
Normal-based 97% CI.
"""
#extracting the After18weeks measurement
x <- cholesterol_data$After8weeks

n <- length(x)        
xbar <- mean(x)       
s <- sd(x)            
alpha <- 0.03         #for a 97% CI
z_val <- qnorm(1 - alpha/2)  #z quantile at 0.985 (about 2.17)

ME <- z_val * s / sqrt(n)    
normal_CI <- c(xbar - ME, xbar + ME)
normal_CI

"""
Bootstraped 97% CI.
"""
set.seed(123) 

B <- 10000
boot_means <- numeric(B)

for (b in 1:B) {
  boot_sample <- sample(x, size=n, replace=TRUE)
  boot_means[b] <- mean(boot_sample)
}

#gets the 1.5th and 98.5th percentiles (for a 97% CI)
boot_CI <- quantile(boot_means, probs = c(0.015, 0.985))
boot_CI

##the results above indicate almost no sensitivity to the normality assumption.