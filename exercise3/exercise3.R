library('MASS')
npk

########### a ###########
set.seed(123)  # add random seed for reproduce

# initial params
I <- 6  # blocks
J <- 4  # plots per block

# initial data frame
randomized_design <- data.frame(block = rep(1:I, each = J), plot = rep(1:J, times = I))

# for each block b, put (N, P, K) on each 2 plots randomly
for (b in 1:I) {
  plots <- sample(1:J, J, replace = FALSE)  # randomly reorder plots in each block
  
  # put N in the header 2 plots
  randomized_design$N[randomized_design$block == b] <- ifelse(plots %in% plots[1:2], 1, 0)
  
  # randomly put P in 2 plots
  randomized_design$P[randomized_design$block == b] <- ifelse(plots %in% sample(plots, 2), 1, 0)
  
  # randomly put K in 2 plots
  randomized_design$K[randomized_design$block == b] <- ifelse(plots %in% sample(plots, 2), 1, 0)
}

# print the plots
print(randomized_design)


########### b ###########
# combine those yield in the same block same N, and calc its mean
yield_matrix <- tapply(npk$yield, list(npk$block, npk$N), mean)

# plot bars
barplot(t(yield_matrix), beside = TRUE, col = c("red", "blue"),
        legend.text = c("N=0", "N=1"),
        main = "Average Yield per Block for Nitrogen Treatment",
        xlab = "Block", ylab = "Average Yield")

########### c ###########
data0 = npk
data0$block = as.factor(data0$block)
data0$N = as.factor(data0$N)
table = lm(yield~block*N, data=data0)
summary(table)
anova(table)

# P_value(block:N) = 0.47, This shows that there is no obvious difference in the effect of nitrogen fertilizer between different plots.
# But P_value(block)= 0.03967 < 0.05, so block should be included in our experiment, we could check the + affect

table = lm(yield ~ block + N, data = data0)
anova(table)
# prove it again, in + affect test, we find P_value(block) = 0.026173

# Friedman test
data0 = npk
data0$block = as.factor(data0$block)
data0$N = as.factor(data0$N)

par(mfrow=c(1,2))
boxplot(data0$yield~data0$N,xlab="Factor N",ylab="Yield")
interaction.plot(data0$N,data0$block,data0$yield)

table(data0$block, data0$N)

# error because we cannot use friedman directly, each block has more than one same value N
# should use lme4
friedman.test(data0$yield,data0$N,data0$block,data=data0)

########### d ###########
library('MASS')
data0 = npk

# initial the model factors
data0$block = as.factor(data0$block)
data0$N = as.factor(data0$N)
data0$P = as.factor(data0$P)
data0$K = as.factor(data0$K)

# model 1: yield ~ N + P + K + N:block
model_1 = lm(yield ~ N + P + K + N:block, data = data0)
summary(model_1)
anova(model_1)

# model 2: yield ~ N + P + K + P:block
model_2 = lm(yield ~ N + P + K + P:block, data = data0)
summary(model_2)
anova(model_2)

# model 3: yield ~ N + P + K + k:block
model_3 = lm(yield ~ N + P + K + K:block, data = data0)
summary(model_3)
anova(model_3)

# model 4: yield ~ N + P + K + block
model_4 = lm(yield ~ N + P + K + block, data = data0)
summary(model_4)
anova(model_4)

# chose model 1 considering the min Residual error = y_i - y_i_predict
# model 4 is just a base line, should not be considered
par(mfrow=c(2,2))
qqnorm(residuals(model_1))
qqline(residuals(model_1), col="red")

qqnorm(residuals(model_2))
qqline(residuals(model_2), col="red")

qqnorm(residuals(model_3))
qqline(residuals(model_3), col="red")

qqnorm(residuals(model_4))
qqline(residuals(model_4), col="red")

plot(fitted(model_1), residuals(model_1))
plot(fitted(model_2), residuals(model_2))
plot(fitted(model_3), residuals(model_3))
plot(fitted(model_4), residuals(model_4))

########### e ###########
model_1 = lm(yield ~ N + P + K + N:block, data = data0)
summary(model_1)
# best combination {(1, 0, 0), block3}


