library('MASS')

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
