library(mosaic)
library(ggplot2)

# input
sample <- c(2, 2, 6, 9, 9, 9, 15, 20)

# solution
sample_med <- median(sample)
N <- length(sample)
R <- 1000
boot_sample <- do(R) * median(resample(sample))
median_smaller_than_15 = sum(boot_sample < 15.0)
ggplot(boot_sample, aes(x=median)) + geom_histogram(color = "black", fill = "green", bins = 8)
print(paste("Prob. that median is smaller than 15 is:", median_smaller_than_15 * 100 / R, "%"))

