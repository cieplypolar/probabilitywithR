library(mosaic)

# input
N <- 40
sample <- c(16, 24)
conf <- 0.95

# solution based on https://www.youtube.com/watch?v=ubX4QEPqx5o
R <- 1000
sample_prop <- sample[2] / N
boot_sample <- do(R) * rflip(N, prob = sample_prop)
ggplot(boot_sample, aes(x=prop)) + geom_histogram(color = "black", fill = "green", bins = 20)
confint(boot_sample, level = conf, method = "quantile")