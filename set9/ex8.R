library(mosaic)
library(ggplot2)
library(stats)

# input
x <- scan("boot_x.txt")
y <- scan("boot_y.txt")

# solution

# scatter plot
df <- data.frame(x = x, y = y)
gg <- ggplot(df, aes(x = x, y = y)) + geom_point(color = "blue")

points <- gg

# regression
sample_size <- 20
sample_df <- df[sample(1:nrow(df), sample_size), ]
sample_model <- lm(y ~ x, sample_df)
sample_line <- gg + geom_abline(intercept = coef(sample_model)[1], slope = coef(sample_model)[2], color = "red", size = 1)

# bootstrap
R <- 1000
boot_lines <- gg
intercept <- 0
slope <- 0
for (i in 1:R) {
  boot_sample <- resample(sample_df)
  boot_model <- lm(y ~ x, boot_sample)
  boot_lines <- boot_lines +
    geom_abline(intercept = coef(boot_model)[1], slope = coef(boot_model)[2], color = "pink", size = 0.5)
  intercept <- intercept + coef(boot_model)[1]
  slope <- slope + coef(boot_model)[2]
}

# average
intercept <- intercept / R
slope <- slope / R

print("Sample coeffs")
sample_model
print("Average coeffs")
intercept
slope

model = lm(y ~ x, df)
print("All data")
model

points
sample_line
boot_lines
sample_line + 
  geom_abline(intercept = intercept, slope = slope, color = "purple", size = 1) + 
  geom_abline(intercept = coef(model)[1], slope = coef(model)[2], color = "green", size = 1)

