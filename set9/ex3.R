# input 
sample = c(1, 2, 2, 3, 3, 3)

# solution
# The domain has 3 elements. In the bootstrap we resample with replacement,
# so it is 3 ^ (sample_size) in our case 3 ^ 6 different samples if the order matters.
# If the order does not matter x_1 + x_2 + x_3 = 6 x_i >= 0 and from simple combinatorics ->
# Newton(6 + 3 - 1, 3 - 1)