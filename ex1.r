
r <- 1


n <- 10000


x1 <- runif(n, -r, r)
y1 <- runif(n, -sqrt(r^2 - x1^2), sqrt(r^2 - x1^2))
x2 <- runif(n, -r, r)
y2 <- runif(n, -sqrt(r^2 - x2^2), sqrt(r^2 - x2^2))

# Calculate chord lengths
chord_lengths <- sqrt((x1 - x2)^2 + (y1 - y2)^2)

# Calculate the number of chord lengths greater than the side of the inscribed triangle
num_long_chords <- sum(chord_lengths > 2 * r / sqrt(3))

# Calculate the probability of a chord being longer than the side of the inscribed triangle
prob <- num_long_chords / n

# Print the result
print(prob)