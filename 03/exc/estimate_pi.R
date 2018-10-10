rm(list = ls())
n <- 1000
reps <- 100
r <- 1

# Variant 1 ------------------------------------------------
hits <- vector("logical", length = n) 
for(i in 1:n){
   hits[i] <- sqrt(sum(runif(2, -r, r)^2)) <= r
}
4 * mean(hits)

darts <- matrix(runif(2 * n, -r, r), ncol = 2)
hits <- apply(
  darts, 1, FUN = function(x, radius = r){
    sqrt(sum(x^2)) <= radius # hit circle?
  }
)
4 * mean(hits)

# Variant 2 ------------------------------------------------
throw_dart <- function(radius){
  coordinates <- runif(2, min = -radius, max = radius)
  sqrt(sum(coordinates^2)) <= radius # hit circle?
}
hits <- replicate(n, expr = throw_dart(radius = r))
4 * mean(hits)

# Variant 3 ------------------------------------------------
throw_darts <- function(radius, n_darts){
  coordinates <- matrix(
    runif(2 * n_darts, min = -radius, max = radius), ncol = 2
  )
  hits <- apply(
    coordinates, 1, function(x){sqrt(sum(x^2)) <= radius}
  )
  4 * mean(hits)
}
estimates <- replicate(reps, throw_darts(radius = r, n_darts = n))
summary(estimates)
plot(density(estimates))
abline(v = pi)