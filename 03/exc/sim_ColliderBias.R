rm(list = ls()) 
# Population parameters # ==================================
n <- 1e3
b <- c(1, 1)
mu_t <- c(0, 0)
vcov_t <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)

# Treatment & outcome # ====================================
treatment <- MASS::mvrnorm(n, mu_t, vcov_t)
cor(treatment) # check correlation in the population
p_y <- pnorm(treatment %*% b)

# simulate one trial # =====================================
y_obs <- rbinom(n, 1, p_y)
glm(y_obs ~ treatment, family = binomial(link = 'probit'))
filter <- y_obs == 1
cor(treatment[filter, 1], treatment[filter, 2], )
cor(treatment[!filter, 1], treatment[!filter, 2], )

# replicate k times # ======================================
K <- 1e3
pearson <- matrix(FALSE, nrow = 2, ncol = K)
for(i in 1:K){
    y_obs <- rbinom(n, 1, p_y)
    filter <- y_obs == 1
    pearson[1, i] <- cor(treatment[filter, 1], treatment[filter, 2], )
    pearson[2, i] <- cor(treatment[!filter, 1], treatment[!filter, 2], )
}
summary(pearson[1, ]) # y_obs == 1
summary(pearson[2, ]) # y_obs == 0

# Variants to explore # ====================================
# 1. Change population parameters
# 2. Change covariance in treatment
# 3. Change link function, keep binomial
# 4. Change quality of the outcome (requires new stratification approach)

