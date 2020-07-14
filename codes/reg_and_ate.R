rm(list = ls())
set.seed(123)
N <- 1000
b1 <- 2 # ATE
# control variables
lenx2 <- 5 # 1 to 5
lenx3 <- 3 # 1 to 3
lenx4 <- 4 # 1 to 4
lenx5 <- 4 # 1 to 4
# probability of covariates
probx2 <- sample(1:5, size = lenx2, replace = TRUE)
probx3 <- sample(1:5, size = lenx3, replace = TRUE)
probx4 <- sample(1:5, size = lenx4, replace = TRUE)
probx5 <- sample(1:5, size = lenx5, replace = TRUE)
# generate covariates
x2 <- sample(1:lenx2, prob = probx2, size = N, replace = TRUE)
x3 <- sample(1:lenx3, prob = probx3, size = N, replace = TRUE)
x4 <- sample(1:lenx4, prob = probx4, size = N, replace = TRUE)
x5 <- sample(1:lenx5, prob = probx5, size = N, replace = TRUE)
raw_mat <- cbind(x2, x3)
# different error term structure
e1 <- rnorm(mean = 0, sd = 4, n = N)
e0 <- rnorm(mean = 0, sd = 4, n = N)

# possible combination of control variables
comb <- expand.grid(1:lenx2, 1:lenx3)#, 1:lenx4)#, 1:lenx5)
colnames(comb) <- c("x2", "x3")#, "x4")#, "x5")
# create unique ID for each combination
comb <- cbind(comb, id = 1:nrow(comb))
# control variables for each observation is mapped to IDs
comb_join <- inner_join(data.frame(x2 = x2, x3 = x3), data.frame(comb), 
                        by = c("x2" = "x2", "x3" = "x3"))
# create a matrix of dummy variables
matXs <- matrix(0, nrow = N, ncol = nrow(comb))

for (i in 1:nrow(comb_join)){
  .id <- comb_join$id[i]
  matXs[i, .id] <- 1
}


# potential outcomes
y1 <- e1 + b1 + 3 * x2 + -2 * x3# + b4 * x4 # + b5 * x5
y0 <- e0 + 3 * x2 + -2 * x3# + b4 * x4 # + b5 * x5

# create function
sim_reg_saturated <- function(.y1, .y0, .mat, .N, .raw) {
  # random assignment of treatment
  .p <- apply(.raw, 1, sum)^2 / max(apply(.raw, 1, sum) + 1)
  x <- as.numeric(runif(length(.p)) < .p)
  df <- data.frame(x = x, .mat)
  # observed y
  y <- ifelse(x == 1, .y1, .y0)
  # satruated regression
  mod <- lm(y ~ . -1, df)
  
  # normal regression
  df2 <- data.frame(x = x, .raw)
  mod2 <- lm(y ~ ., df2)
  
  return( c(mod$coef[1], mod2$coef[2]) )
}

set.seed(123)
# simulation
M <- 1000
# use replicate and generated data above
res <- replicate(n = M, sim_reg_saturated(y1, y0, matXs, N, raw_mat))


xup <- 4
xdown <- 0

hist(res[1, ], breaks = 100, xlab = "Value", main = "", xlim = c(xdown , xup), xaxt = "n")
axis(1, at = seq(xdown , xup, by = .5), labels = seq(xdown , xup, by = .5))
abline(v = mean(y1 - y0), col = "red") # true ATE is not 2
abline(v = mean(res[1, ]), col = "blue")
title(main = "Saturated regresssion")

