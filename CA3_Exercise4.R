# Question 3

## 1)

df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

## 2)

nll_lm <- function(par, data) {
  y <- data$y
  X <- as.matrix(data[, -1])
  n <- length(y)
  
  mu <- X %*% par[1:(ncol(X))]
  
  nll <- -sum(dnorm(y, mean = mu, sd = exp(par[ncol(X) + 1]), log = TRUE))
  
  return(nll)
}

## 3)

Use optim() in conjunction with nll_lm() to find the maximum likelihood estimates of β̂ and σ̂ (not σ̂ 2!) using the response and predictors outlined in part 1. (Hint: to avoid numerical instability, let mean(df$y) be your initial guess for the intercept parameter and specify appropriate lower and upper bounds.)


initial_guess <- c(mean(df$y), rep(0, ncol(df) - 1))
result <- optim(par = initial_guess, fn = nll_lm, data = df,
                lower = c(-Inf, rep(-Inf, ncol(df) - 1)), 
                upper = c(Inf, rep(Inf, ncol(df) - 1)))


## 4)

Why was it necessary to implement the negative log-likelihood function?
  
  R's optim() function performs minimization, so to maximize the likelihood, we minimize its negative.

## 5)

Compare your answers for β̂ with those obtained using matrix operations in base R without the use of lm().


X <- as.matrix(cbind(1, df[, -1]))
y <- df[, 1]
beta_hat <- solve(t(X) %*% X) %*% (t(X) %*% y)


## 6)

coefficients_optim <- result$par[1:(ncol(df) - 1)]
X <- as.matrix(df[, -1])
mu_optim <- X %*% coefficients_optim

residuals_optim <- df$y - mu_optim

sigma_optim <- sqrt(sum(residuals_optim^2) / (length(df$y) - length(coefficients_optim)))


## 7)

In part 5, both methods are doing the same thing, finding the coefficients that minimize the sum of squared residuals in a linear regression model.

In part 6, the difference arises because of the way the variance is estimated (optimization vs matrix operations)

## 8)

mle <- optim(par = init_par, fn = nll_lm, data = df, hessian = TRUE, method = "L-BFGS-B",
             lower = c(-Inf, rep(-Inf, length(init_par) - 1)), 
             upper = c(Inf, rep(Inf, length(init_par) - 1)))

hessian_inv <- solve(mle$hessian)

standard_errors <- sqrt(diag(hessian_inv)[-length(init_par)])
standard_errors

# Question 4

lm_fit <- lm(y ~ x1 + x2 + x3, data = df)

beta_hat <- coef(lm_fit)

sigma_hat <- summary(lm_fit)$sigma

beta_hat
sigma_hat