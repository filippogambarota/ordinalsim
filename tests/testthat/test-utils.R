source("settings.R") # loading objects

test_that("alpha_to_prob() and prob_to_alpha() are working", {
  p1 <- rep(1/6, 6)
  a1l <- prob_to_alpha(p1, link = "logit")
  a1p <- prob_to_alpha(p1, link = "probit")
  expect_equal(p1, alpha_to_prob(a1l, link = "logit"), info = "logit link", ignore_attr = TRUE)
  expect_equal(p1, alpha_to_prob(a1p, link = "probit"), info = "probit link", ignore_attr = TRUE)
})

test_that("dummy_ord() is working!", {
  k <- 5
  y <- sample(1:k, 100, TRUE)
  dy <- dummy_ord(y)
  expect_equal(ncol(dy), k - 1, info = "number of dummy k - 1")
})


test_that("sim_ord_latent(link = 'logit') works with a binary x, k = 4", {
  set.seed(0)
  k <- 4
  n <- 1e5
  probs0 <- c(0.1, 0.3, 0.5, 0.1)
  alpha <- prob_to_alpha(probs0, link = "logit")
  b1 <- log(2) # log odds ratio
  dat <- data.frame(x = rep(0:1, each = n))
  dat <- sim_ord_latent(~x, beta = b1, prob0 = probs0, data = dat, link = "logit")
  fit <- ordinal::clm(y~x, data = dat, link = "logit")
  
  expect_equal(b1, fit$beta, tolerance = .tol["clm"], info = "beta", ignore_attr = TRUE)
  expect_equal(alpha, fit$alpha, tolerance = .tol["clm"], info = "alpha", ignore_attr = TRUE)
  expect_equal(nrow(dat), n*2, info = "sample size", ignore_attr = TRUE)
  
  dat_fit <- dat[, colnames(fit$model)]
  expect_equal(fit$model, dat_fit, info = "dataset is the same after passing within the function", ignore_attr = TRUE)
})

test_that("sim_ord_latent(link = 'probit') works with a binary x, k = 4", {
  set.seed(1)
  k <- 4
  n <- 1e5
  probs0 <- c(0.1, 0.3, 0.5, 0.1)
  alpha <- prob_to_alpha(probs0, link = "probit")
  b1 <- log(2) # log odds ratio
  dat <- data.frame(x = rep(0:1, each = n))
  dat <- sim_ord_latent(~x, beta = b1, prob0 = probs0, data = dat, link = "probit")
  fit <- ordinal::clm(y~x, data = dat, link = "probit")
  
  expect_equal(b1, fit$beta, tolerance = .tol["clm"], info = "beta", ignore_attr = TRUE)
  expect_equal(alpha, fit$alpha, tolerance = .tol["clm"], info = "alpha", ignore_attr = TRUE)
  expect_equal(nrow(dat), n*2, info = "sample size", ignore_attr = TRUE)
  
  dat_fit <- dat[, colnames(fit$model)]
  expect_equal(fit$model, dat_fit, info = "dataset is the same after passing within the function", ignore_attr = TRUE)
})

test_that("sim_ord_latent(link = 'logit') works with a continuous x, k = 4", {
  set.seed(0)
  k <- 4
  n <- 1e5
  probs0 <- c(0.1, 0.3, 0.5, 0.1)
  alpha <- prob_to_alpha(probs0, link = "logit")
  b1 <- log(1.5) # log odds ratio
  dat <- data.frame(x = rnorm(n*2))
  dat <- sim_ord_latent(~x, beta = b1, prob0 = probs0, data = dat, link = "logit")
  fit <- ordinal::clm(y~x, data = dat, link = "logit")
  
  expect_equal(b1, fit$beta, tolerance = .tol["clm"], info = "beta", ignore_attr = TRUE)
  expect_equal(alpha, fit$alpha, tolerance = .tol["clm"], info = "alpha", ignore_attr = TRUE)
  expect_equal(nrow(dat), n*2, info = "sample size", ignore_attr = TRUE)
  
  dat_fit <- dat[, colnames(fit$model)]
  expect_equal(fit$model, dat_fit, info = "dataset is the same after passing within the function", ignore_attr = TRUE)
})

test_that("sim_ord_latent(link = 'probit') works with a continuous x, k = 4", {
  set.seed(0)
  k <- 4
  n <- 1e5
  probs0 <- c(0.1, 0.3, 0.5, 0.1)
  alpha <- prob_to_alpha(probs0, link = "probit")
  b1 <- log(1.5) # log odds ratio
  dat <- data.frame(x = rnorm(n*2))
  dat <- sim_ord_latent(~x, beta = b1, prob0 = probs0, data = dat, link = "probit")
  fit <- ordinal::clm(y~x, data = dat, link = "probit")
  
  expect_equal(b1, fit$beta, tolerance = .tol["clm"], info = "beta", ignore_attr = TRUE)
  expect_equal(alpha, fit$alpha, tolerance = .tol["clm"], info = "alpha", ignore_attr = TRUE)
  expect_equal(nrow(dat), n*2, info = "sample size", ignore_attr = TRUE)
  
  dat_fit <- dat[, colnames(fit$model)]
  expect_equal(fit$model, dat_fit, info = "dataset is the same after passing within the function", ignore_attr = TRUE)
})


test_that("sim_ord_latent(link = 'logit') works with a 2x2 factorial interaction, k = 4", {
  set.seed(1)
  k <- 4
  n <- 1e5
  probs0 <- c(0.1, 0.3, 0.5, 0.1)
  alpha <- prob_to_alpha(probs0, link = "logit")
  b1 <- log(2) # log odds ratio
  b2 <- log(1)
  b3 <- log(1.3)
  dat <- expand.grid(x1 = c("a", "b"), x2 = c("c", "d"), n = 1:((n*2)/4))
  dat <- sim_ord_latent(~x1*x2, beta = c(b1, b2, b3), prob0 = probs0, data = dat, link = "logit")
  fit <- ordinal::clm(y~x1*x2, data = dat, link = "logit")
  
  expect_equal(fit$beta, c(b1, b2, b3), tolerance = .tol["clm"], info = "beta", ignore_attr = TRUE)
  expect_equal(fit$alpha, alpha, tolerance = .tol["clm"], info = "alpha", ignore_attr = TRUE)
  expect_equal(nrow(dat), n*2, info = "sample size", ignore_attr = TRUE)
  
  dat_fit <- dat[, colnames(fit$model)]
  expect_equal(fit$model, dat_fit, info = "dataset is the same after passing within the function", ignore_attr = TRUE)
})

test_that("sim_ord_latent(link = 'probit') works with a 2x2 factorial interaction, k = 4", {
  set.seed(1)
  k <- 4
  n <- 1e5
  probs0 <- c(0.1, 0.3, 0.5, 0.1)
  alpha <- prob_to_alpha(probs0, link = "probit")
  b1 <- log(2) # log odds ratio
  b2 <- log(1)
  b3 <- log(1.3)
  dat <- expand.grid(x1 = c("a", "b"), x2 = c("c", "d"), n = 1:((n*2)/4))
  dat <- sim_ord_latent(~x1*x2, beta = c(b1, b2, b3), prob0 = probs0, data = dat, link = "probit")
  fit <- ordinal::clm(y~x1*x2, data = dat, link = "probit")
  
  expect_equal(fit$beta, c(b1, b2, b3), tolerance = .tol["clm"], info = "beta", ignore_attr = TRUE)
  expect_equal(fit$alpha, alpha, tolerance = .tol["clm"], info = "alpha", ignore_attr = TRUE)
  expect_equal(nrow(dat), n*2, info = "sample size", ignore_attr = TRUE)
  
  dat_fit <- dat[, colnames(fit$model)]
  expect_equal(fit$model, dat_fit, info = "dataset is the same after passing within the function", ignore_attr = TRUE)
})

test_that("sim_ord_latent(link = 'logit') works with a binary predictor x on location and scale, k = 4", {
  set.seed(1)
  k <- 4
  n <- 1e5
  probs0 <- c(0.1, 0.3, 0.5, 0.1)
  alpha <- prob_to_alpha(probs0, link = "logit")
  b1 <- log(2) # log odds ratio
  z1 <- log(3)
  dat <- expand.grid(x = rep(0:1, each = n))
  dat <- sim_ord_latent(~x, ~x, beta = b1, zeta = z1, prob0 = probs0, data = dat, link = "logit")
  fit <- ordinal::clm(y~x, scale = ~x, data = dat, link = "logit")
  
  expect_equal(fit$beta, b1, tolerance = .tol["clm"], info = "beta", ignore_attr = TRUE)
  expect_equal(fit$alpha, alpha, tolerance = .tol["clm"], info = "alpha", ignore_attr = TRUE)
  expect_equal(fit$zeta, z1, tolerance = .tol["clm"], info = "zeta", ignore_attr = TRUE)
  expect_equal(nrow(dat), n*2, info = "sample size", ignore_attr = TRUE)
  
  dat_fit <- dat[, colnames(fit$model)]
  expect_equal(fit$model, dat_fit, info = "dataset is the same after passing within the function", ignore_attr = TRUE)
})

test_that("sim_ord_latent(link = 'probit') works with a binary predictor x on location and scale, k = 4", {
  set.seed(1)
  k <- 4
  n <- 1e5
  probs0 <- c(0.1, 0.3, 0.5, 0.1)
  alpha <- prob_to_alpha(probs0, link = "probit")
  b1 <- log(2) # log odds ratio
  z1 <- log(3)
  dat <- expand.grid(x = rep(0:1, each = n))
  dat <- sim_ord_latent(~x, ~x, beta = b1, zeta = z1, prob0 = probs0, data = dat, link = "probit")
  fit <- ordinal::clm(y~x, scale = ~x, data = dat, link = "probit")
  
  expect_equal(fit$beta, b1, tolerance = .tol["clm"], info = "beta", ignore_attr = TRUE)
  expect_equal(fit$alpha, alpha, tolerance = .tol["clm"], info = "alpha", ignore_attr = TRUE)
  expect_equal(fit$zeta, z1, tolerance = .tol["clm"], info = "zeta", ignore_attr = TRUE)
  expect_equal(nrow(dat), n*2, info = "sample size", ignore_attr = TRUE)
  
  dat_fit <- dat[, colnames(fit$model)]
  expect_equal(fit$model, dat_fit, info = "dataset is the same after passing within the function", ignore_attr = TRUE)
})

test_that("sum_to_1() correctly check", {
  expect_no_error(sum_to_1(rep(1/6, 6)))
  expect_no_error(sum_to_1(rep(1/10, 10)))
  expect_no_error(sum_to_1(rep(1/15, 15)))
  expect_error(sum_to_1(rep(1/4, 3)))
})

test_that("vlogit() correctly calculate the variance", {
  expect_equal((1^2*pi^2)/3, vlogit(1), info = "scale = 1")
  expect_equal((2^2*pi^2)/3, vlogit(2), info = "scale = 2")
  expect_equal((0.5^2*pi^2)/3, vlogit(0.5), info = "scale = 0.5")
})