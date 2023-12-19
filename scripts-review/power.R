n <- 100
dat <- data.frame(
  g = rep(c(0, 1), each = n/2)
)
dat$g <- factor(dat$g)
contrasts(dat$g) <- contr.sum(2)/2

probs <- rep(1/6, 6)
th <- probs_to_th(probs, "probit")

nsim <- 1000
p_clm <- rep(NA, nsim)
p_lm <- rep(NA, nsim)
b_clm <- rep(NA, nsim)
b_lm <- rep(NA, nsim)

for(i in 1:nsim){
  dat <- sim_ord_latent(~g, ~g, B.y = 0.5, B.sigma = log(5), probs = probs, data = dat, link = "probit")
  dat$yn <- as.integer(dat$y)
  fit_clm <- clm(y ~ g, ~ g, data = dat, link = "probit")
  fit_lm <- lm(yn ~ g, data = dat)
  p_clm[i] <- summary(fit_clm)$coefficients[length(th) + 1, 4]
  p_lm[i] <- summary(fit_lm)$coefficients[2, 4]
  b_clm[i] <- summary(fit_clm)$coefficients[length(th) + 1, 1]
  b_lm[i] <- summary(fit_lm)$coefficients[2, 1]/sigma(fit_lm)
}

plot(b_clm, b_lm, xlim = c(-1, 2), ylim = c(-0.5, 1.5))


mean(p_lm <= 0.05)
mean(p_clm <= 0.05, na.rm = TRUE)


n <- 1e3
dat <- data.frame(
  x = rnorm(n)
)

dat <- sim_ord_latent(~x, ~x, B.y = 0.5, B.sigma = log(1.5),probs = probs, data = dat, link = "logit")
dat$yn <- as.integer(dat$y)

fit2 <- clm(y ~ x, ~ x, data = dat, link = "logit")

summary(fit2)

library(effects)

summary(fit2)









