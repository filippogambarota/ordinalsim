# underestimating the true d ordinal vs metric

library(ordinal)

d <- 0.5
n <- 50
g <- c("a", "b")
nsim <- 1e3
b0 <- 0
b1 <- d
s <- 2

d_clm <- rep(NA, nsim)
d_lm <- rep(NA, nsim)
p_clm <- rep(NA, nsim)
p_lm <- rep(NA, nsim)
s_lm <- rep(NA, nsim)

for(i in 1:nsim){
  group <- rep(g, each = n/2)
  ystar <- b0 + b1 * ifelse(group == "a", 0, 1) + rnorm(n)
  y <- cut(ystar, c(-Inf, probs_to_th(rep(1/4, 4)), Inf))
  y <- ordered(as.integer(y))
  yn <- as.integer(y)
  fit <- clm(y ~ group, link = "probit")
  fit_lm <- lm(yn ~ group)
  d_clm[i] <- coef(fit)[4]
  d_lm[i] <- coef(fit_lm)[2]
  s_lm[i] <- sigma(fit_lm)
  p_clm[i] <- summary(fit)$coefficients["groupb", 4]
  p_lm[i] <- summary(fit_lm)$coefficients[2, 4]
  filor::pb(1000, i)
}

d_lms <- d_lm / s_lm

plot(d_lms, d_clm)

mean(p_lm <= 0.05)
mean(p_clm <= 0.05)



n <- 1e4
group <- rep(g, each = n/2)
ystar <- b0 + b1 * ifelse(group == "a", 0, 1) + rnorm(n)
y <- cut(ystar, c(-Inf, probs_to_th(rep(1/4, 4), link = "logit"), Inf))
y <- ordered(as.integer(y))
yn <- as.integer(y)
fit <- clm(y ~ group, link = "logit")

B <- c(b1)
X <- model.matrix(~group)
TH <- c(-Inf, probs_to_th(rep(1/4, 4), link = "logit"), Inf)

lp <- lapply(TH, function(th) as.vector(plogis(X %*% c(th, B))))
lp <- data.frame(lp)
names(lp) <- 1:ncol(lp)

dat <- data.frame(t(diff(t(lp))))
names(dat) <- 1:ncol(dat)
dat$y <- apply(dat, 1, function(x) sample(names(dat), 1, replace = TRUE, prob = x))
dat$y

fit2 <- clm(y ~ group, data = dat,  link = "logit")

TH <- probs_to_th(rep(1/4, 4), link = "probit")
b1 <- 0.5

B <- c(TH, b1)
X <- model.matrix(~ 0 + group)
X %*% B

n <- 1e4
group <- rep(g, each = n/2)
ystar <- b0 + b1 * ifelse(group == "a", 0, 1) + rnorm(n)
y <- cut(ystar, c(-Inf, probs_to_th(rep(1/4, 4), link = "logit"), Inf))
y <- ordered(as.integer(y))
yn <- as.integer(y)
fit <- clm(y ~ group, link = "logit")

predict(fit, newdata = data.frame(group = "a"))$fit |> 
  probs_to_th()

y <- lapply(TH, function(x) x - b0 * ifelse(group == "a", 0, 1) + rnorm(length(group)))










