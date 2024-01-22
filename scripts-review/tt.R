library(ordinal)

odds <- function(p) p / (1 - p)

set.seed(2026)

N <- 1e4 # total sample size
k <- 4 # number of ordinal outcomes
x <- rep(c(0, 1), each = N/2) # group
b1 <- log(3) # log odds ratio
z1 <- log(2) # log difference in the scale
probs0 <- rep(1/k, k) # uniform probabilities for the group 0
alpha <- qlogis(cumsum(probs0)[-k]) # thresholds
ystar <- rlogis(N, location = (b1 * x), scale = exp(z1 * x))
y <- ordered(findInterval(ystar, alpha) + 1)
dat <- data.frame(x, y)

fit <- clm(y ~ x, scale = ~ x, data = dat, link = "logit")
summary(fit)

# I fitted a proportional odds model or better the b1 is the same
# for each cutpoint

fit_non_po <- clm(y ~ 1, nominal = ~ x, data = dat, link = "logit")

# the vector of coefficients b1 for each alpha is the same
# as fitting k - 1 logistic regression on the cumulative probabilities

y1vs234 <- ifelse(dat$y <= 1, 1, 0)
y12vs34 <- ifelse(dat$y <= 2, 1, 0)
y123vs4 <- ifelse(dat$y <= 3, 1, 0)

ylist <- list(y1vs234, y12vs34, y123vs4)

fitl <- lapply(ylist, function(y) glm(y ~ x, family = binomial(link = "logit")))

# betas are the same for the non-po model and the k - 1 logistic regressions
sapply(fitl, coef)[2, ]
coef(fit_non_po)[4:6]

# here i estimate the cumulative odds from the observed probabilities

(p <- sapply(ylist, function(y) tapply(y, dat$x, mean)))

# cumulative odds ratios
apply(p, 2, function(c) log(odds(c[1])/odds(c[2])))

# here the true cumulative odds from the linear predictor of the scale-location
# model

pr <- data.frame(x = c(0, 1))
lp <- lapply(c(-Inf, alpha, Inf), function(a) plogis((a - b1 * pr$x) / exp(z1 * pr$x)))
lp <- data.frame(lp)[, 2:k]
names(lp) <- c("y1vs234", "y12vs34", "y123vs4")
apply(lp, 2, function(c) log(odds(c[1])/odds(c[2])))

