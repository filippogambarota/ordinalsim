library(ordinal)
library(effects)
library(MASS)

# difference response 1-4 between two groups

ps1 <- c(0.25, 0.25, 0.25, 0.25)
b1 <- log(2)
b0 <- probs_to_th(ps1, link = "logit")
n <- 1e5

dat <- data.frame(x = rep(c("g1", "g2"), each = n/2))
P <- get_probs(~x, b0, b1, "logit", cnames = 1:4, data = dat)
dat$y <- get_y(P = P)

fit <- polr(y ~ x, data = dat)

pr <- predict(fit, 
              newdata = data.frame(x = unique(dat$x)), 
              type = "prob")

pr <- data.frame(pr)

apply(pr, 2, function(x) filor::odds_ratio(x[1], x[2]))

pg1 <- predict(fit, newdata = data.frame(x = "g1"))[[1]]
pg2 <- predict(fit, newdata = data.frame(x = "g2"))[[1]]

filor::odds_ratio(pg1, pg2)

filor::odds_ratio(plogis(fit$zeta - fit$coefficients),
                  plogis(fit$zeta))


pr <- predict(fit, 
              newdata = data.frame(x = unique(dat$x)), 
              type = "linear.predictor")






curve(dnorm(x), -5, 5)
curve(dnorm(x, b1), -5, 5, add = TRUE)

xtabs(~x + y, data = dat) |> 
  prop.table(margin = 1) |> 
  apply(2, function(x) filor::odds_ratio(x[1], x[2]))


get_probs(~x, b0, 1, "probit", cnames = 1:4, data = dat) |> 
  distinct() |> 
  mutate(group = c("g1", "g2")) |> 
  pivot_longer(1:4) |> 
  #apply(2, function(x) filor::odds_ratio(x[1], x[2])) |> 
  ggplot(aes(x = name, y = value, fill = group)) +
  geom_col(position = position_dodge())

get_probs(~x, b0, 1, "probit", cnames = 1:4, data = dat) |> 
  distinct()

