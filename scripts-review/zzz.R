# quick example with a random effect

library(ordinal)

n <- 1e5
dat <- data.frame(
  x = seq(0, 1, length.out = n)
)

dat <- sim_ord_latent(~x, By = 8, probs = rep(1/5, 5), data = dat, link = "logit")
fit <- clm(y ~ 1, data = dat, link = "logit")
pr <- predict(fit, type = "linear.predictor")
dat <- cbind(dat, pr)

dat$yn <- as.integer(dat$y)

cum_prob <- function(y){
  if(!is.numeric(y)){
    y <- as.integer(y)
  }
  sy <- sort(unique(y))
  cy <- sapply(sy, function(t) mean(y <= t))
  cy <- cy[-length(cy)]
  names(cy) <- sprintf("y<=%s", sy[-length(sy)])
  return(cy)
}

dat$xc <- cut(dat$x, seq(0, 1, 0.1), include.lowest = TRUE)

dd <- lapply(split(dat$y, dat$xc), function(x) qlogis(cum_prob(x))) |> 
  data.frame() |> 
  t() |> 
  data.frame()

names(dd) <- sprintf("y<=%s", 1:4)
dd$x <- 1:nlevels(dat$xc)
dd |> 
  pivot_longer(starts_with("y")) |> 
  ggplot(aes(x = x, y = value, color = name)) +
  geom_line()

dat <- data.frame(x = seq(0, 1, 0.1))
dat <- get_probs(~x, 5, rep(1/4, 4), data = dat, link = "logit", append = TRUE)

pp <- apply(dat[, -1], 1, cumsum, simplify = FALSE) |> 
  bind_rows()
pp <- pp[, -ncol(pp)]
names(pp) <- sprintf("y<=%s", 1:3)

cbind(dat, pp) |> 
  select(x, 6:8) |> 
  pivot_longer(2:4) |> 
  ggplot(aes(x = x, y = qlogis(value), color = name)) +
  geom_line()


k <- 5 # number of levels for y
grid <- data.frame(x = seq(0, 1, 0.1)) # numeric predictor
probs <- get_probs(~x, 5, rep(1/k, k), data = grid, link = "logit") # probabilities
cprobs <- apply(probs, 1, cumsum, simplify = FALSE)
cprobs <- data.frame(do.call(rbind, cprobs))
names(cprobs) <- sprintf("y<=%s", 1:k)
cprobs <- cprobs[, -ncol(cprobs)] # remove the last
grid <- cbind(grid, cprobs)

grid <- grid |> 
  pivot_longer(starts_with("y")) |> 
  mutate(lp = qlogis(value))

poa_probs_plot <- grid |> 
  ggplot(aes(x = x, y = value, color = name)) +
  geom_line() +
  theme_minimal(15) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Probability")

poa_lp_plot <- grid |> 
  ggplot(aes(x = x, y = lp, color = name)) +
  geom_line() +
  theme_minimal(15) +
  theme(axis.title.y = element_blank()) +
  ggtitle("Linear Predictor")

bl <- cowplot::get_legend(
  poa_probs_plot
)

poa_plot <- cowplot::plot_grid(
  poa_probs_plot + theme(legend.position = "none"),
  poa_lp_plot + theme(legend.position = "none")
)

cowplot::plot_grid(poa_plot, bl, ncol = 1, rel_heights = c(0.9, 0.1))





