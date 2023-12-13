# Packages

library(tidyverse)
library(cowplot)
library(distributional)
library(ggdist)
library(here)
devtools::load_all()

# Figure Ordinal Models from @Burkner2019-aw ------------------------------

# k <- 4
# dd <- data.frame(m = 0, s = 1)
# dd$dist <- distributional::dist_normal(dd$m, dd$s)
# alpha <- prob_to_alpha(c(0.1, 0.4, 0.4, 0.1), link = "probit")
# 
# 
# cumulative <- ggplot(dd, aes(dist = dist, fill = after_stat(factor(findInterval(x, alpha) + 1)))) +
#   stat_slab(orientation = "horizontal") +
#   xlim(c(-4, 4)) +
#   xlab(latex2exp::TeX("$Y^{*}$")) +
#   theme(legend.position = "none") +
#   geom_vline(xintercept = alpha) +
#   annotate("label", x = alpha, y = 0, label = latex2exp::TeX(sprintf("$\\alpha_{%s}$", 1:(k - 1)))) +
#   annotate("label", x = seq(-2, 2, length.out = 4), y = 1, label = sprintf("Y = %s", 1:k))
# 
# seq1 <- ggplot(dd, aes(dist = dist, fill = after_stat(factor(findInterval(x, alpha[1]) + 1)))) +
#   stat_slab(orientation = "horizontal") +
#   xlim(c(-4, 4)) +
#   xlab(latex2exp::TeX("$Y^{*}_1$")) +
#   cowplot::theme_nothing() +
#   theme(axis.title.x = element_text(),
#         legend.position = "none") +
#   geom_vline(xintercept = alpha[1]) +
#   annotate("label", x = alpha[1], y = 0, label = latex2exp::TeX(sprintf("$\\alpha_{%s}$", 1))) +
#   annotate("label", x = c(-2, -0.5), y = 1, label = c("Y = 1", "Y > 1"))
# 
# seq2 <- ggplot(dd, aes(dist = dist, fill = after_stat(factor(findInterval(x, alpha[2]) + 1)))) +
#   stat_slab(orientation = "horizontal") +
#   xlim(c(-4, 4)) +
#   xlab(latex2exp::TeX("$Y^{*}_2$")) +
#   cowplot::theme_nothing() +
#   theme(axis.title.x = element_text(),
#         legend.position = "none") +
#   geom_vline(xintercept = alpha[2]) +
#   annotate("label", x = alpha[2], y = 0, label = latex2exp::TeX(sprintf("$\\alpha_{%s}$", 2))) +
#   annotate("label", x = c(-1, 1), y = 1, label = c("Y = 2", "Y > 2"))
# 
# seq3 <- ggplot(dd, aes(dist = dist, fill = after_stat(factor(findInterval(x, alpha[3]) + 1)))) +
#   stat_slab(orientation = "horizontal") +
#   xlim(c(-4, 4)) +
#   xlab(latex2exp::TeX("$Y^{*}_3$")) +
#   cowplot::theme_nothing() +
#   theme(axis.title.x = element_text(),
#         legend.position = "none") +
#   geom_vline(xintercept = alpha[3]) +
#   annotate("label", x = alpha[3], y = 0, label = latex2exp::TeX(sprintf("$\\alpha_{%s}$", 3))) +
#   annotate("label", x = c(0.5, 2), y = 1, label = c("Y = 3", "Y > 3"))
# 
# 
# cowplot::plot_grid(seq1, seq2, seq3, nrow = 1)

# Figure Proportional Odds ------------------------------------------------

k <- 4
dd <- tibble(
  l = c(0, 1, 3),
  s = c(1, 1, 1),
  y = sprintf("$Y \\leq %s", 1:(k - 1))
)

dd <- rbind(dd, dd)
dd$type <- rep(c("po", "npo"), each = 3)
dd$s <- ifelse(dd$type == "npo", c(0.5, 1.5, 2), dd$s)

dd$x <- list(runif(1e3, -10, 10))
dd$p <- map2(dd$l, dd$s, ~plogis(dd$x[[1]], .x, .y))

dd <- unnest(dd, c(x, p))
dd$type <- ifelse(dd$type == "po", "PO", "NPO")
dd$lp <- qlogis(dd$p)
dd <- pivot_longer(dd, c(p, lp))
dd$name <- factor(dd$name, labels = c(latex2exp::TeX("$\\eta$"), latex2exp::TeX("Probability")))

fig_prop_odds <- dd |> 
  ggplot(aes(x = x, y = value, color = y)) +
  facet_grid(name~type, scales = "free", labeller = label_parsed) +
  geom_line(lwd = 1) +
  scale_color_discrete(labels = latex2exp::TeX(unique(dd$y))) +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) 

# Saving ------------------------------------------------------------------

plots <- filor::named_list(
  fig_prop_odds
)

saveRDS(plots, here("objects", "r-figs.rds"))