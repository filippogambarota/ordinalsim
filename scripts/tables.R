library(flextable)
devtools::load_all()

# MODEL TABLE

models_table <- data.frame(
  stringsAsFactors = FALSE,
  Model = c("Model", "Cumulative Logit", "Cumulative Probit"),
  Link.Function = c("Equation",
                    "$\\text{logit}(p) = \\text{log}(p / (1-p))$",
                    "$z = \\Phi(p)$"),
  Link.Function = c("R Code", "`qlogis()`", "`qnorm()`"),
  Inverse.Link.Function = c("Equation",
                            "$e^{\\text{logit}(p)} / (1 + e^{\\text{logit}(p)})$","$\\Phi^{-1}(z)$"),
  Inverse.Link.Function = c("R Code", "`plogis()`", "`pnorm()`")
)

models_table[-1, ] |> 
  flextable() |> 
  autofit() |> 
  set_header_labels(values = list(Model = "Model",
                                  Link.Function = "Equation",
                                  Link.Function.1 = "R Code",
                                  Inverse.Link.Function = "Equation",
                                  Inverse.Link.Function.1 = "R Code")) |>
  add_header_row(values = c("", "Link Function", "Link Function", "Inverse Link Function", "Inverse Link Function")) |>
  merge_h(1, part = "header") |>
  theme_booktabs(bold_header = TRUE) |>
  align(part = "all", align = "center") |> 
  ftExtra::colformat_md(part = "all") |> 
  save_as_docx(path = sprintf(here::here("paper", "tables", "%s.docx"), "notation-table"))


# DATASET EXAMPLE TABLE

b1 <- log(3) # log odds ratio
k <- 4
n <- 100
x <- rep(c("a", "b"), each = n/2)
dat <- data.frame(x = x)
probs <- rep(1/k, k) # for the group "a", uniform probabilities
dat <- sim_ord_latent(~x, By = b1, prob = probs, data = dat, link = "logit")

funs <- list(mean = mean, median = median, sd = sd)
ss <- sapply(funs, function(f) tapply(as.numeric(dat$y), dat$x, f))
ff <- t(table(dat$y, dat$x))

ss <- data.frame(cbind(ss, ff))
names(ss) <- c("mean", "median", "sd", paste0("Y", 1:4))

ss$tot <- apply(ss[, 4:ncol(ss)], 1, sum)
pp <- ss[, 4:7]/50
ss <- round(ss, 2)
ss$Y1 <- sprintf("**%s** (*p = %.2f*)", ss$Y1, pp$Y1)
ss$Y2 <- sprintf("**%s** (*p = %.2f*)", ss$Y2, pp$Y2)
ss$Y3 <- sprintf("**%s** (*p = %.2f*)", ss$Y3, pp$Y3)
ss$Y4 <- sprintf("**%s** (*p = %.2f*)", ss$Y4, pp$Y4)

ss$group <- rownames(ss)
ss <- select(ss, group, everything(), -tot)

flextable(ss) |> 
  autofit() |> 
  set_caption(caption = filor::get_caption()) |> 
  align(part = "all", align = "center") |> 
  ftExtra::colformat_md(part = "all") |> 
  save_as_docx(path = sprintf(here::here("paper", "tables", "%s.docx"), "dataset-example-table"))


# Saving

tables <- filor::named_list(
  models_table,
  data_example_table = list(dat = dat, table = ss)
)

saveRDS(tables, here::here("objects", "r-tabs.rds"))