library(flextable)

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

tables <- filor::named_list(
  models_table
)

saveRDS(tables, here::here("objects", "r-tabs.rds"))