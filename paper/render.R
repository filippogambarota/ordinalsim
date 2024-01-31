mkdirif <- function(dir, ...){
  if(!dir.exists(dir)){
    dir.create(dir, ...)
  }
}

# render paper

rmarkdown::render("paper/paper.Rmd", output_format = "all") # pdf and word

# anonymize paper

anonymize_paper("paper/paper.pdf")

# figures

figs_names <- c(
  "fig-explain-cumulative",
  "fig-logit-vs-probit",
  "fig-odds-example",
  "fig-prop-odds",
  "fig-sim-from-latent",
  "fig-show-th-example",
  "fig-example-cat-latent",
  "fig-example-num-latent",
  "fig-effects-2-by-2-interaction",
  "fig-effects-num-by-cat-interaction",
  "fig-power-curve"
)

figs <- paste0("figure", 1:length(figs_names))
names(figs) <- paste0(figs_names, "-1")

mkdirif("paper/figures-submission")
actual_figures <- list.files("paper/paper_files/figure-latex/", pattern = "*.pdf", full.names = TRUE)
fs::file_copy(actual_figures, "paper/figures-submission/")
submission_figures <- list.files("paper/figures-submission/", pattern = "*.pdf")

fs::file_move(
  file.path("paper", "figures-submission", submission_figures),
  file.path("paper", "figures-submission", sprintf("%s.pdf", figs[tools::file_path_sans_ext(submission_figures)]))
)