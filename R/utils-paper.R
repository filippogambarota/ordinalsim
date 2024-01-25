tidy_clm <- function(fit, ...){
  fits <- broom::tidy(fit, ...)
  return(fits)
}

clm_table <- function(fit, truth = NULL, caption = NULL){
  require(flextable)
  require(ftExtra)
  fits <- tidy_clm(fit)
  nalpha <- length(fit$alpha)
  if(!is.null(truth)){
    fits$estimate <- sprintf("%.3f (*%.3f*)", fits$estimate, truth)
  }
  fits <- dplyr::select(fits, coef.type, dplyr::everything())
  fits$coef.type <- dplyr::case_when(
    fits$coef.type == "intercept" ~ "$\\alpha$",
    fits$coef.type == "location" ~ "$\\mu$",
    fits$coef.type == "scale" ~ "$\\sigma$",
    TRUE ~ NA
  )
  
  flextable(fits) |> 
    flextable::merge_v(1) |> 
    colformat_double(digits = 3) |> 
    autofit() |> 
    theme_vanilla() |> 
    set_header_labels(coef.type = "", 
                      term = "Parameter", 
                      estimate = "Value", 
                      std.error = "SE",
                      statistic = "z",
                      p.value = "p") |> 
    align(part = "all", align = "center") |> 
    colformat_md(part = "all") |> 
    set_caption(caption = caption)
}

z_to_x <- function(z, mean, sd){
  (z * sd) + mean
}

clm_table_old <- function(fit, truth = NULL, caption = NULL){
  require(gt)
  fits <- tidy_clm(fit)
  nalpha <- length(fit$alpha)
  if(!is.null(truth)){
    fits$estimate <- sprintf("%.3f (*%.3f*)", fits$estimate, truth)
  }
  fits <- dplyr::select(fits, coef.type, dplyr::everything())
  fits$coef.type <- dplyr::case_when(
    fits$coef.type == "intercept" ~ "$$\\alpha$$",
    fits$coef.type == "location" ~ "$$\\mu$$",
    fits$coef.type == "scale" ~ "$$\\sigma$$",
    TRUE ~ NA
  )
  fits |> 
    mutate(row = row_number(), .by = coef.type) |> 
    mutate(across(coef.type, ~ifelse(row == 1, ., ""))) |> 
    gt::gt() |> 
    gt::fmt_number(decimals = 3) |> 
    tab_caption(caption)
}

th_names <- function(k){
  sprintf("%s|%s", 1:(k-1), 2:k)
}

plot_grid_common_legend <- function(..., nrow = NULL, ncol = NULL){
  plts <- list(...)
  legend <- cowplot::get_legend(plts[[1]] + theme(legend.position = "bottom", legend.title = element_blank()))
  plts <- lapply(plts, function(p) p + theme(legend.position = "none"))
  grid <- cowplot::plot_grid(plotlist = plts)
  cowplot::plot_grid(grid, legend, nrow = nrow, ncol = ncol, rel_heights = c(0.9, 0.1))
}

mtab <- function(data, type = c("flextable", "kable", "apa"), caption = NULL, digits = 3){
  type <- match.arg(type)
  if(type == "flextable"){
    qflex(data, caption, digits)
  }else if(type == "kable"){
    qkable(data, caption, digits)
  }else{
    qapa(data, caption)
  }
}

qkable <- function(data, caption = NULL, digits = 3){
  require(kableExtra)
  data |> 
    kable(booktabs = TRUE,
          format = "latex",
          align = "c",  
          digits = digits, 
          caption = caption) |> 
    kable_styling(full_width = FALSE,
                  position = "center")
}

qflex <- function(data, caption = NULL, digits = 3){
  require(flextable)
  data |> 
    flextable() |> 
    theme_booktabs(bold_header = TRUE) |> 
    autofit() |> 
    align(part = "all", align = "center") |> 
    set_caption(caption = caption) |> 
    colformat_double(digits = digits)
}

qapa <- function(data, caption){
  require(papaja)
  data |> 
    apa_table(caption = caption)
}

rm_legend <- function(){
  theme(legend.position = "none")
}

replace_before_after <- function(x, before, after = NULL, new){
  if(is.null(after)) after <- before
  whole <- sprintf("%s.*%s", before, after)
  within <- sprintf("(?<=%s).*(?=%s)", before, after)
  within_string <- regmatches(x, regexpr(within, x, perl = TRUE))
  new_string <- sprintf(new, within_string)
  x[grepl(whole, x)] <- stringr::str_replace_all(x[grepl(whole, x)], whole, new_string)
  return(x)
}

fmt <- function(x, what, type = "markdown", digits = 2, ...){
  type <- match.arg(type, c("latex", "markdown"))
  what <- match.arg(what, c("bold", "italic"))
  if(is.numeric(x)) x <- round(x, digits)
  fun <- switch(what,
    "bold" = bold,
    "italic" = italic
  )
  fun(x, type, ...)
}

bold <- function(x, type = "markdown"){
  switch(type,
    "markdown" = sprintf("**%s**", x),
    "latex" = sprintf("\\textbf{%s}", x)
  )
}

italic <- function(x, type = "markdown"){
  switch(type,
         "markdown" = sprintf("*%s*", x),
         "latex" = sprintf("\\textit{%s}", x)
  )
}
