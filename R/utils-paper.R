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