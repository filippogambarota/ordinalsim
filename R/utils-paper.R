tidy_clm <- function(fit, ...){
  fits <- broom::tidy(fit, ...)
  return(fits)
}

clm_table <- function(fit, truth = NULL){
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
    colformat_md(part = "all")
}

z_to_x <- function(z, mean, sd){
  (z * sd) + mean
}



th_names <- function(k){
  sprintf("%s|%s", 1:(k-1), 2:k)
}