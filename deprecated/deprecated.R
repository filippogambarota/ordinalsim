tidy_clm <- function(fit, ...){
  fits <- broom::tidy(fit, ...)
  return(fits)
}

plot_grid_common_legend <- function(..., nrow = NULL, ncol = NULL){
  plts <- list(...)
  legend <- cowplot::get_legend(plts[[1]] + theme(legend.position = "bottom", legend.title = element_blank()))
  plts <- lapply(plts, function(p) p + theme(legend.position = "none"))
  grid <- cowplot::plot_grid(plotlist = plts)
  cowplot::plot_grid(grid, legend, nrow = nrow, ncol = ncol, rel_heights = c(0.9, 0.1))
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


get_y <- function(data = NULL, P, ordered = TRUE){
  y <- apply(P, 1, function(x) sample(names(P), 1, replace = TRUE, prob = x))
  y <- factor(y, levels = names(P), ordered = TRUE)
  if(!is.null(data)){
    cbind(data, y)
  }else{
    y
  }
}

codds <- function(p){
  co <- cumsum(p)/(1 - cumsum(p))
  co <- co[-length(co)]
  nn <- Reduce(paste0, 1:(length(p) - 1), accumulate = TRUE)
  names(co) <- paste0("o", nn)
  co
}


show_th <- function(th = NULL, probs = NULL, link = c("logit", "probit"),
                    cnames = NULL){
  link <- match.arg(link)
  if(!is.null(th) & !is.null(probs)){
    stop("Only one between th and probs need to be supplied")
  }
  
  fn <- get_link(link)
  
  if(is.null(th)){
    th <- probs_to_th(probs, link = link)
  }
  if(is.null(probs)){
    probs <- th_to_probs(th, link = link)
  }
  
  if(is.null(cnames)){
    cnames <- 1:length(probs)
  }
  
  if(sum(probs) != 1){
    stop("The vector of probabilities must sum to 1")
  }
  
  par(mfrow = c(1,3))
  curve(fn$dfun(x), -5, 5, main = "Latent Distribution",
        ylab = fn$dfun_n)
  abline(v = th)
  
  curve(fn$pfun(x), -4, 4, main = "Cumulative Probabilities",
        ylab = fn$pfun_n)
  segments(th, 0, th, fn$pfun(th))
  
  barplot(probs, ylim = c(0, 1), main = "Expected Probabilities",
          col = RColorBrewer::brewer.pal(n = length(probs), name = "RdBu"),
          names = cnames)
  
  par(mfrow = c(1,1))
}

dummy_ord <- function(y){
  if(!is.numeric(y)) y <- as.integer(y)
  yc <- sort(unique(y))
  dummy <- lapply(yc, function(t) ifelse(y <= t, 1, 0))
  nn1 <- Reduce(paste0, yc, accumulate = TRUE)
  nn2 <- Reduce(paste0, yc, accumulate = TRUE, right = TRUE)
  names(dummy) <- sprintf("y%svs%s", nn1[-length(nn1)], nn2[-1])
  data.frame(dummy[-length(dummy)])
}

clm_to_ord <- function(fit){
  th <- unname(fit$alpha) # estimated thresholds
  coefs <- fit$coefficients
  coefs <- unname(coefs[(length(th) + 1):length(coefs)])
  k <- length(th) + 1 # number of levels for y
  y <- 1:k # ordinal values
  stdm <- 0 # latent mean
  stds <- 1 # latent sigma
  th_y <- scales::rescale(th, to = c(y[1] + 0.5, y[k] - 0.5))
  lats <- (th_y[2] - th_y[k - 2]) / (th[2] - th[k - 2]) # real latent sigma
  b0 <- th_y[1] - th[1] * lats
  betas <- lats * coefs
  out <- list(sigma = lats, b0 = b0, beta = betas, alpha = th_y)
  return(out)
}

get_th <- function(b0, b1){
  -(b0/b1)
}

get_slope <- function(b1){
  1/b1
}

get_b0_from_th <- function(th, b1){
  -b1 * th
}
get_probs <- function(formula, 
                      B, 
                      probs0, 
                      data, 
                      link = c("logit", "probit"),
                      ynames = NULL,
                      append = FALSE){
  if(is.null(ynames)){
    ynames <- paste0("y", 1:length(probs0))
  }
  lf <- get_link(link)
  ths <- c(-Inf, prob_to_alpha(probs0, link = link), Inf)
  X <- model.matrix(formula, data = data)[, -1]
  X <- matrix(X)
  # notice the minus sign t - X %*% B thus an increase in x
  # is an increase in p
  P <- lapply(ths, function(t) c(lf$pfun(t - X %*% B)))
  P <- data.frame(P)
  P <- data.frame(t(apply(P, 1, diff)))
  names(P) <- ynames
  if(append){
    P <- cbind(data, P)
  }
  return(P)
}


qrender <- function(){
  rmarkdown::render("paper/paper.Rmd")
  system("quarto render")
}

mkdirif <- function(dir){
  if(!fs::is_dir(dir)){
    fs::dir_create(dir)
  }
}

render <- function(file){
  db <- getdb()
  parent <- dirname(file)
  current_md5 <- tools::md5sum(file)
  update <- !(db$md5[db$files == names(current_md5)] == current_md5)
  if(update){
    out <- rmarkdown::render(file)
    db$md5[db$files == names(current_md5)] <- current_md5
    saveRDS(db, ".dbcache/dbcache.rds")
    return(out)
  }else{
    cat("file not changed!")
  }
}

getdb <- function(){
  mkdirif(file.path(".dbcache"))
  dbpath <- here::here(".dbcache", "dbcache.rds")
  if(!fs::file_exists(dbpath)){
    files <- list.files(pattern = "Rmd", full.names = TRUE, recursive = TRUE)
    md5 <- tools::md5sum(files)
    db <- data.frame(
      file = gsub("\\./", "", names(md5)),
      md5 = md5
    )
    names(db) <- c("files", "md5")
    rownames(db) <- NULL
    saveRDS(db, dbpath)
  }
  db <- readRDS(dbpath)
}

cat_latent_plot <- function(location = 0,
                            scale = 1, 
                            alpha = NULL, 
                            prob = NULL, 
                            link = "logit"
){
  require(ggplot2)
  lf <- get_link(link)
  x <- paste0("g", 1:length(location))
  lat <- data.frame(
    x = x,
    location = location,
    scale = scale
  )
  
  if(is.null(alpha)){
    sum_to_1(prob)
    alpha <- prob_to_alpha(prob, link)
  }
  
  thl <- latex2exp::TeX(sprintf("$\\alpha_{%s}$", 1:length(alpha)))
  
  if(link == "logit"){
    dfun <- distributional::dist_logistic
    sd <- sqrt(vlogit(scale))
    ylab <- latex2exp::TeX("Logistic Distribution $Y^{*}$")
  }else{
    dfun <- distributional::dist_normal
    sd <- lat$scale
    ylab <- latex2exp::TeX("Gaussian Distribution $Y^{*}$")
  }
  lat$dist <- dfun(lat$location, lat$scale)
  range_y <- c(min(lat$location) - max(sd) * 5, max(lat$location) + max(sd) * 5)
  
  lat_plot <- ggplot(lat,
                     aes(x = factor(x), y = location, dist = dist)) +
    ggdist::stat_halfeye(aes(fill = after_stat(factor(findInterval(y, alpha) + 1))),
                         alpha = 0.85) +
    ylim(range_y) +
    geom_hline(yintercept = alpha, linetype = "dashed", col = "black", alpha = 0.7) + {
      if(length(location) > 1){
        geom_line(aes(x = factor(x), y = location, group = 1))
      }
    } +
    annotate("label", x = 0.7, y = alpha, label = thl, size = 5) +
    theme_minimal(15) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_blank()) +
    ylab(ylab)
  
  alpha <- c(-Inf, alpha, Inf)
  lat_ms <- lat[, c("location", "scale")]
  ps <- apply(lat_ms, 1, function(x) data.frame(t(diff(lf$pfun(alpha, x[1], x[2])))), simplify = FALSE)
  ps <- do.call(rbind, ps)
  names(ps) <- paste0("y", 1:ncol(ps))
  latl <- cbind(lat, ps)
  latl <- tidyr::pivot_longer(latl, starts_with("y"), names_to = "y", values_to = "value")
  probs_plot <- ggplot(latl, aes(x = factor(x), y = value, fill = y)) +
    geom_col(position = position_dodge()) +
    theme_minimal(15) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_blank()) +
    ylab("Probability") +
    ylim(c(0, 1))
  out <- list(lat_plot = lat_plot, probs_plot = probs_plot)
  suppressWarnings(print(cowplot::plot_grid(lat_plot, probs_plot, ncol = 2)))
  invisible(out)
}
