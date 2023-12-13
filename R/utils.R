show_alpha <- function(prob = NULL, 
                       alpha = NULL, 
                       link = c("logit", "probit")){
  require(ggplot2)
  require(cowplot)
  require(ggtext)
  
  if(is.null(prob) & is.null(alpha)){
    stop("alpha or prob must be specified!")
  }
  
  lf <- get_link(link)
  
  if(link == "logit"){
    xlim <- c(-7, 7)
    dist <- distributional::dist_logistic
  }else{
    xlim <- c(-5, 5)
    dist <- distributional::dist_normal
  }
  
  if(is.null(prob)){
    prob <- alpha_to_prob(alpha, link)
  }else{
    alpha <- prob_to_alpha(prob, link)
  }
  
  k <- length(prob)
  D <- data.frame(l = 0, s = 1)
  D$dist <- dist(D$l, D$s)
  
  alpha_n <- th_names(k)
  
  dplot <- ggplot(D) +
    ggdist::stat_slab(aes(dist = dist,
                          fill = factor(after_stat(findInterval(x, alpha) + 1))),
                      orientation = "horizontal",
                      alpha = 0.8) +
    xlim(xlim) +
    theme_minimal(15) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    geom_vline(xintercept = alpha,
               alpha = 0.5) +
    ggtext::geom_richtext(data = data.frame(alpha, alpha_n),
                          aes(x = alpha, 
                              y = 1, 
                              label = alpha_n), 
                          angle = 90,
                          hjust = 0.5)
  
  y_n <- sprintf("P(Y = %s)", 1:k)
  y_cp <- lf$pfun(c(-Inf, alpha, Inf))
  y_p <- 0.5 * (y_cp[1:(length(y_cp) - 1)] + y_cp[2:length(y_cp)])
  
  pplot <- ggplot() +
    stat_function(geom = "line", fun = lf$pfun) +
    xlim(xlim) +
    geom_segment(aes(x = alpha, y = 0, xend = alpha, yend = lf$pfun(alpha))) +
    geom_segment(aes(x = -Inf, y = lf$pfun(alpha), xend = alpha, yend = lf$pfun(alpha))) +
    ggtext::geom_richtext(aes(x = alpha, y = 0, label = alpha_n), angle = 90, hjust = 0) +
    annotate("label", x = -Inf, y = y_p, label = 1:k, hjust = -0.5) +
    theme_minimal(15) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  pp <- data.frame(
    y = 1:k,
    y_n = y_n,
    p = diff(y_cp)
  )
  
  hplot <- ggplot(pp, aes(x = y_n, fill = factor(y), y = p)) +
    geom_col(width = 0.6) +
    ylab("Probability") +
    theme_minimal(15) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank()) +
    ylim(c(0, 1))
  
  bl <- get_legend(dplot)
  
  
  top <- plot_grid(dplot + theme(legend.position = "none"), 
                   pplot)
  bottom <- plot_grid(NULL, hplot, NULL, nrow = 1, ncol = 3, rel_widths = c(0.3, 0.4, 0.3))
  pgrid <- plot_grid(top, bottom, nrow = 2)  
  plot_grid(pgrid, bl, nrow = 2, rel_heights = c(0.9, 0.1))
  
}


alpha_to_prob <- function(th, link = c("logit", "probit"), ...){
  lf <- get_link(link)
  ths <- c(-Inf, unname(th), Inf)
  cprobs <- lf$pfun(ths, ...)
  probs <- diff(cprobs)
  return(probs)
}

prob_to_alpha <- function(probs, link = c("logit", "probit"), ...){
  lf <- get_link(link)
  cprobs <- cumsum(probs)
  lf$qfun(cprobs[-length(cprobs)], ...)
}


get_link <- function(link = c("logit", "probit")){
  link <- match.arg(link)
  if(link == "logit"){
    pfun <- plogis
    dfun <- dlogis
    qfun <- qlogis
    rfun <- rlogis
    dfun_n <- "dlogis(x)"
    pfun_n <- "plogis(x)"
  }else{
    pfun <- pnorm
    dfun <- dnorm
    qfun <- qnorm
    rfun <- rnorm
    dfun_n <- "dnorm(x)"
    pfun_n <- "pnorm(x)"
  }
  list(rfun = rfun, pfun = pfun, dfun = dfun, qfun = qfun,
       dfun_n = dfun_n, pfun_n = pfun_n)
}

sim_ord_latent <- function(location,
                           scale = NULL,
                           By,
                           Bscale = NULL,
                           probs = NULL, 
                           alphas = NULL, 
                           data, 
                           link = c("logit", "probit")){
  
  # get all the correct functions according to the link
  lf <- get_link(link)
  if(is.null(alphas)){
    # calculate thresholds if not provided
    alphas <- prob_to_alpha(probs, link = link)
  }
  k <- length(alphas) + 1 # number of ordinal outcomes
  n <- nrow(data) # number of observations
  
  # model matrix for the location effect
  Xy <- model.matrix(location, data = data)[, -1, drop = FALSE] # remove intercept
  lpy <- c(Xy %*% By) # linear predictor for the location
  lps <- 0 # default scale effect 0, exp(0) = 1 (the default scale)
  
  # check predictors on the scale parameter
  if(!is.null(scale)){
    # model matrix for the scale effect
    Xsigma <- model.matrix(scale, data = data)[, -1, drop = FALSE] # remove intercept
    lps <- c(Xsigma %*% Bscale) # linear predictor for the scale
  }
  
  # latent variable with appropriate error function
  ystar <- lf$rfun(n, lpy, exp(lps))
  
  # cut according to thresholds
  y <- findInterval(ystar, alphas) + 1 # to start from 1
  
  data$y <- ordered(y) # to ordered factor
  data$ys <- ystar # save also the latent
  return(data)
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

cat_latent_plot <- function(m = 0, 
                            s = 1, 
                            th = NULL, 
                            probs = NULL, 
                            link = c("logit", "probit"),
                            plot = c("probs", "latent", "both")){
  require(ggplot2)
  plot <- match.arg(plot)
  lf <- get_link(link)
  x <- paste0("g", 1:length(m))
  lat <- data.frame(
    x = x,
    m = m,
    s = s
  )
  
  if(is.null(th)){
    th <- prob_to_alpha(probs, link)
  }
  
  thl <- latex2exp::TeX(sprintf("$\\alpha_{%s}$", 1:length(th)))
  
  if(link == "logit"){
    dfun <- distributional::dist_logistic
    s <- sqrt(vlogit(s))
    title <- "Logistic Distribution"
  }else{
    dfun <- distributional::dist_normal
    s <- lat$s
    title <- "Normal Distribution"
  }
  lat$dist <- dfun(lat$m, lat$s)
  range_y <- c(min(lat$m) - max(s) * 5, max(lat$m) + max(s) * 5)

  lat_plot <- ggplot(lat,
         aes(x = factor(x), y = m, dist = dist)) +
    ggdist::stat_halfeye(aes(fill = after_stat(factor(findInterval(y, th) + 1))),
                 alpha = 0.85) +
    ylim(range_y) +
    geom_hline(yintercept = th, linetype = "dashed", col = "black", alpha = 0.7) +
    geom_line(aes(x = factor(x), y = m, group = 1)) +
    annotate("label", x = 0.7, y = th, label = thl, size = 5) +
    theme_minimal(15) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_blank()) +
    ylab(latex2exp::TeX("\\mu")) +
    ggtitle(title)
  
  
  th <- c(-Inf, th, Inf)
  lat_ms <- lat[, c("m", "s")]
  ps <- apply(lat_ms, 1, function(x) data.frame(t(diff(lf$pfun(th, x[1], x[2])))), simplify = FALSE)
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
    ylab("Probability")
  if(plot == "probs"){
    probs_plot
  }else if(plot == "latent"){
    lat_plot
  }else{
    legend_b <- cowplot::get_legend(
      probs_plot
    )
    plts <- cowplot::plot_grid(
      lat_plot + theme(legend.position = "none"),
      probs_plot + theme(legend.position = "none")
    )
    cowplot::plot_grid(plts, legend_b, ncol = 1, rel_heights = c(1, .1))
  }
}

num_latent_plot <- function(x, 
                            b1, 
                            th = NULL, 
                            probs = NULL,
                            link = c("logit", "probit"), 
                            nsample = 1e3, 
                            size = 20){
  lf <- get_link(link)
  if(is.null(th)){
    th <- prob_to_alpha(probs, link)
  }
  data <- data.frame(x = seq(min(x), max(x), length.out = nsample))
  data <- get_probs(~x, b1, probs, data, link, append = TRUE)
  datal <- tidyr::pivot_longer(data, starts_with("y"), names_to = "y", values_to = "value")
  ggplot(datal, aes(x = x, y = value, color = y)) +
    geom_line() +
    theme_minimal(size) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ylab("Probability") +
    ggtitle(latex2exp::TeX(sprintf("$\\beta_1 = %s$", b1)))
}

vlogit <- function(scale = 1){
  (scale^2 * pi^2)/3
}

codds <- function(p){
  co <- cumsum(p)/(1 - cumsum(p))
  co <- co[-length(co)]
  nn <- Reduce(paste0, 1:(length(p) - 1), accumulate = TRUE)
  names(co) <- paste0("o", nn)
  co
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