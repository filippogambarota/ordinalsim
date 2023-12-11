th_to_probs <- function(th, link = c("logit", "probit"), ...){
  lf <- get_link(link)
  ths <- c(-Inf, unname(th), Inf)
  cprobs <- lf$pfun(ths, ...)
  probs <- diff(cprobs)
  return(probs)
}

probs_to_th <- function(probs, link = c("logit", "probit"), ...){
  lf <- get_link(link)
  cprobs <- cumsum(probs)
  lf$qfun(cprobs[-length(cprobs)], ...)
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
  
  if(sum(probs) != 1L){
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
    alphas <- probs_to_th(probs, link = link)
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
  ths <- c(-Inf, probs_to_th(probs0, link = link), Inf)
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
    th <- probs_to_th(probs, link)
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
    th <- probs_to_th(probs, link)
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

vlogit <- function(scale){
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