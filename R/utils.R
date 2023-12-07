th_to_probs <- function(th, link = c("logit", "probit"), ...){
  link <- match.arg(link)
  if(link == "logit"){
    lfun <- plogis
  }else{
    lfun <- pnorm
  }
  ths <- c(-Inf, unname(th), Inf)
  cprobs <- lfun(ths, ...)
  probs <- diff(cprobs)
  return(probs)
}

probs_to_th <- function(probs, link = c("logit", "probit")){
  link <- match.arg(link)
  if(link == "logit"){
    lfun <- qlogis
  }else{
    lfun <- qnorm
  }
  th <- lfun(Reduce(sum, probs, accumulate = TRUE))
  th[-length(th)]
}

# get_probs <- function(formula, 
#                       B, 
#                       probs = NULL, 
#                       th = NULL, 
#                       ynames = NULL, 
#                       data, 
#                       get_lp = FALSE, 
#                       link = c("logit", "probit")){
#   link <- match.arg(link)
#   X <- model.matrix(formula, data = data)
#   if(is.null(th)){
#     th <- probs_to_th(probs, link = "probit")
#   }
#   probs <- th_to_probs(th, "probit")
#   
#   pnorm(as.vector(B %*% X[, -1]), mean = as.vector(B %*% X[, -1]))
#   
#   
#   
#   
#   
#   
#   lp <- lapply(ths, function(x) as.vector(pnorm(X %*% c(x, B))))
#   lp <- data.frame(lp)
#   probs <- data.frame(t(diff(t(lp))))
#   
#   
#   
#   
#   return(probs)
# }

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

sim_ord_latent <- function(formula,
                           sigma = NULL,
                           By,
                           Bsigma = NULL,
                           probs = NULL, 
                           th = NULL, 
                           data, 
                           link = c("logit", "probit")){
  lf <- get_link(link)
  if(is.null(th)){
    th <- probs_to_th(probs, link = link)
  }
  probs <- th_to_probs(th, link = link)
  k <- length(probs) # number of options
  n <- nrow(data)
  Xy <- model.matrix(formula, data = data)[, -1, drop = FALSE] # remove intercept
  lpy <- c(Xy %*% By)
  if(is.null(sigma)){
    ystar <- lf$rfun(n, lpy)
  }else{
    Xsigma <- model.matrix(sigma, data = data)[, -1, drop = FALSE] # remove intercept
    ystar <- lf$rfun(n, lpy, exp(c(Xsigma %*% Bsigma)))
  }
  y <- findInterval(ystar, th) + 1 # to start from 1
  data$y <- factor(y)
  data$y <- ordered(data$y)
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
  latl <- pivot_longer(latl, starts_with("y"), names_to = "y", values_to = "value")
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
    legend_b <- get_legend(
      probs_plot
    )
    plts <- plot_grid(
      lat_plot + theme(legend.position = "none"),
      probs_plot + theme(legend.position = "none")
    )
    plot_grid(plts, legend_b, ncol = 1, rel_heights = c(1, .1))
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
