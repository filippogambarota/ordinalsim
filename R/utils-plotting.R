#' Show the thresholds and associated probabilities
#'
#' @param prob vector of probabilities
#' @param alpha vector of thresholds
#' @param link link function (logit or probit)
#'
#' @return a list of plots
#' @export
show_alpha <- function(prob = NULL, 
                       alpha = NULL, 
                       link = "logit",
                       plot = TRUE){
  
  # not elegant, but effective
  suppressMessages(suppressWarnings(library(ggplot2)))
  
  if(is.null(prob) & is.null(alpha)){
    stop("alpha or prob must be specified!")
  }
  
  if(is.null(prob)){
    prob <- alpha_to_prob(alpha, link)
  }else{
    sum_to_1(prob)
    alpha <- prob_to_alpha(prob, link)
  }
  
  lf <- get_link(link)
  
  if(link == "logit"){
    xlim <- c(-7, 7)
    dist <- distributional::dist_logistic
  }else{
    xlim <- c(-5, 5)
    dist <- distributional::dist_normal
  }
  
  k <- length(prob)
  D <- data.frame(l = 0, s = 1)
  D$dist <- dist(D$l, D$s)
  
  alpha_n <- th_names(k)
  
  dplot <- ggplot(D) +
    ggdist::stat_slab(aes(dist = dist,
                          fill = factor(after_stat(findInterval(x, alpha) + 1))),
                      orientation = "horizontal",
                      alpha = 1) +
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
                          hjust = 1)
  
  y_n <- sprintf("P(Y = %s)", 1:k)
  y_cp <- lf$pfun(c(-Inf, alpha, Inf))
  y_p <- 0.5 * (y_cp[1:(length(y_cp) - 1)] + y_cp[2:length(y_cp)])
  
  pplot <- ggplot(data = NULL, aes(x = xlim)) +
    geom_line(stat = "function", 
              fun = lf$pfun, 
              #aes(color = factor(after_stat(findInterval(x, alpha)))),
              linewidth = 1.5) +
    geom_segment(data = NULL, aes(x = alpha, y = 0, xend = alpha, yend = lf$pfun(alpha))) +
    geom_segment(data = NULL,aes(x = -Inf, y = lf$pfun(alpha), xend = alpha, yend = lf$pfun(alpha))) +
    ggtext::geom_richtext(aes(x = alpha, y = 0, label = alpha_n), angle = 90, hjust = 0) +
    annotate("label", x = -Inf, y = y_p, label = 1:k, hjust = -0.5) +
    theme_minimal(15) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none")
  
  
  pp <- data.frame(
    y = 1:k,
    y_n = y_n,
    p = diff(y_cp)
  )
  
  hplot <- ggplot(pp, aes(x = factor(y), fill = factor(y), y = p)) +
    geom_col(width = 0.6) +
    ylab("Probability") +
    theme_minimal(15) +
    theme(legend.position = "none",
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank()) +
    ylim(c(0, 1))
  
  out <- list(latent = dplot, cumulative = pplot, probs = hplot)
  if(plot){
    print(cowplot::plot_grid(dplot, pplot, hplot, nrow = 1))
  }
  invisible(out)
}

#' Shows the effect of a categorical variable on predicted ordinal probabilities
#'
#' @param location vector of group latent locations
#' @param scale vector of group latent scales
#' @param alpha vector of threshold
#' @param prob0 vector of baseline probabilities
#' @param link 
#' @param plot 
#'
#' @return
#' @export
#'
#' @examples
cat_latent_plot <- function(location = 0,
                            scale = 1, 
                            alpha = NULL, 
                            prob0 = NULL, 
                            link = "logit",
                            plot = TRUE
){
  require(ggplot2)
  lf <- get_link(link)
  
  if(is.null(alpha)){
    sum_to_1(prob0)
    alpha <- prob_to_alpha(prob0, link = link)
  }

  data <- data.frame(x = factor(1:length(location)))
  k <- length(alpha) + 1
  if(length(location) > 1){
    beta <- abs(location[1] - location[2:length(location)])
    zeta <- log(scale[2]/scale[1])
    data <- sim_ord_latent(~x, scale = ~x, beta = beta, zeta = zeta, data = data, alpha = alpha, link = link, simulate = FALSE)
    data <- data[, !grepl("^yp", names(data))]
  }else{
    names(prob0) <- paste0("y", 1:k)
    data <- cbind(data, t(data.frame(prob0)))
  }

  data$location <- location
  data$scale <- scale
  
  thl <- latex2exp::TeX(sprintf("$\\alpha_{%s}$", 1:length(alpha)))
  
  if(link == "logit"){
    dfun <- distributional::dist_logistic
    sd <- sqrt(vlogit(scale))
    ylab <- latex2exp::TeX("Logistic Distribution $Y^{*}$")
  }else{
    dfun <- distributional::dist_normal
    sd <- data$scale
    ylab <- latex2exp::TeX("Gaussian Distribution $Y^{*}$")
  }
  
  data$dist <- dfun(data$location, data$scale)
  range_y <- c(min(data$location) - max(sd) * 5, max(data$location) + max(sd) * 5)
  
  lat_plot <- ggplot(data,
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
  
  probs_plot <- data |> 
    tidyr::pivot_longer(dplyr::starts_with("y")) |> 
    ggplot(aes(x = factor(x), y = value, fill = name)) +
    geom_col(position = position_dodge()) +
    theme_minimal(15) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.x = element_blank()) +
    ylab("Probability") +
    ylim(c(0, 1))
  
  out <- list(lat_plot = lat_plot, probs_plot = probs_plot)
  if(plot){
    suppressWarnings(print(cowplot::plot_grid(lat_plot, probs_plot, ncol = 2)))
  }
  invisible(out)
}

#' Shows the effect of a numerical variable on predicted ordinal probabilities
#' @description
#' Given the predictor `x` and a single regression parameter `b1` the function return a plot
#' of the predicted probabilities P(Y = k|X). The function return also the plot of the predicted
#' probabilities at specific `x` values. By default the 25th, 50th and 75th quantiles but custom
#' values can be provided.
#' 
#' @param x vector with the predictor
#' @param b1 regression coefficient
#' @param alpha vector of thresholds
#' @param prob0 vector of probabilities
#' @param at vector of `x` values where probabilities are calculated. If `NULL`, the function use `quantile(x, c(0.25, 0.5, 0.75))`
#' @param link link function (logit or probit)
#' @param size size of the `ggplot2::theme_minimal(base_size = )`
#' @param linewidth size of the `ggplot2::geom_line()`
#'
#' @return list of plots with the predicted probabilities and barplots at specific `x` values.
#' @export
#'
num_latent_plot <- function(x,
                            b1, 
                            alpha = NULL, 
                            prob0 = NULL,
                            at = NULL,
                            link = "logit",
                            size = 20,
                            linewidth = 1,
                            plot = TRUE){
  lf <- get_link(link)
  
  if(link == "logit"){
    title <- latex2exp::TeX(sprintf("Logit $\\beta_1 = %s$", b1)) 
  }else{
    title <- latex2exp::TeX(sprintf("Probit $\\beta_1 = %s$", b1))
  }
  
  data <- data.frame(x = seq(min(x), max(x), length.out = 1e3))
  data <- sim_ord_latent(~x, beta = b1, prob0 = prob0, data = data, link = link, simulate = FALSE)
  data <- data[, !grepl("^yp", names(data))]
  
  if(is.null(at)){
    at <- quantile(data$x, c(0.25, 0.5, 0.75))
    xq <- sapply(at, function(q) filor::closest_number(data$x, q))
  }else{
    xq <- sapply(at, function(q) filor::closest_number(data$x, q))
    names(xq) <- sprintf("~ %.2f", xq)
  }
  
  datal <- tidyr::pivot_longer(data, starts_with("y"), names_to = "y", values_to = "value")
  pplot <- ggplot(datal, aes(x = x, y = value, color = y)) +
    geom_vline(xintercept = at, linetype = "dashed") +
    geom_line(linewidth = linewidth) +
    theme_minimal(size) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ylab("Probability") +
    ggtitle(title) +
    ylim(c(0, 1))
  
  
  data_q <- data[data$x %in% xq, ]
  data_q$x <- names(xq)
  data_q <- tidyr::pivot_longer(data_q, dplyr::starts_with("y"))
  
  qplot <- ggplot(data_q, aes(x = x, y = value, fill = name)) +
    geom_col(position = position_dodge(), width = 0.5) +
    ylab("Probability") +
    xlab("x") +
    theme_minimal(size) +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    ylim(c(0, 1))
  
  out <- list(pplot = pplot, qplot = qplot)
  if(plot){
    print(cowplot::plot_grid(pplot, qplot, ncol = 1, align = "hv", rel_heights = c(0.6, 0.4)))
  }
  invisible(out)
}