show_alpha <- function(prob = NULL, 
                       alpha = NULL, 
                       link = c("logit", "probit"),
                       plot = c("latent", "cumulative", "probs")){
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
    if(sum(prob) != 1){
      stop("the prob vector must sum to 1!")
    }
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
  
  hplot <- ggplot(pp, aes(x = y, fill = factor(y), y = p)) +
    geom_col(width = 0.6) +
    ylab("Probability") +
    theme_minimal(15) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ylim(c(0, 1))
  
  plist <- list(latent = dplot, cumulative = pplot, probs = hplot)
  plist <- plist[plot]
  bl <- get_legend(dplot)
  
  pgrid <- plot_grid(plotlist = lapply(plist, function(x) x + theme(legend.position = "none")))
  plot_grid(pgrid, bl, nrow = 2, rel_heights = c(0.9, 0.1))
}

cat_latent_plot <- function(m = 0, 
                            s = 1, 
                            th = NULL, 
                            probs = NULL, 
                            link = c("logit", "probit"),
                            plot = c("probs", "latent", "both"),
                            title = NULL){
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
    title <- if(is.null(title)) "Logistic Distribution" else title
  }else{
    dfun <- distributional::dist_normal
    s <- lat$s
    title <- if(is.null(title)) "Normal Distribution" else title
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
    ylab("Probability") +
    ylim(c(0, 1))
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
    ggtitle(latex2exp::TeX(sprintf("$\\beta_1 = %s$", b1))) +
    ylim(c(0, 1))
}