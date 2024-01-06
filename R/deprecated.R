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
