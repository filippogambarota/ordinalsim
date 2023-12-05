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
  if(link == "logit"){
    pfun <- plogis
    dfun <- dlogis
    qfun <- qlogis
    dfun_n <- "dlogis(x)"
    pfun_n <- "plogis(x)"
  }else{
    pfun <- pnorm
    dfun <- dnorm
    qfun <- qnorm
    dfun_n <- "dnorm(x)"
    pfun_n <- "pnorm(x)"
  }
  list(pfun = pfun, dfun = dfun, qfun = qfun,
       dfun_n = dfun_n, pfun_n = pfun_n)
}

sim_ord_latent <- function(formula, 
                           B, 
                           probs = NULL, 
                           th = NULL, 
                           ynames = NULL, 
                           data, 
                           link = c("logit", "probit")){
  link <- match.arg(link)
  if(link == "logit"){
    rdata <- rlogis
  }else{
    rdata <- rnorm
  }
  if(is.null(th)){
    th <- probs_to_th(probs, link = link)
  }
  probs <- th_to_probs(th)
  k <- length(probs) # number of options
  n <- nrow(data)
  X <- model.matrix(formula, data = data)[, 2] # remove intercept
  ystar <- rdata(n, B %*% X)
  y <- findInterval(ystar, th)
  if(is.null(names(probs)) & is.null(ynames)){
    ynames <- 1:k
  }
  data$y <- factor(y, labels = ynames)
  return(data)
}

get_probs <- function(formula, B, probs0, data, append = FALSE, link = c("logit", "probit")){
  link <- match.arg(link)
  ths <- probs_to_th(probs0, link)
  X <- model.matrix(formula, data = data)
  lp <- c(B %*% X[, -1])
  p <- lapply(lp, function(l) {
    ps <- diff(pnorm(c(-Inf, ths, Inf), l))
    names(ps) <- paste0("y", 1:length(ps))
    t(data.frame(ps))
  })
  p <- do.call(rbind, p)
  if(append){
    out <- cbind(data, p)
  }else{
    out <- p
  }
  rownames(out) <- NULL
  out
}