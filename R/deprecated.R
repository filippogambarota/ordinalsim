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
