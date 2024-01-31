#' Simulating Ordinal Data
#'
#' @param location a formula with predictors for the location parameter. The components of the formula need to be columns of the \code{data} object.
#' @param scale a formula with predictors for the scale parameter. Default to \code{NULL} thus if not specified the scale is assumed to be 1.
#' @param beta vector of regression coefficients for the location formula. The order is the same as returned by running \code{model.matrix(~formula, data = data)}
#' @param zeta vector of regression coefficients for the scale formula. Default to \code{NULL} thus if not specified the scale is assumed to be 1.
#' @param prob0 vector of baseline probabilities from which thresholds are calculated. They have a role similar to intercepts in standard regression modelling. The probabilities are fixed assuming all predictors to 0.
#' @param alpha vector of baseline thresholds. Either \code{probs} or \code{alpha} must be specified. The function calculated thresholds from \code{probs} and viceversa
#' @param data a dataframe with predictors. both \code{location} and \code{scale} formulas refers to columns of the dataframe.
#' @param link character vector indicating the link function. Can be \code{logit} or \code{probit}.
#' @param simulate logical indicating if sampling values (`TRUE`) or returning the true probabilities 
#'
#' @return a dataframe with the ordinal variable \code{y} and the latent variable \code{ys}
#' @export
#'
#' @examples
#' # simulate a group difference in probit scale of 0.5
#' set.seed(123)
#' n <- 100
#' k <- 5 # 5 ordinal values
#' probs0 <- rep(1/k, k) # baseline probabilities for the group 0
#' dat <- data.frame(x = rep(c(0, 1), each = n))
#' dat <- sim_ord_latent(~x, beta = 0.5, prob0 = probs0, data = dat, link = "probit")
sim_ord_latent <- function(location,
                           scale = NULL,
                           beta,
                           zeta = NULL,
                           prob0 = NULL, 
                           alpha = NULL, 
                           data, 
                           link = "logit",
                           simulate = TRUE){
  
  # get all the correct functions according to the link
  lf <- get_link(link)
  if(is.null(alpha)){
    # calculate thresholds if not provided
    alpha <- prob_to_alpha(prob0, link = link)
  }
  k <- length(alpha) + 1 # number of ordinal outcomes
  n <- nrow(data) # number of observations
  
  # model matrix for the location effect
  Xloc <- model.matrix(location, data = data)[, -1, drop = FALSE] # remove intercept
  lploc <- c(Xloc %*% beta) # linear predictor for the location
  lpscale <- 0 # default scale effect 0, exp(0) = 1 (the default scale)
  
  # check predictors on the scale parameter
  if(!is.null(scale)){
    # model matrix for the scale effect
    Xscale <- model.matrix(scale, data = data)[, -1, drop = FALSE] # remove intercept
    lpscale <- c(Xscale %*% zeta) # linear predictor for the scale
  }
  
  if(simulate){
    # latent variable with appropriate error function
    ystar <- lf$rfun(n, lploc, exp(lpscale))
    
    # cut according to thresholds
    y <- findInterval(ystar, alpha) + 1 # to start from 1
    
    data$y <- ordered(y) # to ordered factor
    data$ys <- ystar # save also the latent
  } else{
    alpha <- c(-Inf, alpha, Inf)
    cp <- lapply(alpha, function(a) lf$pfun((a - lploc) / exp(lpscale)))
    cp <- data.frame(do.call(cbind, cp))
    p <- data.frame(t(apply(cp, 1, diff)))
    names(p) <- paste0("y", 1:ncol(p))
    cp <- cp[, 2:k]
    names(cp) <- paste0("yp", Reduce(paste0, 1:(k - 1), accumulate = TRUE))
    data <- cbind(data, cp, p)
  }
  return(data)
}

#' Transform threshold into probabilities
#'
#' @param alpha a vector of thresholds
#' @param link the link function. \code{logit} or \code{probit}
#' @param ... other arguments passed to the appropriate cumulative distribution function (\code{pnorm} or \code{plogis})
#'
#' @return a vector of probabilities of length \code{length(alpha) + 1}
#' @export
#'
#' @examples
#' alpha <- c(-2, -0.5, 1, 1.5)
#' alpha_to_prob(alpha, link = "logit")
#' alpha_to_prob(alpha, link = "probit")
alpha_to_prob <- function(alpha, link = "logit", ...){
  lf <- get_link(link)
  ths <- c(-Inf, unname(alpha), Inf)
  cprobs <- lf$pfun(ths, ...)
  prob <- diff(cprobs)
  names(prob) <- paste0("p", 1:length(prob))
  return(prob)
}

#' Transform probabilities into thresholds
#'
#' @param prob a vector of probabilities 
#' @param link the link function. \code{logit} or \code{probit}
#' @param ... other arguments passed to the appropriate inverse cumulative distribution function (\code{qnorm} or \code{qlogis})
#' @return a vector of thresholds of length \code{length(prob) - 1}
#' @export
#'
#' @examples
#' prob <- c(0.5, 0.3, 0.1, 0.1)
#' prob_to_alpha(probs, link = "logit")
#' prob_to_alpha(probs, link = "probit")
prob_to_alpha <- function(prob, link = "logit", ...){
  sum_to_1(prob)
  k <- length(prob)
  lf <- get_link(link)
  cprobs <- cumsum(prob)
  alpha <- lf$qfun(cprobs[-length(cprobs)], ...)
  names(alpha) <- th_names(k)
  return(alpha)
}

#' List of probability functions
#'
#' @param link the link function. \code{logit} or \code{probit}
#'
#' @return a list with the data generation function, cumulative and inverse cumulative probability function and the density function.
#' @export
#'
#' @examples
#' str(get_link("logit"))
#' str(get_link("probit"))
get_link <- function(link = "logit"){
  link <- match.arg(link, choices = c("logit", "probit"))
  if(link == "logit"){
    pfun <- plogis
    dfun <- dlogis
    qfun <- qlogis
    rfun <- rlogis
  }else{
    pfun <- pnorm
    dfun <- dnorm
    qfun <- qnorm
    rfun <- rnorm
  }
  list(rfun = rfun, pfun = pfun, dfun = dfun, qfun = qfun)
}

#' Create k - 1 dummy variables from an ordinal variable
#' @description
#' Given an ordinal variable with `k` levels the function return a data.frame with k - 1 dummy variables.
#'
#' @param y a vector. Can be numeric, integer or (ordered) factor. If `y` is a factor, the function extract the underlying integer representation.
#'
#' @return a data.frame with k - 1 dummy variables.
#' @export
#'
#' @examples
#' k <- 4
#' y <- sample(1:k, size = 10, replace = TRUE)
#' dummy_ord(y)
dummy_ord <- function(y){
  if(!all(is.numeric(y) | is.integer(y) | is.factor(y))){
    stop("y need to be numeric, integer or (ordered) factor")
  }
  if(length(unique(y)) < 2){
    stop("y need to have at least 2 levels!")
  }
  # nothing for numeric and integers, safe for (ordered) factors
  y <- as.integer(y)
  yc <- sort(unique(y))
  dummy <- lapply(yc, function(t) ifelse(y <= t, 1, 0))
  nn1 <- Reduce(paste0, yc, accumulate = TRUE)
  nn2 <- Reduce(paste0, yc, accumulate = TRUE, right = TRUE)
  names(dummy) <- sprintf("y%svs%s", nn1[-length(nn1)], nn2[-1])
  data.frame(dummy[-length(dummy)])
}

#' Calculate the variance of a logistic distribution
#'
#' @param scale the scale of the logistic distribution. Default to `1`
#'
#' @return variance of the logistic distribution
#' @export
#'
#' @examples
#' vlogit(scale = 1)
#' vlogit(scale = 2)
vlogit <- function(scale = 1){
  (scale^2 * pi^2)/3
}

#' Assing names to thresholds
#' @description
#' Give intuitive names to thresholds in the form "k - 1|k".
#' 
#' @param k number of ordinal outcomes
#'
#' @return
#' @export
#'
#' @examples
#' th_names(5)
th_names <- function(k){
  sprintf("%s|%s", 1:(k-1), 2:k)
}

#' Check if the vector of probabilities sum to 1
#' 
sum_to_1 <- function(probs){
  if(sum(probs) != 1){
    stop("probs must sum to 1!")
  }
}