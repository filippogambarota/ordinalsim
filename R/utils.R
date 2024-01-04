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

vlogit <- function(scale = 1){
  (scale^2 * pi^2)/3
}

#' Simulating Ordinal Data
#'
#' @param location a formula with predictors for the location parameter. The components of the formula need to be columns of the \code{data} object.
#' @param scale a formula with predictors for the scale parameter. Default to \code{NULL} thus if not specified the scale is assumed to be 1.
#' @param By vector of regression coefficients for the location formula. The order is the same as returned by running \code{model.matrix(~formula, data = data)}
#' @param Bscale vector of regression coefficients for the scale formula. Default to \code{NULL} thus if not specified the scale is assumed to be 1.
#' @param prob vector of baseline probabilities from which thresholds are calculated. They have a role similar to intercepts in standard regression modelling. The probabilities are fixed assuming all predictors to 0.
#' @param alpha vector of baseline thresholds. Either \code{probs} or \code{alpha} must be specified. The function calculated thresholds from \code{probs} and viceversa
#' @param data a dataframe with predictors. both \code{location} and \code{scale} formulas refers to columns of the dataframe.
#' @param link character vector indicating the link function. Can be \code{logit} or \code{probit}.
#'
#' @return a dataframe with the ordinal variable \code{y} and the latent variable \code{ys}
#' @export
#'
#' @examples
#' # simulate a group difference in probit scale of 0.5
#' set.seed(123)
#' n <- 100
#' k <- 5 # 5 ordinal values
#' bprobs <- rep(1/5, 5) # baseline probabilities for the group 0
#' dat <- data.frame(x = rep(c(0, 1), each = n))
#' dat <- sim_ord_latent(~x, By = 0.5, prob = bprobs, data = dat, link = "probit")
sim_ord_latent <- function(location,
                           scale = NULL,
                           By,
                           Bscale = NULL,
                           prob = NULL, 
                           alpha = NULL, 
                           data, 
                           link = c("logit", "probit")){
  
  # get all the correct functions according to the link
  lf <- get_link(link)
  if(is.null(alpha)){
    # calculate thresholds if not provided
    alpha <- prob_to_alpha(prob, link = link)
  }
  k <- length(alpha) + 1 # number of ordinal outcomes
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
  y <- findInterval(ystar, alpha) + 1 # to start from 1
  
  data$y <- ordered(y) # to ordered factor
  data$ys <- ystar # save also the latent
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
alpha_to_prob <- function(alpha, link = c("logit", "probit"), ...){
  lf <- get_link(link)
  ths <- c(-Inf, unname(alpha), Inf)
  cprobs <- lf$pfun(ths, ...)
  diff(cprobs)
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
prob_to_alpha <- function(prob, link = c("logit", "probit"), ...){
  lf <- get_link(link)
  cprobs <- cumsum(prob)
  lf$qfun(cprobs[-length(cprobs)], ...)
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
get_link <- function(link = c("logit", "probit")){
  link <- match.arg(link)
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
  # nothing for numeric and integers, safe for (ordered) factors
  y <- as.integer(y)
  yc <- sort(unique(y))
  dummy <- lapply(yc, function(t) ifelse(y <= t, 1, 0))
  nn1 <- Reduce(paste0, yc, accumulate = TRUE)
  nn2 <- Reduce(paste0, yc, accumulate = TRUE, right = TRUE)
  names(dummy) <- sprintf("y%svs%s", nn1[-length(nn1)], nn2[-1])
  data.frame(dummy[-length(dummy)])
}