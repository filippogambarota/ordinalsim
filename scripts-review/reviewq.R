tab <- with(wine, table(contact, rating))
dat <- data.frame(freq =c(tab),
                  contact=rep(c("no", "yes"), 5),
                  rating = factor(rep(1:5, each=2), ordered=TRUE))
thresholds <- 1:4
cum.rate <- as.vector(sapply(thresholds, function(x) dat$rating <= x))
rating.factor <- gl(n=length(thresholds), k=nrow(dat),
                    length=nrow(dat) * length(thresholds))
thres.X <- model.matrix(~ rating.factor - 1)
colnames(thres.X) <- paste("t", thresholds, sep="")
old.X <- -model.matrix(~contact, dat)[, -1, drop=FALSE]
new.X <- kronecker(matrix(rep(1, length(thresholds)), nc = 1), old.X)
weights <- kronecker(matrix(rep(1, length(thresholds)), nc = 1), dat$freq)
new.X <- cbind(thres.X, new.X)
colnames(new.X)[-seq(length(thresholds))] <- colnames(old.X)
p.df <- cbind(cum.rate = 1*cum.rate, as.data.frame(new.X), weights)
p.df

# John K. Kruschke, 2014-Nov-21.
# This function is described at the blog post
# http://doingbayesiandataanalysis.blogspot.com/2014/11/ordinal-probit-regression-transforming.html
# A PDF copy of the blog post is available at
# https://osf.io/fc6zd/
# This function comes with no warranties, guarantees, etc. Use at your own risk.
polrToOrdScale = function( polrObject ) {
  polrThresh = polrObject$zeta
  polrSlopes = polrObject$coefficients
  polrInter = 0.0
  polrSigma = 1.0
  K = length(polrThresh) + 1  # K is number of ordinal levels
  sigmaMult = unname( (K-2)/(polrThresh[K-1]-polrThresh[1]) )
  inter = unname( 1.5 - ( sigmaMult*polrThresh[1] ) )
  respThresh = sigmaMult*polrThresh + inter
  respSigma = sigmaMult*polrSigma
  respB0 = sigmaMult*polrInter + inter
  respSlopes = sigmaMult*polrSlopes
  return( list( sigma=respSigma ,
                b0=respB0 ,
                coefficients=respSlopes ,
                zeta=respThresh ) )
}

fit <- MASS::polr(y ~ x, data = dat, method = "probit")



polrToOrdScale(fit)

diff(tapply(as.numeric(dat$y), dat$x, mean))


sd(dat$ys)


n <- 1e5
dat <- data.frame(
  x = rep(c(0, 1), each = n)
)

dat <- sim_ord_latent(~x, By = 1, probs = rep(1/5, 5), link = "probit", data = dat)

tapply(dat$ys, dat$x, mean)
tapply(dat$ys, dat$x, sd)

dat$ys

z1 <- rnorm(1e5)
z2 <- rnorm(1e5, 1, 1)

d <- 1
s <- 10
m1 <- 10
m2 <- m1 + s * d

(m1 - m2)/s



summary(fit)
fit$info


theta <- seq(1.5, 6.5, 1)
z <- (theta - 1)/2.5

pnorm(c(-Inf, theta, Inf), 1, 2.5) |> diff()

pnorm(c(-Inf, z, Inf)) |> diff()

k <- length(z) + 1
(k-2)/(z[k-1]-z[1])

clm_to_ord <- function(fit){
  # adaptation of John K. Kruschke's function
  # the original function is described at the blog post
  # http://doingbayesiandataanalysis.blogspot.com/2014/11/ordinal-probit-regression-transforming.html
  # A PDF copy of the blog post is available at
  # https://osf.io/fc6zd/
  # This function comes with no warranties, guarantees, etc. Use at your own risk.
  
  link <- fit$link
  lf <- get_link(link)
  th <- as.vector(unname(fit$Theta))
  mu <- 0
  s <- if(link == "logit") pi^2/sqrt(3) else 1
  k <- length(th) + 1  # K is number of ordinal levels
  s_ord <- (k-2)/(th[k-1] - th[1]) # standard deviation latent ordinal
  inter <- 1.5 - (s_ord * th[1])
  respThresh = sigmaMult*polrThresh + inter
  respSigma = sigmaMult*polrSigma
  respB0 = sigmaMult*polrInter + inter
  respSlopes = sigmaMult*polrSlopes
}


th <- probs_to_th(rep(1/4, 4), link = "probit")
k <- length(th) + 1

(k-2)/(th[k-1] - th[1])

fit <- clm(y ~ x, data = dat, link = "probit")
summary(fit)

library(ggplot2)

ggplot(dat, aes(x = y, y = ))




