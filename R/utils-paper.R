rm_legend <- function(){
  theme(legend.position = "none")
}

fmt <- function(x, what, type = "markdown", digits = 2, ...){
  type <- match.arg(type, c("latex", "markdown"))
  what <- match.arg(what, c("bold", "italic"))
  if(is.numeric(x)) x <- round(x, digits)
  fun <- switch(what,
    "bold" = bold,
    "italic" = italic
  )
  fun(x, type, ...)
}

bold <- function(x, type = "markdown"){
  switch(type,
    "markdown" = sprintf("**%s**", x),
    "latex" = sprintf("\\textbf{%s}", x)
  )
}

italic <- function(x, type = "markdown"){
  switch(type,
         "markdown" = sprintf("*%s*", x),
         "latex" = sprintf("\\textit{%s}", x)
  )
}
