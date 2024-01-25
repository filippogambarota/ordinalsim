anonymize_paper <- function(file){
  # remove title page
  qpdf::pdf_subset(file, pages = -1, output = sprintf("%s_anonymized.pdf", xfun::sans_ext(file)))
  
  # create title page
  qpdf::pdf_subset(file, pages = 1, output = sprintf("%s_title_page.pdf", xfun::sans_ext(file)))
}

render_paper <- function(){
  rmarkdown::render(here::here("paper", "paper.Rmd"), output_format = "all")
  anonymize_paper("paper/paper.pdf")
}
