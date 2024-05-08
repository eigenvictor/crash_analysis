
knitr::opts_knit$set(root.dir = getwd())
rmarkdown::render(
  "R/mot_report.Rmd",
  output_file = "mot_report.pptx",
  output_dir = "reports")
