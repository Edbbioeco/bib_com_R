# Pacotes ----

library(rmarkdown)

# Convertendo R para RMD ----

rmarkdown::render("README.Rmd",
                  rmarkdown::md_document(variant = "gfm"),
                  output_options = list(fig_path = "man/figures/"))
#
