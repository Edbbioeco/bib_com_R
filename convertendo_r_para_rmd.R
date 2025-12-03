# Pacotes ----

library(rmarkdown)

# Convertendo R para RMD ----

knitr::spin(hair = "readme.R", format = "Rmd")
