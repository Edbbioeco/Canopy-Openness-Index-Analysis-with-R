# Pacotes ----

library(rmarkdown)

# Conververtendo HTML em MD ----

rmarkdown::pandoc_convert(input = "Canopy Openness Index in R.html",
                          to = "markdown", 
                          output = "README.md")
