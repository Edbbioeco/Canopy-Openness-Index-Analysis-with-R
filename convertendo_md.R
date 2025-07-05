# Pacotes ----

library(rmarkdown)

# Conververtendo HTML em MD ----

render("README.html", 
       output_format = "github_document", 
       output_file = "README.md") 

