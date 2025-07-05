# Pacotes ----

library(rvest)

# Conververtendo HTML em MD ----

rvest::read_html("Canopy Openness Index in R.html") |> 
  rvest::html_element("body") |> 
  rvest::html_text2() |> 
  writeLines("README.md")