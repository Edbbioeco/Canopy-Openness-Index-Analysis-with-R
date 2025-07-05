# Pacotes ----

library(pandoc)

# Conververtendo HTML em MD ----

pandoc::pandoc_convert(file = "README.html",
                       to = "gfm",
                       output = "README.md",
                       args = "--extract-media=.")
