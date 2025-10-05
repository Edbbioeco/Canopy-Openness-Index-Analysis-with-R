# Pacotes ----

library(quarto)

# Conververtendo HTML em MD ----

quarto::quarto_render("Canopy Openess Index in R.qmd",
                      output_file = "Canopy Openess Index in R.md")

system("git rebase --continue")

system("git add Canopy Openess Index in R.html")

system("git commit")
system("git commit -m 'Merge branch origin/main into main'")
system("git commit")
