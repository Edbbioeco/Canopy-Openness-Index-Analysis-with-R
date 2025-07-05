# Pacotes ----

library(rmarkdown)

# Conververtendo HTML em MD ----

render("README.html", 
       output_format = "github_document", 
       output_file = "README.md")

library(rvest)

# Ler o HTML
html <- read_html("README.html")

# Extrair todas as imagens
img_tags <- html %>% html_nodes("img")
img_src <- html_attr(img_tags, "src")
img_alt <- html_attr(img_tags, "alt")

# Criar diretório para imagens
if (!dir.exists("images")) dir.create("images")

# Baixar imagens
for(i in seq_along(img_src)){
  
  if(!is.na(img_src[i])) {
    img_url <- img_src[i]
    img_name <- basename(img_url)
    img_path <- file.path("images", img_name)
    
    tryCatch({
      download.file(img_url, img_path, mode = "wb")
      message("Imagem salva: ", img_path)
    }, error = function(e) {
      message("Erro ao baixar: ", img_url)
    })
  }
  
}
