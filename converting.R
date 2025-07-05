library(rvest)
library(xml2)

# Ler o HTML
html <- read_html("README.html")

# Extrair todas as imagens
images <- html %>% html_nodes("img")
img_src <- html_attr(images, "src")
img_alt <- html_attr(images, "alt")

# Criar um diretório para as imagens (se necessário)
if (!dir.exists("images")) dir.create("images")

# Baixar e salvar as imagens localmente (opcional)
purrr::walk2(img_src, img_alt, ~ {
  if (!is.na(.x)) {
    dest <- file.path("images", basename(.x))
    download.file(.x, dest, mode = "wb")
  }
})

# Substituir as tags <img> no HTML por sintaxe Markdown
for (i in seq_along(images)) {
  img_md <- ifelse(
    is.na(img_alt[i]),
    sprintf("![](%s)", img_src[i]),
    sprintf("![%s](%s)", img_alt[i], img_src[i])
  )
  xml_replace(images[[i]], read_xml(sprintf("<text>%s</text>", img_md)))
}

# Extrair o conteúdo do body e salvar como Markdown
content <- html %>% 
  html_nodes("body") %>% 
  as.character()

# Remover tags HTML indesejadas (opcional)
content <- gsub("<[^>]+>", "", content) # Isso remove todas as tags, mas pode não ser ideal

# Salvar como README.md
writeLines(content, "README.md")