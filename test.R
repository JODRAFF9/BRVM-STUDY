library(rvest)

url <- "https://www.brvm.org/fr/cours-actions/0"
page <- read_html(url)

# Sélectionner uniquement le grand tableau
data_brvm <- page %>%
  html_node("section#block-system-main table.table-striped") %>%
  html_table(fill = TRUE)

# Vérifier le résultat
head(data_brvm)

# Sauvegarder
write.csv(data_brvm, "data_brvm.csv", row.names = FALSE)
cat("Grand tableau sauvegardé dans data_brvm.csv\n")
