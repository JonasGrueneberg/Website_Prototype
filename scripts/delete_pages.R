
ordner <- "projects"

# Liste aller .qmd-Dateien im Ordner und allen Unterordnern
qmd_dateien <- list.files(
  path = ordner,
  pattern = "\\.qmd$",
  full.names = TRUE,
  recursive = TRUE
)

# Dateien löschen
if (length(qmd_dateien) > 0) {
  file.remove(qmd_dateien)
  cat(length(qmd_dateien), " .qmd Dateien wurden gelöscht.\n")
} else {
  cat("Keine .qmd Dateien gefunden.\n")
}
