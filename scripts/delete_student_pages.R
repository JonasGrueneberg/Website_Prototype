#!/usr/bin/env Rscript

# Basisverzeichnis für Projekte
ROOT <- normalizePath(".", winslash = "/")
PROJECTS_ROOT <- file.path(ROOT, "projects")

if (!dir.exists(PROJECTS_ROOT)) {
  stop("Verzeichnis nicht gefunden: ", PROJECTS_ROOT)
}

# Alle student_projects-Ordner finden
student_dirs <- list.dirs(PROJECTS_ROOT, recursive = TRUE, full.names = TRUE)
student_dirs <- student_dirs[basename(student_dirs) != "student_projects" & grepl("/student_projects/", student_dirs)]

# Alle .qmd-Dateien in diesen Ordnern sammeln
qmd_files <- unlist(lapply(student_dirs, function(d) {
  list.files(d, pattern = "\\.qmd$", full.names = TRUE, ignore.case = TRUE)
}))

if (length(qmd_files) == 0) {
  message("Keine .qmd-Dateien in student_projects-Ordnern gefunden.")
} else {
  message("Lösche ", length(qmd_files), " .qmd-Datei(en):")
  for (f in qmd_files) {
    message(" - ", f)
    unlink(f)
  }
  message("Fertig.")
}
