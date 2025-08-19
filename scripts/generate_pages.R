#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("yaml", quietly = TRUE)) {
    install.packages("yaml", repos = "https://cloud.r-project.org")
  }
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# Basisverzeichnisse
ROOT <- normalizePath(".", winslash = "/")
PROJECTS_ROOT <- file.path(ROOT, "projects")

# Defaults (ohne feste HTML/MP4-Dateinamen)
DEFAULTS <- list(
  project = list(
    type = "descriptive",
    semester = "WS 25-26",
    categories = character(0),
    description = "Kurze Beschreibung des Projekts und Motivation"
  ),
  student = list(
    authors = character(0)
  )
)

slug_to_title <- function(s) {
  s <- gsub("[-_]+", " ", s)
  s <- trimws(s)
  tools::toTitleCase(s)
}

read_meta <- function(path) {
  if (file.exists(path)) {
    m <- yaml::yaml.load_file(path)
    if (is.null(m)) list() else m
  } else list()
}

yaml_vec <- function(x) {
  if (length(x) == 0) "[]"
  else paste0("[", paste(sprintf('"%s"', x), collapse = ", "), "]")
}

# Sucht im Student:innen-Ordner nach "Projekt_<Projektname>.html|.htm" (case-insensitive)
find_student_html <- function(sdir, project_name) {
  files <- list.files(sdir, full.names = TRUE)
  if (!length(files)) return(NULL)
  bn <- tolower(basename(files))
  wanted <- c(
    paste0("projekt_", tolower(project_name), ".html"),
    paste0("projekt_", tolower(project_name), ".htm")
  )
  idx <- which(bn %in% wanted)
  if (length(idx)) basename(files[idx[1]]) else NULL
}

# Findet genau eine .mp4 im Ordner (Dateiname egal); bei mehreren wird "Screencast.mp4" bevorzugt
find_student_mp4 <- function(sdir) {
  mp4s <- list.files(sdir, pattern = "\\.mp4$", full.names = FALSE, ignore.case = TRUE)
  if (length(mp4s) == 1) return(mp4s[1])
  if (length(mp4s) > 1) {
    pref <- mp4s[tolower(mp4s) == "screencast.mp4"]
    if (length(pref) >= 1) return(pref[1])
    return(mp4s[1])
  }
  NULL
}

assets_section <- function(html_rel = NULL, mp4_rel = NULL) {
  parts <- character()
  if (!is.null(mp4_rel)) {
    parts <- c(
      parts,
      "**Screencast**", "",
      sprintf('<video controls width="100%%" style="margin-bottom: 2em;"><source src="%s" type="video/mp4"></video>', mp4_rel)
    )
  }
  if (!is.null(html_rel)) {
    parts <- c(
      parts,
      "**Ausarbeitung**", "",
      sprintf('<iframe src="%s" loading="lazy" width="100%%" height="1200" style="border:0"></iframe>', html_rel)
    )
  }
  if (length(parts) == 0) {
    parts <- c("> TODO: Lege `Projekt_<Projektname>.html` und/oder genau **eine** `.mp4` in diesen Ordner, dann wird die Einbettung automatisch angezeigt.")
  }
  paste(parts, collapse = "\n")
}

write_if_missing <- function(path, content) {
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    con <- file(path, open = "w+", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(content, con, useBytes = TRUE)
    message("created: ", path)
  } else {
    message("exists:  ", path)
  }
}

generate_project_page <- function(pdir) {
  pname <- basename(pdir)
  meta <- DEFAULTS$project
  m <- read_meta(file.path(pdir, "meta.yml"))
  for (k in names(m)) meta[[k]] <- m[[k]]
  
  front <- sprintf(
    paste(
      "---",
      'title: "%s"',
      'type: "%s"',
      'semester: "%s"',
      "categories: %s",
      "listing:",
      "  id: student_projects",
      '  contents: ["student_projects/*/*.qmd"]',
      "  type: table",
      "  sort: title",
      "  fields: [title, authors]",
      "  field-display-names:",
      '    title: "Ausarbeitung"',
      '    authors: "Autor:innen"',
      "---",
      sep = "\n"
    ),
    (m$title %||% slug_to_title(pname)),
    meta$type, meta$semester, yaml_vec(meta$categories)
  )
  
  body <- paste(meta$description %||% "", "", sep = "\n")
  out_path <- file.path(pdir, sprintf("%s_page.qmd", pname))
  write_if_missing(out_path, paste(front, body, sep = "\n"))
}

generate_student_page <- function(sdir, project_name) {
  sname <- basename(sdir)
  meta <- DEFAULTS$student
  m <- read_meta(file.path(sdir, "meta.yml"))
  for (k in names(m)) meta[[k]] <- m[[k]]
  
  title   <- m$title %||% slug_to_title(sname)
  authors <- meta$authors
  
  # HTML & MP4 automatisch finden
  html_rel <- find_student_html(sdir, project_name)
  mp4_rel  <- find_student_mp4(sdir)
  
  front <- sprintf(
    paste(
      "---",
      'title: "%s"',
      "authors: %s",
      "---",
      sep = "\n"
    ),
    title, yaml_vec(authors)
  )
  
  authors_line <- if (length(authors)) paste0("Autoren: ", paste(authors, collapse = ", "), "\n") else ""
  
  body <- paste(
    authors_line, "",
    assets_section(html_rel, mp4_rel),
    "",
    sep = "\n"
  )
  
  out_path <- file.path(sdir, sprintf("%s.qmd", sname))
  write_if_missing(out_path, paste(front, body, sep = "\n"))
}

main <- function() {
  if (!dir.exists(PROJECTS_ROOT)) {
    message("No 'quarto_pages/projects' directory found.")
    return(invisible())
  }
  
  projects <- list.dirs(PROJECTS_ROOT, full.names = TRUE, recursive = FALSE)
  for (p in sort(projects)) {
    pname <- basename(p)
    generate_project_page(p)
    
    sp_root <- file.path(p, "student_projects")
    if (dir.exists(sp_root)) {
      students <- list.dirs(sp_root, full.names = TRUE, recursive = FALSE)
      for (s in sort(students)) {
        generate_student_page(s, pname)
      }
    }
  }
}

main()
