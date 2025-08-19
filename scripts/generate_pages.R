#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("yaml", quietly = TRUE)) {
    install.packages("yaml", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    install.packages("readxl", repos = "https://cloud.r-project.org")
  }
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------
# Basisverzeichnisse
# ------------------------------------------------------------
ROOT <- normalizePath(".", winslash = "/")
PROJECTS_ROOT <- file.path(ROOT, "projects")  # ggf. auf "quarto_pages/projects" ändern

# ------------------------------------------------------------
# Defaults
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# Hilfsfunktionen
# ------------------------------------------------------------
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
  x <- as.character(x)
  x <- x[nzchar(trimws(x))]
  if (length(x) == 0) "[]"
  else paste0("[", paste(sprintf('"%s"', gsub('"','\\"', x, fixed = TRUE)), collapse = ", "), "]")
}

yaml_scalar <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)))) return(NA_character_)
  xn <- suppressWarnings(as.numeric(x))
  if (!is.na(xn)) as.character(xn) else sprintf('"%s"', gsub('"','\\"', as.character(x), fixed = TRUE))
}

clean_authors <- function(...) {
  x <- unlist(list(...), use.names = FALSE)
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[nzchar(x)]
  # falls in einer Zelle "Max Mustermann, Erika Muster" steht:
  x <- unlist(strsplit(x, "\\s*,\\s*"))
  x <- trimws(x)
  x <- x[nzchar(x)]
  unique(x)  # doppelte entfernen
}

# ------------------------------------------------------------
# HTML & MP4 finden
# ------------------------------------------------------------
# Sucht im Team-Ordner nach "Projekt_<Projektname>.html|.htm" (Case-insensitive)
find_student_html <- function(sdir, project_name) {
  files <- list.files(sdir, full.names = FALSE)
  if (!length(files)) return(NULL)
  lbn <- tolower(basename(files))
  wanted <- c(
    paste0("projekt_", tolower(project_name), ".html"),
    paste0("projekt_", tolower(project_name), ".htm")
  )
  hit <- which(lbn %in% wanted)
  if (length(hit)) files[hit[1]] else NULL
}

# Findet genau eine .mp4 im Ordner (Dateiname egal). Falls mehrere, bevorzuge "Screencast.mp4".
find_student_mp4 <- function(sdir) {
  mp4s <- list.files(sdir, pattern = "\\.mp4$", full.names = FALSE, ignore.case = TRUE)
  if (length(mp4s) == 0) return(NULL)
  if (length(mp4s) == 1) return(mp4s[1])
  pref <- mp4s[tolower(mp4s) == "screencast.mp4"]
  if (length(pref)) pref[1] else mp4s[1]
}

# Abstand zwischen Video und iFrame via CSS-Margin
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

# ------------------------------------------------------------
# Excel pro Projektordner lesen
# Erwartet 'metadata.xlsx' oder 'metadata.xls' mit Spalten:
# student_folder, author1, author2, author3, rank
# ------------------------------------------------------------
read_project_metadata <- function(pdir) {
  candidates <- list.files(pdir, pattern = "^metadata\\.(xlsx|xls)$", ignore.case = TRUE, full.names = TRUE)
  if (!length(candidates)) return(NULL)
  f <- candidates[1]
  df <- readxl::read_excel(f)
  # Spaltennamen robust machen
  names(df) <- tolower(gsub("\\s+", "_", names(df)))
  needed <- c("student_folder", "author1", "author2", "author3", "rank")
  missing <- setdiff(needed, names(df))
  if (length(missing)) {
    stop("In '", basename(f), "' fehlen Spalten: ", paste(missing, collapse = ", "), " (im Ordner: ", pdir, ")")
  }
  df$student_folder <- as.character(df$student_folder)
  df$author1 <- as.character(df$author1)
  df$author2 <- as.character(df$author2)
  df$author3 <- as.character(df$author3)
  df$rank <- as.character(df$rank)
  df
}

# ------------------------------------------------------------
# Seiten-Generatoren
# ------------------------------------------------------------
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
      'image: "image.png"',
      "categories: %s",
      "listing:",
      "  id: student_projects",
      '  contents: ["student_projects/*/*.qmd"]',
      "  type: table",
      "  sort: title",
      "  fields: [title, authors]",       # bei Bedarf auf [rank, title, authors] ändern und sort: rank
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

generate_student_page <- function(sdir, project_name, md_df) {
  sname <- basename(sdir)
  # Titel ggf. aus meta.yml, ansonsten Slug
  m <- read_meta(file.path(sdir, "meta.yml"))
  title <- m$title %||% slug_to_title(sname)
  
  # --- Excel-Zeile für diesen student_folder holen
  authors <- character(0)
  rank_val <- NA_character_
  
  if (!is.null(md_df)) {
    row <- md_df[tolower(md_df$student_folder) == tolower(sname), , drop = FALSE]
    if (nrow(row) >= 1) {
      authors <- clean_authors(row$author1[1], row$author2[1], row$author3[1])
      rank_val <- as.character(row$rank[1])
    } else {
      message("Warnung: Kein Eintrag in metadata für student_folder='", sname, "' in Projekt '", project_name, "'.")
    }
  } else {
    message("Hinweis: Keine 'metadata.xlsx' in Projekt '", project_name, "' gefunden – Autoren/Rang werden nicht gesetzt.")
  }
  
  # optional: Autoren aus meta.yml ergänzen (falls dort zusätzlich gepflegt)
  if (!is.null(m$authors)) {
    authors <- unique(c(authors, clean_authors(m$authors)))
  }
  
  # HTML & MP4 automatisch finden
  html_rel <- find_student_html(sdir, project_name)
  mp4_rel  <- find_student_mp4(sdir)
  
  # Frontmatter
  front_lines <- c(
    "---",
    sprintf('title: "%s"', title),
    sprintf("authors: %s", yaml_vec(authors))
  )
  if (!is.null(rank_val) && nzchar(trimws(rank_val))) {
    front_lines <- c(front_lines, paste0("rank: ", yaml_scalar(rank_val)))
  }
  front_lines <- c(front_lines, "---")
  front <- paste(front_lines, collapse = "\n")
  
  authors_line <- if (length(authors)) paste0("Autoren: ", paste(authors, collapse = ", "), "\n") else ""
  #rank_line    <- if (!is.null(rank_val) && nzchar(trimws(rank_val))) paste0("Rang: ", rank_val, "\n") else ""
  
  body <- paste(
    authors_line,
    #rank_line,
    "",
    assets_section(html_rel, mp4_rel),
    "",
    sep = "\n"
  )
  
  out_path <- file.path(sdir, sprintf("%s.qmd", sname))
  write_if_missing(out_path, paste(front, body, sep = "\n"))
}

# ------------------------------------------------------------
# Main
# ------------------------------------------------------------
main <- function() {
  if (!dir.exists(PROJECTS_ROOT)) {
    message("Verzeichnis nicht gefunden: ", PROJECTS_ROOT)
    return(invisible())
  }
  
  projects <- list.dirs(PROJECTS_ROOT, full.names = TRUE, recursive = FALSE)
  for (p in sort(projects)) {
    pname <- basename(p)
    
    # 1) Projektseite erzeugen (falls fehlt)
    generate_project_page(p)
    
    # 2) Excel des Projekts lesen
    md_df <- read_project_metadata(p)  # NULL, falls nicht vorhanden
    
    # 3) Team-Seiten erzeugen
    sp_root <- file.path(p, "student_projects")
    if (dir.exists(sp_root)) {
      students <- list.dirs(sp_root, full.names = TRUE, recursive = FALSE)
      for (s in sort(students)) {
        generate_student_page(s, pname, md_df)
      }
    }
  }
}

main()
