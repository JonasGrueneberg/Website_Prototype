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

# --- Einstellungen -----------------------------------------------------------
OVERWRITE_STUDENT_QMD <- FALSE  # TRUE = Team-QMD immer überschreiben
ROOT         <- normalizePath(".", winslash = "/")
PROJECTS_DIR <- file.path(ROOT, "quarto_pages", "projects")
EXCEL_PATH   <- file.path(ROOT, "quarto_pages", "metadata.xlsx")

# --- Excel einlesen ----------------------------------------------------------
library(readxl)
if (!file.exists(EXCEL_PATH)) {
  stop("Excel-Datei nicht gefunden: ", EXCEL_PATH)
}
metadata <- read_excel(EXCEL_PATH)

# robuste Spaltennamen (case-insensitive)
names(metadata) <- tolower(gsub("\\s+", "_", names(metadata)))

required_cols <- c("project_name", "student_folder")
missing_req   <- setdiff(required_cols, names(metadata))
if (length(missing_req)) {
  stop("Folgende Pflichtspalten fehlen in der Excel-Datei: ", paste(missing_req, collapse = ", "))
}

# Ermittele bis zu 3 Autorenspalten (author1/2/3 oder autor1/2/3)
author_cols <- names(metadata)[grepl("^(author|autor)[0-9]*$", names(metadata))]
if (length(author_cols) == 0) {
  # fallback auf bekannte Namen
  author_cols <- c("author1","author2","author3")
}
author_cols <- intersect(author_cols, names(metadata))
author_cols <- author_cols[order(author_cols)]           # konsistente Reihenfolge
author_cols <- head(author_cols, 3)                      # max. 3 Spalten

# Rang-Spalte (optional)
rank_col <- if ("rank" %in% names(metadata)) "rank" else NA_character_

# Strings sicherstellen
to_chr <- function(x) if (is.null(x)) character(0) else as.character(x)

# Hilfsfunktionen -------------------------------------------------------------
slug_to_title <- function(s) {
  s <- gsub("[-_]+", " ", s); s <- trimws(s); tools::toTitleCase(s)
}

read_meta <- function(path) {
  if (file.exists(path)) {
    m <- yaml::yaml.load_file(path); if (is.null(m)) list() else m
  } else list()
}

yaml_quote <- function(s) gsub('"', '\\"', s, fixed = TRUE)
yaml_vec <- function(x) {
  x <- to_chr(x)
  x <- x[nzchar(trimws(x))]
  if (!length(x)) "[]"
  else paste0("[", paste(sprintf('"%s"', yaml_quote(x)), collapse = ", "), "]")
}
yaml_scalar <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)))) return(NA_character_)
  xn <- suppressWarnings(as.numeric(x))
  if (!is.na(xn)) as.character(xn) else sprintf('"%s"', yaml_quote(as.character(x)))
}

# HTML nach Schema "Projekt_<Projektname>.html|.htm" finden (tolerant bei -/_/Leerzeichen)
find_student_html <- function(sdir, project_name) {
  cand <- tolower(c(
    project_name,
    gsub("\\s+", "_", project_name),
    gsub("\\s+", "-", project_name),
    gsub("-", "_", project_name),
    gsub("_", "-", project_name)
  ))
  cand <- unique(cand)
  files <- list.files(sdir, full.names = FALSE)
  if (!length(files)) return(NULL)
  lbn <- tolower(basename(files))
  wanted <- unlist(lapply(cand, function(nm) c(paste0("projekt_", nm, ".html"),
                                               paste0("projekt_", nm, ".htm"))))
  hit <- which(lbn %in% wanted)
  if (length(hit)) files[hit[1]] else NULL
}

# Eine .mp4 finden (Dateiname egal). Falls mehrere, bevorzuge "Screencast.mp4".
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
    parts <- c(parts,
               "**Screencast**", "",
               sprintf('<video controls width="100%%" style="margin-bottom: 2em;"><source src="%s" type="video/mp4"></video>', mp4_rel),
               ""
    )
  }
  if (!is.null(html_rel)) {
    parts <- c(parts,
               "**Ausarbeitung**", "",
               sprintf('<iframe src="%s" loading="lazy" width="100%%" height="1200" style="border:0"></iframe>', html_rel),
               ""
    )
  }
  if (length(parts) == 0) {
    parts <- c("> TODO: Lege `Projekt_<Projektname>.html` und/oder genau **eine** `.mp4` in diesen Ordner, dann wird die Einbettung automatisch angezeigt.")
  }
  paste(parts, collapse = "\n")
}

write_text <- function(path, content) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "w+", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(content, con, useBytes = TRUE)
}

write_if_missing <- function(path, content) {
  if (!file.exists(path)) {
    write_text(path, content)
    message("created: ", path)
  } else {
    message("exists:  ", path)
  }
}

write_or_update <- function(path, content, overwrite = FALSE) {
  if (overwrite || !file.exists(path)) {
    write_text(path, content)
    if (overwrite && file.exists(path)) message("updated: ", path) else message("created: ", path)
  } else {
    message("exists:  ", path)
  }
}

# --- Seiten-Generator --------------------------------------------------------
generate_project_page <- function(pdir) {
  pname <- basename(pdir)
  meta <- list(type = "descriptive", semester = "WS 25-26", categories = character(0),
               description = "Kurze Beschreibung des Projekts und Motivation")
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
      "  sort: rank",                  # sortiere nach Rang
      "  fields: [rank, title, authors]",
      "  field-display-names:",
      '    rank: "Rang"',
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

# Hole Metadatenzeile aus Excel für Projekt & Team
lookup_metadata <- function(project_name, student_folder) {
  sub <- metadata[
    tolower(metadata$project_name)  == tolower(project_name) &
      tolower(metadata$student_folder) == tolower(student_folder),
    , drop = FALSE]
  if (nrow(sub) == 0) return(NULL)
  sub[1, , drop = FALSE]
}

generate_student_page <- function(sdir, project_name) {
  sname <- basename(sdir)
  m_yaml <- read_meta(file.path(sdir, "meta.yml"))  # optional, falls vorhanden
  
  # Excel-Zeile holen
  row <- lookup_metadata(project_name, sname)
  
  # Autoren aus bis zu 3 Spalten zusammenstellen
  authors <- character(0)
  if (!is.null(row)) {
    for (col in author_cols) {
      if (col %in% names(row)) {
        a <- to_chr(row[[col]])
        a <- trimws(a)
        if (length(a) && nzchar(a)) authors <- c(authors, a)
      }
    }
  }
  # optional aus meta.yml ergänzen, falls vorhanden
  if (!is.null(m_yaml$authors)) {
    authors <- unique(c(authors, to_chr(m_yaml$authors)))
  }
  
  # Rang aus Excel (oder meta.yml, falls gesetzt)
  rank_val <- NA_character_
  if (!is.null(row) && !is.na(rank_col) && rank_col %in% names(row)) {
    rank_val <- to_chr(row[[rank_col]])[1]
  }
  if (!is.null(m_yaml$rank) && nzchar(to_chr(m_yaml$rank))) {
    rank_val <- to_chr(m_yaml$rank)
  }
  
  title <- (m_yaml$title %||% slug_to_title(sname))
  
  # HTML & MP4 automatisch finden
  html_rel <- find_student_html(sdir, project_name)
  mp4_rel  <- find_student_mp4(sdir)
  
  # Frontmatter bauen
  front_lines <- c(
    "---",
    sprintf('title: "%s"', title),
    sprintf("authors: %s", yaml_vec(authors))
  )
  if (!is.na(rank_val)) {
    front_lines <- c(front_lines, paste0("rank: ", yaml_scalar(rank_val)))
  }
  front_lines <- c(front_lines, "---")
  front <- paste(front_lines, collapse = "\n")
  
  authors_line <- if (length(authors)) paste0("Autoren: ", paste(authors, collapse = ", "), "\n") else ""
  rank_line    <- if (!is.na(rank_val)) paste0("Rang: ", rank_val, "\n") else ""
  
  body <- paste(
    authors_line,
    rank_line,
    "",
    assets_section(html_rel, mp4_rel),
    "",
    sep = "\n"
  )
  
  out_path <- file.path(sdir, sprintf("%s.qmd", sname))
  write_or_update(out_path, paste(front, body, sep = "\n"), overwrite = OVERWRITE_STUDENT_QMD)
}

# --- Main --------------------------------------------------------------------
main <- function() {
  if (!dir.exists(PROJECTS_DIR)) {
    message("Verzeichnis nicht gefunden: ", PROJECTS_DIR)
    return(invisible())
  }
  
  projects <- list.dirs(PROJECTS_DIR, full.names = TRUE, recursive = FALSE)
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
