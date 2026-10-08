# Padroniza títulos, identificadores e outros campos usados nas comparações.
# A normalização prepara as chaves; ela não decide a elegibilidade dos estudos.


norm_empty_if_na <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

norm_trim <- function(x) {
  trimws(norm_empty_if_na(x))
}

norm_first_nonempty <- function(...) {
  values <- lapply(list(...), norm_trim)
  max_len <- max(vapply(values, length, integer(1L)), 0L)
  if (max_len == 0L) {
    return(character())
  }

  values <- lapply(values, function(value) {
    if (length(value) == max_len) {
      return(value)
    }
    rep_len(value, max_len)
  })

  out <- rep("", max_len)
  for (value in values) {
    take <- !nzchar(out) & nzchar(value)
    out[take] <- value[take]
  }
  out
}

norm_decode_html_entities <- function(x) {
  x <- norm_empty_if_na(x)
  replacements <- c(
    "&nbsp;" = " ",
    "&#160;" = " ",
    "&amp;" = "&",
    "&lt;" = "<",
    "&gt;" = ">",
    "&quot;" = "\"",
    "&#34;" = "\"",
    "&apos;" = "'",
    "&#39;" = "'"
  )

  for (pattern in names(replacements)) {
    x <- gsub(pattern, replacements[[pattern]], x, fixed = TRUE)
  }

  decode_numeric_entities <- function(value) {
    matches <- gregexpr("&#(?:[0-9]+|x[0-9A-Fa-f]+);", value, perl = TRUE)[[1L]]
    if (identical(matches[[1L]], -1L)) {
      return(value)
    }

    entities <- regmatches(value, list(matches))[[1L]]
    decoded <- vapply(entities, function(entity) {
      raw_code <- sub("^&#", "", sub(";$", "", entity))
      code_point <- if (grepl("^x", raw_code, ignore.case = TRUE)) {
        suppressWarnings(strtoi(sub("^x", "", raw_code, ignore.case = TRUE), base = 16L))
      } else {
        suppressWarnings(as.integer(raw_code))
      }
      if (is.na(code_point) || code_point < 0L) {
        return(entity)
      }
      tryCatch(intToUtf8(code_point), error = function(e) entity)
    }, character(1L))

    for (i in seq_along(entities)) {
      value <- gsub(entities[[i]], decoded[[i]], value, fixed = TRUE)
    }
    value
  }

  vapply(x, decode_numeric_entities, character(1L), USE.NAMES = FALSE)
}

norm_ascii_fold <- function(x) {
  x <- norm_decode_html_entities(x)
  folded <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  folded[is.na(folded)] <- x[is.na(folded)]
  folded
}

norm_space <- function(x) {
  x <- norm_trim(x)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

normalize_text_key <- function(x) {
  x <- tolower(norm_ascii_fold(x))
  x <- gsub("[^a-z0-9]+", " ", x, perl = TRUE)
  norm_space(x)
}

normalize_compact_key <- function(x) {
  x <- normalize_text_key(x)
  gsub(" ", "", x, fixed = TRUE)
}

regex_extract_first <- function(text, pattern, ignore_case = FALSE) {
  text <- norm_empty_if_na(text)
  flags <- if (isTRUE(ignore_case)) "(?i)" else ""
  match <- regexpr(paste0(flags, pattern), text, perl = TRUE)
  out <- rep("", length(text))
  hits <- match > 0L
  if (any(hits)) {
    out[hits] <- regmatches(text, match)
  }
  out
}

normalize_doi <- function(...) {
  text <- tolower(norm_first_nonempty(...))
  text <- norm_decode_html_entities(text)
  text <- gsub("https?://(dx\\.)?doi\\.org/", "", text, perl = TRUE)
  text <- gsub("^doi\\s*[: ]\\s*", "", text, perl = TRUE)

  pattern <- "10\\.[0-9]{4,9}/[^[:space:]\"'<>]+"
  doi <- regex_extract_first(text, pattern)
  doi <- gsub("[\\]\\[\\)\\}\\.,;:]+$", "", doi, perl = TRUE)
  doi
}

normalize_pmid <- function(...) {
  text <- norm_first_nonempty(...)
  digits <- gsub("[^0-9]+", "", text, perl = TRUE)
  ifelse(nzchar(digits), digits, "")
}

normalize_arxiv_id <- function(...) {
  text <- tolower(norm_first_nonempty(...))
  text <- gsub("arxiv:", "", text, fixed = TRUE)

  out <- regex_extract_first(text, "[0-9]{4}\\.[0-9]{4,5}(v[0-9]+)?")
  out <- sub("v[0-9]+$", "", out, perl = TRUE)

  needs_old_style <- !nzchar(out)
  if (any(needs_old_style)) {
    old_out <- regex_extract_first(text[needs_old_style], "[a-z][a-z0-9.-]*/[0-9]{7}(v[0-9]+)?")
    old_out <- sub("v[0-9]+$", "", old_out, perl = TRUE)
    out[needs_old_style] <- old_out
  }

  out
}

normalize_nct_id <- function(...) {
  text <- toupper(norm_first_nonempty(...))
  regex_extract_first(text, "NCT[0-9]{8}")
}

normalize_crd_id <- function(...) {
  text <- toupper(norm_first_nonempty(...))
  regex_extract_first(text, "CRD[0-9]+")
}

normalize_osf_id <- function(osf_id, url = "") {
  explicit_id <- tolower(normalize_compact_key(osf_id))
  explicit_id <- gsub("^osf", "", explicit_id)

  from_url <- tolower(norm_empty_if_na(url))
  match <- regexec("osf\\.io/([a-z0-9]{4,10})", from_url, perl = TRUE)
  parts <- regmatches(from_url, match)
  url_id <- vapply(parts, function(part) {
    if (length(part) >= 2L) {
      return(part[[2L]])
    }
    ""
  }, character(1L))

  norm_first_nonempty(explicit_id, url_id)
}

normalize_year <- function(publication_date, year) {
  text <- norm_first_nonempty(publication_date, year)
  regex_extract_first(text, "(19|20)[0-9]{2}")
}

extract_first_author <- function(authors) {
  authors <- norm_space(authors)
  first <- sub(";.*$", "", authors, perl = TRUE)
  first <- sub("\\s+and\\s+.*$", "", first, perl = TRUE, ignore.case = TRUE)
  first <- sub("\\|.*$", "", first, perl = TRUE)

  surname <- ifelse(
    grepl(",", first, fixed = TRUE),
    sub(",.*$", "", first, perl = TRUE),
    sub("^.*\\s+", "", first, perl = TRUE)
  )
  norm_space(surname)
}

normalize_first_author <- function(authors) {
  normalize_compact_key(extract_first_author(authors))
}

normalize_source_fallback <- function(...) {
  value <- normalize_compact_key(norm_first_nonempty(...))
  ifelse(nzchar(value), value, "")
}
