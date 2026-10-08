# Lê as exportações bibliográficas e converte os campos para uma estrutura comum.
# Mantém a identificação da fonte para que cada registro possa ser rastreado.


source_route_table <- function() {
  data.frame(
    source_bucket_normalized = c(
      "PUBMED",
      "EMBASE",
      "SCOPUS",
      "WOS",
      "COCHRANE Library",
      "EBSCO",
      "PsycINFO",
      "PROQuest",
      "IEEE Xplore",
      "ENGINEERING VILLAGE",
      "ACM Digital Library",
      "arXiv",
      "medRxiv",
      "bioRxiv",
      "ClinicalTrials",
      "PROSPERO",
      "OSF_Registries"
    ),
    source_database = c(
      "PubMed/MEDLINE",
      "Embase",
      "Scopus",
      "Web of Science Core Collection",
      "Cochrane Library",
      "CINAHL/EBSCO",
      "PsycINFO",
      "ProQuest Dissertations and Theses",
      "IEEE Xplore",
      "Compendex/Engineering Village",
      "ACM Digital Library",
      "arXiv",
      "medRxiv via Europe PMC",
      "bioRxiv via Europe PMC",
      "ClinicalTrials.gov",
      "PROSPERO",
      "OSF Registries"
    ),
    corpus_type = c(
      rep("primary", 14L),
      "clinical_trial_registry_supplementary",
      "review_registry_overlap",
      "review_registry_overlap"
    ),
    canonical_input = c(
      "NBIB",
      "RIS",
      "RIS",
      "RIS",
      "RIS",
      "BibTeX",
      "RIS",
      "RIS",
      "CSV",
      "CSV",
      "BibTeX",
      "Atom XML",
      "Europe PMC CSV",
      "Europe PMC CSV",
      "CSV result exports",
      "raw derived CSV",
      "raw derived CSV"
    ),
    parser = c(
      "parse_nbib()",
      "parse_ris(dialect = \"embase\")",
      "parse_ris(dialect = \"scopus\")",
      "parse_ris(dialect = \"wos\")",
      "parse_ris(dialect = \"cochrane\")",
      "parse_bibtex(dialect = \"ebsco\")",
      "parse_ris(dialect = \"psycnet\")",
      "parse_ris(dialect = \"proquest\")",
      "parse_csv(schema = \"ieee\")",
      "parse_csv(schema = \"compendex\")",
      "parse_bibtex(dialect = \"acm\")",
      "parse_arxiv_atom()",
      "parse_csv(schema = \"europepmc\")",
      "parse_csv(schema = \"europepmc\")",
      "parse_csv(schema = \"ctgov\")",
      "parse_csv(schema = \"prospero\")",
      "parse_csv(schema = \"osf\")"
    ),
    stringsAsFactors = FALSE
  )
}

source_route_for_bucket <- function(source_bucket_normalized) {
  route_table <- source_route_table()
  bucket <- trimws(source_bucket_normalized)
  row <- route_table[route_table$source_bucket_normalized == bucket, , drop = FALSE]
  if (nrow(row) == 0L) {
    return(list(
      source_database = "",
      corpus_type = "excluded_nondata",
      canonical_input = "",
      parser = ""
    ))
  }

  list(
    source_database = row$source_database[[1L]],
    corpus_type = row$corpus_type[[1L]],
    canonical_input = row$canonical_input[[1L]],
    parser = row$parser[[1L]]
  )
}

file_format_from_extension <- function(file_extension, file_name = "") {
  ext <- tolower(trimws(file_extension))
  name_lower <- tolower(file_name)

  if (name_lower %in% c(".ds_store", "thumbs.db")) {
    return("OS_METADATA")
  }
  if (identical(ext, "nbib")) {
    return("NBIB")
  }
  if (identical(ext, "ris")) {
    return("RIS")
  }
  if (ext %in% c("bib", "bibtex")) {
    return("BibTeX")
  }
  if (identical(ext, "csv")) {
    return("CSV")
  }
  if (identical(ext, "xml")) {
    return("Atom XML")
  }
  if (identical(ext, "json")) {
    return("JSON")
  }
  if (ext %in% c("txt", "log")) {
    return("TXT")
  }
  if (ext %in% c("doc", "docx", "pdf")) {
    return(toupper(ext))
  }
  if (ext %in% c("r", "py", "sh")) {
    return(toupper(ext))
  }
  if (!nzchar(ext)) {
    return("UNKNOWN")
  }
  toupper(ext)
}

PARSER_OUTPUT_COLUMNS <- c(
  "raw_record_index",
  "raw_record_id",
  "title",
  "authors",
  "year",
  "publication_date",
  "journal_or_source",
  "doi",
  "pmid",
  "pmcid",
  "arxiv_id",
  "nct_id",
  "crd_id",
  "osf_id",
  "url",
  "abstract",
  "keywords",
  "language",
  "publication_type",
  "document_type",
  "source_record_id",
  "raw_record_text",
  "raw_record_json",
  "import_warning",
  "import_error"
)

empty_parser_output <- function() {
  as.data.frame(
    setNames(rep(list(character()), length(PARSER_OUTPUT_COLUMNS)), PARSER_OUTPUT_COLUMNS),
    stringsAsFactors = FALSE
  )
}

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

first_nonempty <- function(...) {
  values <- unlist(list(...), use.names = FALSE)
  values <- clean_text(values)
  values <- values[nzchar(values)]
  if (length(values) == 0L) {
    return("")
  }
  values[[1L]]
}

extract_year_value <- function(...) {
  text <- paste(unlist(list(...), use.names = FALSE), collapse = " ")
  match <- regexpr("(18|19|20)[0-9]{2}", text, perl = TRUE)
  if (match < 0L) {
    return("")
  }
  regmatches(text, match)
}

extract_doi_value <- function(...) {
  text <- paste(unlist(list(...), use.names = FALSE), collapse = " ")
  match <- regexpr("10[.][0-9]{4,9}/[^[:space:]\"'<>]+", text, perl = TRUE, ignore.case = TRUE)
  if (match < 0L) {
    return("")
  }
  doi <- regmatches(text, match)
  doi <- sub("[[:punct:]]+$", "", doi)
  tolower(doi)
}

parser_row <- function(
  raw_record_index,
  raw_record_id = "",
  title = "",
  authors = "",
  year = "",
  publication_date = "",
  journal_or_source = "",
  doi = "",
  pmid = "",
  pmcid = "",
  arxiv_id = "",
  nct_id = "",
  crd_id = "",
  osf_id = "",
  url = "",
  abstract = "",
  keywords = "",
  language = "",
  publication_type = "",
  document_type = "",
  source_record_id = "",
  raw_record_text = "",
  raw_record_json = "",
  import_warning = "",
  import_error = ""
) {
  data.frame(
    raw_record_index = as.character(raw_record_index),
    raw_record_id = clean_text(raw_record_id),
    title = clean_text(title),
    authors = clean_text(authors),
    year = clean_text(year),
    publication_date = clean_text(publication_date),
    journal_or_source = clean_text(journal_or_source),
    doi = clean_text(doi),
    pmid = clean_text(pmid),
    pmcid = clean_text(pmcid),
    arxiv_id = clean_text(arxiv_id),
    nct_id = clean_text(nct_id),
    crd_id = clean_text(crd_id),
    osf_id = clean_text(osf_id),
    url = clean_text(url),
    abstract = clean_text(abstract),
    keywords = clean_text(keywords),
    language = clean_text(language),
    publication_type = clean_text(publication_type),
    document_type = clean_text(document_type),
    source_record_id = clean_text(source_record_id),
    raw_record_text = as.character(raw_record_text),
    raw_record_json = as.character(raw_record_json),
    import_warning = clean_text(import_warning),
    import_error = clean_text(import_error),
    stringsAsFactors = FALSE
  )
}

bind_parser_rows <- function(rows) {
  if (length(rows) == 0L) {
    return(empty_parser_output())
  }
  out <- do.call(rbind, rows)
  out[, PARSER_OUTPUT_COLUMNS, drop = FALSE]
}

read_text_lines <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

tag_values <- function(record, tags) {
  values <- unlist(record[intersect(tags, names(record))], use.names = FALSE)
  values <- clean_text(values)
  values[nzchar(values)]
}

tag_first <- function(record, tags) {
  first_nonempty(tag_values(record, tags))
}

tag_collapse <- function(record, tags) {
  values <- tag_values(record, tags)
  if (length(values) == 0L) {
    return("")
  }
  paste(unique(values), collapse = "; ")
}

parse_tagged_records <- function(lines, tag_pattern, record_end_pattern = NULL) {
  records <- list()
  current <- list()
  current_tag <- ""
  raw <- character()

  flush_record <- function() {
    if (length(current) == 0L) {
      return(NULL)
    }
    list(fields = current, raw = paste(raw, collapse = "\n"))
  }

  for (line in lines) {
    if (!is.null(record_end_pattern) && grepl(record_end_pattern, line)) {
      raw <- c(raw, line)
      flushed <- flush_record()
      if (!is.null(flushed)) {
        records[[length(records) + 1L]] <- flushed
      }
      current <- list()
      current_tag <- ""
      raw <- character()
      next
    }

    match <- regexec(tag_pattern, line, perl = TRUE)
    parts <- regmatches(line, match)[[1L]]
    if (length(parts) >= 3L) {
      tag <- parts[[2L]]
      value <- parts[[3L]]
      current[[tag]] <- c(current[[tag]], value)
      current_tag <- tag
    } else if (nzchar(current_tag) && length(current[[current_tag]]) > 0L) {
      last_index <- length(current[[current_tag]])
      current[[current_tag]][[last_index]] <- paste(current[[current_tag]][[last_index]], trimws(line))
    }
    raw <- c(raw, line)
  }

  flushed <- flush_record()
  if (!is.null(flushed)) {
    records[[length(records) + 1L]] <- flushed
  }
  records
}

parse_ris <- function(path, dialect = "") {
  records <- parse_tagged_records(read_text_lines(path), "^([A-Z0-9]{2})  -[[:space:]]?(.*)$", "^ER  -")
  rows <- lapply(seq_along(records), function(i) {
    rec <- records[[i]]$fields
    doi <- extract_doi_value(tag_collapse(rec, c("DO", "L2", "UR")))
    raw_id <- first_nonempty(doi, tag_first(rec, c("U2", "UT", "AN", "ID", "SN")), paste0("RIS_", i))
    parser_row(
      raw_record_index = i,
      raw_record_id = raw_id,
      title = tag_first(rec, c("T1", "TI", "CT", "BT")),
      authors = tag_collapse(rec, c("A1", "AU")),
      year = extract_year_value(tag_first(rec, c("Y1", "PY", "DA"))),
      publication_date = tag_first(rec, c("Y1", "PY", "DA")),
      journal_or_source = tag_first(rec, c("JF", "JO", "T2", "JA")),
      doi = doi,
      url = tag_first(rec, c("UR", "L2")),
      abstract = tag_collapse(rec, c("N2", "AB")),
      keywords = tag_collapse(rec, c("KW")),
      language = tag_first(rec, c("LA")),
      publication_type = tag_first(rec, c("TY", "M3", "DT")),
      document_type = dialect,
      source_record_id = raw_id,
      raw_record_text = records[[i]]$raw
    )
  })
  bind_parser_rows(rows)
}

parse_nbib <- function(path) {
  lines <- read_text_lines(path)
  records <- list()
  current <- list()
  current_tag <- ""
  raw <- character()

  flush_current <- function() {
    if (length(current) == 0L) {
      return(NULL)
    }
    list(fields = current, raw = paste(raw, collapse = "\n"))
  }

  for (line in lines) {
    match <- regexec("^([A-Z0-9]{2,4})[[:space:]]*-[[:space:]]?(.*)$", line, perl = TRUE)
    parts <- regmatches(line, match)[[1L]]

    if (length(parts) >= 3L && identical(parts[[2L]], "PMID") && length(current) > 0L) {
      flushed <- flush_current()
      if (!is.null(flushed)) {
        records[[length(records) + 1L]] <- flushed
      }
      current <- list()
      current_tag <- ""
      raw <- character()
    }

    if (length(parts) >= 3L) {
      tag <- parts[[2L]]
      value <- parts[[3L]]
      current[[tag]] <- c(current[[tag]], value)
      current_tag <- tag
    } else if (nzchar(current_tag) && length(current[[current_tag]]) > 0L) {
      last_index <- length(current[[current_tag]])
      current[[current_tag]][[last_index]] <- paste(current[[current_tag]][[last_index]], trimws(line))
    }
    raw <- c(raw, line)
  }

  flushed <- flush_current()
  if (!is.null(flushed)) {
    records[[length(records) + 1L]] <- flushed
  }

  rows <- lapply(seq_along(records), function(i) {
    rec <- records[[i]]$fields
    pmid <- tag_first(rec, "PMID")
    doi <- extract_doi_value(tag_collapse(rec, c("AID", "LID")))
    parser_row(
      raw_record_index = i,
      raw_record_id = first_nonempty(pmid, doi, paste0("NBIB_", i)),
      title = tag_first(rec, "TI"),
      authors = tag_collapse(rec, c("FAU", "AU")),
      year = extract_year_value(tag_first(rec, c("DP", "DEP", "EDAT"))),
      publication_date = tag_first(rec, c("DP", "DEP", "EDAT")),
      journal_or_source = tag_first(rec, c("JT", "TA")),
      doi = doi,
      pmid = pmid,
      abstract = tag_collapse(rec, "AB"),
      keywords = tag_collapse(rec, c("MH", "OT")),
      language = tag_first(rec, "LA"),
      publication_type = tag_collapse(rec, "PT"),
      document_type = "nbib",
      source_record_id = pmid,
      raw_record_text = records[[i]]$raw
    )
  })
  bind_parser_rows(rows)
}

split_bibtex_entries <- function(text) {
  lines <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  entries <- character()
  current <- character()
  depth <- 0L
  in_entry <- FALSE

  for (line in lines) {
    if (grepl("^@", trimws(line))) {
      if (in_entry && length(current) > 0L) {
        entries <- c(entries, paste(current, collapse = "\n"))
      }
      current <- line
      depth <- gregexpr_count(line, "{") - gregexpr_count(line, "}")
      in_entry <- TRUE
    } else if (in_entry) {
      current <- c(current, line)
      depth <- depth + gregexpr_count(line, "{") - gregexpr_count(line, "}")
    }

    if (in_entry && depth <= 0L && length(current) > 0L) {
      entries <- c(entries, paste(current, collapse = "\n"))
      current <- character()
      in_entry <- FALSE
    }
  }

  if (in_entry && length(current) > 0L) {
    entries <- c(entries, paste(current, collapse = "\n"))
  }
  entries
}

gregexpr_count <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1L]]
  if (identical(matches[[1L]], -1L)) {
    return(0L)
  }
  length(matches)
}

split_top_level_commas <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1L]]
  parts <- character()
  current <- character()
  depth <- 0L
  in_quote <- FALSE
  previous <- ""

  for (ch in chars) {
    if (identical(ch, "\"") && !identical(previous, "\\")) {
      in_quote <- !in_quote
    }
    if (!in_quote && identical(ch, "{")) {
      depth <- depth + 1L
    }
    if (!in_quote && identical(ch, "}")) {
      depth <- max(0L, depth - 1L)
    }
    if (!in_quote && depth == 0L && identical(ch, ",")) {
      parts <- c(parts, paste(current, collapse = ""))
      current <- character()
    } else {
      current <- c(current, ch)
    }
    previous <- ch
  }
  parts <- c(parts, paste(current, collapse = ""))
  trimws(parts[nzchar(trimws(parts))])
}

clean_bibtex_value <- function(value) {
  value <- trimws(value)
  value <- sub("^[{]", "", value)
  value <- sub("[}]$", "", value)
  value <- sub("^[\"]", "", value)
  value <- sub("[\"]$", "", value)
  clean_text(value)
}

parse_bibtex_entry <- function(entry) {
  header <- regexec("(?s)^@([A-Za-z]+)[[:space:]]*[{][[:space:]]*([^,]+),(.*)[}][[:space:]]*$", entry, perl = TRUE)
  parts <- regmatches(entry, header)[[1L]]
  if (length(parts) < 4L) {
    return(NULL)
  }

  entry_type <- parts[[2L]]
  entry_key <- clean_text(parts[[3L]])
  body <- parts[[4L]]
  fields <- list(entry_type = entry_type, entry_key = entry_key)
  for (field in split_top_level_commas(body)) {
    eq <- regexpr("=", field, fixed = TRUE)
    if (eq < 0L) {
      next
    }
    name <- tolower(trimws(substr(field, 1L, eq - 1L)))
    value <- clean_bibtex_value(substr(field, eq + 1L, nchar(field)))
    fields[[name]] <- value
  }
  fields
}

parse_bibtex <- function(path, dialect = "") {
  text <- paste(read_text_lines(path), collapse = "\n")
  entries <- split_bibtex_entries(text)
  parsed <- lapply(entries, parse_bibtex_entry)
  parsed <- parsed[!vapply(parsed, is.null, logical(1L))]
  rows <- lapply(seq_along(parsed), function(i) {
    rec <- parsed[[i]]
    authors <- gsub("[[:space:]]+[Aa][Nn][Dd][[:space:]]+", "; ", first_nonempty(rec$author, rec$editor))
    parser_row(
      raw_record_index = i,
      raw_record_id = first_nonempty(rec$doi, rec$entry_key, paste0("BIBTEX_", i)),
      title = first_nonempty(rec$title),
      authors = authors,
      year = extract_year_value(rec$year, rec$date),
      publication_date = first_nonempty(rec$year, rec$date),
      journal_or_source = first_nonempty(rec$journal, rec$booktitle, rec$series, rec$publisher),
      doi = extract_doi_value(rec$doi, rec$url),
      url = first_nonempty(rec$url),
      abstract = first_nonempty(rec$abstract),
      keywords = first_nonempty(rec$keywords),
      publication_type = first_nonempty(rec$entry_type),
      document_type = dialect,
      source_record_id = first_nonempty(rec$entry_key),
      raw_record_text = entries[[i]]
    )
  })
  bind_parser_rows(rows)
}

json_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\r", "\\\\r", x)
  x <- gsub("\n", "\\\\n", x)
  x
}

row_to_json <- function(row) {
  keys <- names(row)
  values <- vapply(row, function(x) as.character(x[[1L]]), character(1L))
  paste0(
    "{",
    paste0("\"", json_escape(keys), "\":\"", json_escape(values), "\"", collapse = ","),
    "}"
  )
}

csv_col <- function(data, name, default = "") {
  if (!name %in% names(data)) {
    return(rep(default, nrow(data)))
  }
  value <- as.character(data[[name]])
  value[is.na(value)] <- ""
  value
}

csv_first_col <- function(data, names) {
  for (name in names) {
    if (name %in% colnames(data)) {
      return(csv_col(data, name))
    }
  }
  rep("", nrow(data))
}

placeholder_to_empty <- function(x) {
  x <- clean_text(x)
  x[tolower(x) %in% c("not extracted", "not available", "null", "none")] <- ""
  x
}

parse_csv_schema <- function(path, schema = "") {
  data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(data) == 0L) {
    return(empty_parser_output())
  }
  raw_input_row_index <- seq_len(nrow(data))

  if (identical(schema, "compendex")) {
    title_values <- csv_col(data, "Title")
    accession_values <- csv_col(data, "Accession number")
    doi_values <- csv_col(data, "DOI")
    author_values <- csv_col(data, "Author")
    year_values <- csv_col(data, "Publication year")
    footer_row <- title_values %in% c("Compendex", "Engineering Village") &
      !nzchar(doi_values) &
      !nzchar(author_values) &
      !nzchar(year_values) &
      (title_values == "Engineering Village" | grepl("Copyright", accession_values, fixed = TRUE))
    data <- data[!footer_row, , drop = FALSE]
    raw_input_row_index <- raw_input_row_index[!footer_row]
  }

  rows <- lapply(seq_len(nrow(data)), function(position) {
    i <- raw_input_row_index[[position]]
    row <- data[position, , drop = FALSE]
    row_json <- row_to_json(row)

    if (identical(schema, "ctgov")) {
      title <- first_nonempty(csv_col(row, "brief_title"), csv_col(row, "official_title"))
      abstract <- paste(c(csv_col(row, "brief_summary"), csv_col(row, "detailed_description")), collapse = " ")
      return(parser_row(
        raw_record_index = i,
        raw_record_id = first_nonempty(csv_col(row, "nct_id"), paste0("CTGOV_", i)),
        title = title,
        authors = csv_col(row, "lead_sponsor"),
        year = extract_year_value(csv_col(row, "start_date"), csv_col(row, "study_first_submit_date")),
        publication_date = first_nonempty(csv_col(row, "start_date"), csv_col(row, "study_first_submit_date")),
        journal_or_source = "ClinicalTrials.gov",
        nct_id = csv_col(row, "nct_id"),
        url = csv_col(row, "clinicaltrials_url"),
        abstract = abstract,
        keywords = csv_col(row, "keywords"),
        publication_type = csv_col(row, "study_type"),
        document_type = "clinical_trial_registry",
        source_record_id = csv_col(row, "nct_id"),
        raw_record_json = row_json
      ))
    }

    if (identical(schema, "europepmc")) {
      return(parser_row(
        raw_record_index = i,
        raw_record_id = first_nonempty(csv_col(row, "id"), csv_col(row, "doi"), paste0("EUROPEPMC_", i)),
        title = csv_col(row, "title"),
        authors = csv_col(row, "authorString"),
        year = first_nonempty(csv_col(row, "pubYear"), extract_year_value(csv_col(row, "firstPublicationDate"))),
        publication_date = csv_col(row, "firstPublicationDate"),
        journal_or_source = first_nonempty(csv_col(row, "journalTitle"), csv_col(row, "publisher"), csv_col(row, "source")),
        doi = extract_doi_value(csv_col(row, "doi")),
        pmid = csv_col(row, "pmid"),
        pmcid = csv_col(row, "pmcid"),
        url = csv_col(row, "fullTextUrlList"),
        abstract = csv_col(row, "abstractText"),
        keywords = csv_col(row, "keywordList"),
        publication_type = csv_col(row, "pubType"),
        document_type = csv_col(row, "source"),
        source_record_id = csv_col(row, "id"),
        raw_record_json = row_json
      ))
    }

    if (identical(schema, "ieee")) {
      return(parser_row(
        raw_record_index = i,
        raw_record_id = first_nonempty(csv_col(row, "Document Identifier"), csv_col(row, "DOI"), paste0("IEEE_", i)),
        title = csv_col(row, "Document Title"),
        authors = csv_col(row, "Authors"),
        year = first_nonempty(csv_col(row, "Publication Year"), extract_year_value(csv_col(row, "Online Date"), csv_col(row, "Issue Date"), csv_col(row, "Meeting Date"))),
        publication_date = first_nonempty(csv_col(row, "Online Date"), csv_col(row, "Issue Date"), csv_col(row, "Meeting Date")),
        journal_or_source = csv_col(row, "Publication Title"),
        doi = extract_doi_value(csv_col(row, "DOI")),
        url = csv_col(row, "PDF Link"),
        abstract = csv_col(row, "Abstract"),
        keywords = paste(c(csv_col(row, "Author Keywords"), csv_col(row, "IEEE Terms"), csv_col(row, "Mesh_Terms")), collapse = "; "),
        publication_type = csv_col(row, "Publisher"),
        document_type = "ieee_csv",
        source_record_id = csv_col(row, "Document Identifier"),
        raw_record_json = row_json
      ))
    }

    if (identical(schema, "compendex")) {
      return(parser_row(
        raw_record_index = i,
        raw_record_id = first_nonempty(csv_col(row, "Accession number"), csv_col(row, "DOI"), paste0("COMPENDEX_", i)),
        title = csv_col(row, "Title"),
        authors = csv_col(row, "Author"),
        year = first_nonempty(csv_col(row, "Publication year"), extract_year_value(csv_col(row, "Issue date"))),
        publication_date = csv_col(row, "Issue date"),
        journal_or_source = csv_col(row, "Source"),
        doi = extract_doi_value(csv_col(row, "DOI")),
        url = csv_col(row, "Link to ProQuest Dissertations"),
        abstract = csv_col(row, "Abstract"),
        keywords = paste(c(csv_col(row, "Main heading"), csv_col(row, "Controlled/Subject terms"), csv_col(row, "Uncontrolled terms")), collapse = "; "),
        language = csv_col(row, "Language"),
        publication_type = csv_col(row, "Document type"),
        document_type = "compendex_csv",
        source_record_id = csv_col(row, "Accession number"),
        raw_record_json = row_json
      ))
    }

    if (identical(schema, "prospero")) {
      return(parser_row(
        raw_record_index = i,
        raw_record_id = first_nonempty(csv_col(row, "record_id"), paste0("PROSPERO_", i)),
        title = csv_col(row, "title"),
        authors = csv_col(row, "authors"),
        year = first_nonempty(csv_col(row, "year_registered"), extract_year_value(csv_col(row, "date_registered"))),
        publication_date = csv_col(row, "date_registered"),
        journal_or_source = first_nonempty(csv_col(row, "database"), "PROSPERO"),
        crd_id = csv_col(row, "record_id"),
        url = csv_col(row, "url"),
        publication_type = csv_col(row, "entry_type"),
        document_type = "review_registry_overlap",
        source_record_id = csv_col(row, "record_id"),
        raw_record_json = row_json
      ))
    }

    if (identical(schema, "osf")) {
      title <- placeholder_to_empty(csv_col(row, "title"))
      created_or_registered <- placeholder_to_empty(csv_col(row, "date_created_or_registered"))
      return(parser_row(
        raw_record_index = i,
        raw_record_id = first_nonempty(csv_col(row, "osf_id"), csv_col(row, "osf_url"), paste0("OSF_", i)),
        title = title,
        authors = csv_col(row, "contributors"),
        year = extract_year_value(created_or_registered),
        publication_date = created_or_registered,
        journal_or_source = "OSF Registries",
        osf_id = csv_col(row, "osf_id"),
        url = csv_col(row, "osf_url"),
        abstract = csv_col(row, "notes"),
        keywords = csv_col(row, "scope_overlap"),
        publication_type = csv_col(row, "registration_template"),
        document_type = csv_col(row, "role_in_review"),
        source_record_id = first_nonempty(csv_col(row, "osf_id"), csv_col(row, "osf_url")),
        raw_record_json = row_json
      ))
    }

    parser_row(
      raw_record_index = i,
      raw_record_id = paste0("CSV_", i),
      title = csv_first_col(row, c("title", "Title")),
      year = extract_year_value(paste(row, collapse = " ")),
      document_type = schema,
      raw_record_json = row_json,
      import_warning = paste0("No explicit CSV schema mapping for schema: ", schema)
    )
  })
  bind_parser_rows(rows)
}

xml_unescape <- function(x) {
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  clean_text(x)
}

xml_first_tag <- function(block, tag) {
  pattern <- paste0("(?s)<", tag, "([^>]*)>(.*?)</", tag, ">")
  match <- regexec(pattern, block, perl = TRUE)
  parts <- regmatches(block, match)[[1L]]
  if (length(parts) < 3L) {
    return("")
  }
  xml_unescape(parts[[3L]])
}

xml_all_name_tags <- function(block) {
  matches <- gregexpr("<name>(.*?)</name>", block, perl = TRUE)
  parts <- regmatches(block, matches)[[1L]]
  if (length(parts) == 1L && identical(parts[[1L]], "")) {
    return("")
  }
  values <- gsub("^<name>|</name>$", "", parts)
  paste(vapply(values, xml_unescape, character(1L)), collapse = "; ")
}

xml_arxiv_id <- function(id_url) {
  id <- sub("^https?://arxiv[.]org/abs/", "", id_url)
  sub("v[0-9]+$", "", id)
}

parse_arxiv_atom <- function(path) {
  text <- paste(read_text_lines(path), collapse = "\n")
  matches <- gregexpr("(?s)<entry>(.*?)</entry>", text, perl = TRUE)
  entries <- regmatches(text, matches)[[1L]]
  if (length(entries) == 1L && identical(entries[[1L]], "")) {
    return(empty_parser_output())
  }

  rows <- lapply(seq_along(entries), function(i) {
    block <- entries[[i]]
    id_url <- xml_first_tag(block, "id")
    doi <- xml_first_tag(block, "arxiv:doi")
    title <- xml_first_tag(block, "title")
    published <- xml_first_tag(block, "published")
    parser_row(
      raw_record_index = i,
      raw_record_id = first_nonempty(xml_arxiv_id(id_url), id_url, paste0("ARXIV_", i)),
      title = title,
      authors = xml_all_name_tags(block),
      year = extract_year_value(published),
      publication_date = published,
      journal_or_source = first_nonempty(xml_first_tag(block, "arxiv:journal_ref"), "arXiv"),
      doi = extract_doi_value(doi),
      arxiv_id = xml_arxiv_id(id_url),
      url = id_url,
      abstract = xml_first_tag(block, "summary"),
      keywords = "",
      publication_type = "preprint",
      document_type = "arxiv_atom",
      source_record_id = xml_arxiv_id(id_url),
      raw_record_text = block
    )
  })
  bind_parser_rows(rows)
}

dispatch_manifest_parser <- function(path, parser) {
  if (identical(parser, "parse_nbib()")) {
    return(parse_nbib(path))
  }
  if (grepl("^parse_ris", parser)) {
    dialect <- sub('^parse_ris\\(dialect = "([^"]+)"\\)$', "\\1", parser)
    return(parse_ris(path, dialect = dialect))
  }
  if (grepl("^parse_bibtex", parser)) {
    dialect <- sub('^parse_bibtex\\(dialect = "([^"]+)"\\)$', "\\1", parser)
    return(parse_bibtex(path, dialect = dialect))
  }
  if (grepl("^parse_csv", parser)) {
    schema <- sub('^parse_csv\\(schema = "([^"]+)"\\)$', "\\1", parser)
    return(parse_csv_schema(path, schema = schema))
  }
  if (identical(parser, "parse_arxiv_atom()")) {
    return(parse_arxiv_atom(path))
  }
  stop("Unsupported parser declaration: ", parser, call. = FALSE)
}
