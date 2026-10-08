# Reúne as regras de prioridade, as chaves estáveis e as medidas de semelhança.
# Essas funções ajudam a identificar duplicatas sem perder a origem dos registros.


file_dedup_has_dated_prefix <- function(file_name) {
  grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}_", file_name)
}

file_dedup_is_bruto_path <- function(relative_path) {
  grepl("(^|/)BRUTO(/|$)", relative_path, ignore.case = TRUE)
}

file_dedup_canonical_order <- function(rows) {
  if (nrow(rows) == 0L) {
    return(integer())
  }

  order(
    -as.integer(file_dedup_has_dated_prefix(rows$file_name)),
    as.integer(file_dedup_is_bruto_path(rows$relative_path)),
    rows$relative_path,
    method = "radix"
  )
}

file_dedup_clinicaltrials_query_number <- function(relative_path) {
  pattern <- "ClinicalTrials_([0-9]{2})_"
  matches <- regexec(pattern, relative_path)
  parts <- regmatches(relative_path, matches)
  vapply(parts, function(x) if (length(x) >= 2L) x[[2L]] else "", character(1L))
}

file_dedup_is_ct_empty_query_evidence <- function(source_bucket_normalized, relative_path, role) {
  query_number <- file_dedup_clinicaltrials_query_number(relative_path)
  identical_source <- source_bucket_normalized == "ClinicalTrials"
  query_number %in% c("08", "15", "18") &
    identical_source &
    role %in% c("export", "api_provenance")
}

record_dedup_raw_index_number <- function(x) {
  out <- suppressWarnings(as.integer(x))
  out[is.na(out)] <- .Machine$integer.max
  out
}

assign_stable_record_ids <- function(records, prefix, key = "record_source_id") {
  if (!key %in% names(records)) stop("Coluna ausente para identificador estável: ", key, call. = FALSE)
  if (any(!nzchar(trimws(as.character(records[[key]]))))) stop("Há chave vazia para identificador estável.", call. = FALSE)
  if (anyDuplicated(records[[key]])) stop("A chave para identificador estável deve ser única após a deduplicação.", call. = FALSE)
  if (!nrow(records)) {
    records$record_id <- character()
    return(records[, c("record_id", setdiff(names(records), "record_id")), drop = FALSE])
  }
  records <- records[order(records[[key]], method = "radix"), , drop = FALSE]
  records$record_id <- sprintf("%s_%05d", prefix, seq_len(nrow(records)))
  records[, c("record_id", setdiff(names(records), "record_id")), drop = FALSE]
}

record_dedup_is_preprint <- function(records) {
  text <- tolower(paste(records$source_database, records$publication_type, records$document_type))
  grepl("arxiv|medrxiv|biorxiv|preprint|ppr", text, perl = TRUE)
}

record_dedup_is_thesis <- function(records) {
  text <- tolower(paste(records$source_database, records$publication_type, records$document_type, records$journal_or_source))
  grepl("proquest|dissertation|thesis|\\bds\\b", text, perl = TRUE)
}

record_dedup_is_conference_abstract <- function(records) {
  text <- tolower(paste(records$publication_type, records$document_type, records$journal_or_source))
  grepl("conference abstract|meeting abstract|conference review|\\babstract\\b", text, perl = TRUE)
}

record_dedup_is_full_conference <- function(records) {
  text <- tolower(paste(records$publication_type, records$document_type, records$journal_or_source))
  grepl("inproceedings|proceedings|conference proceeding|conference article|cpaper|\\bconf\\b|ieee", text, perl = TRUE) &
    !record_dedup_is_conference_abstract(records)
}

record_dedup_is_journal_article <- function(records) {
  text <- tolower(paste(records$publication_type, records$document_type, records$journal_or_source))
  grepl("journal|\\bjour\\b|\\barticle\\b", text, perl = TRUE) &
    !record_dedup_is_preprint(records) &
    !record_dedup_is_thesis(records) &
    !record_dedup_is_full_conference(records) &
    !record_dedup_is_conference_abstract(records)
}

record_dedup_rank_frame <- function(records) {
  has_doi <- nzchar(records$doi_normalized)
  has_abstract <- nzchar(records$abstract)
  is_journal <- record_dedup_is_journal_article(records)
  is_conference <- record_dedup_is_full_conference(records)
  is_preprint <- record_dedup_is_preprint(records)
  is_thesis <- record_dedup_is_thesis(records)
  is_conference_abstract <- record_dedup_is_conference_abstract(records)

  rank_class <- rep(6L, nrow(records))
  rank_class[is_conference_abstract] <- 5L
  rank_class[is_thesis] <- 4L
  rank_class[is_preprint & has_doi & has_abstract] <- 3L
  rank_class[is_conference & has_doi & has_abstract] <- 2L
  rank_class[is_journal & has_doi & has_abstract] <- 1L

  metadata_fields <- c(
    "title",
    "authors",
    "year",
    "publication_date",
    "journal_or_source",
    "doi_normalized",
    "pmid_normalized",
    "arxiv_id_normalized",
    "abstract",
    "keywords",
    "language",
    "publication_type",
    "document_type",
    "url"
  )
  present <- vapply(metadata_fields, function(field) nzchar(records[[field]]), logical(nrow(records)))
  metadata_score <- rowSums(present)

  data.frame(
    rank_class = rank_class,
    abstract_chars = nchar(records$abstract, type = "chars", allowNA = FALSE, keepNA = FALSE),
    metadata_score = metadata_score,
    source_database = records$source_database,
    source_file = records$source_file,
    raw_record_index_number = record_dedup_raw_index_number(records$raw_record_index),
    record_source_id = records$record_source_id,
    stringsAsFactors = FALSE
  )
}

record_dedup_survivor_index <- function(records) {
  ranks <- record_dedup_rank_frame(records)
  order_index <- order(
    ranks$rank_class,
    -ranks$abstract_chars,
    -ranks$metadata_score,
    ranks$source_database,
    ranks$source_file,
    ranks$raw_record_index_number,
    ranks$record_source_id,
    method = "radix"
  )
  order_index[[1L]]
}

record_dedup_key_type <- function(id_key) {
  sub(":.*$", "", id_key, perl = TRUE)
}

record_jaro_similarity <- function(a, b) {
  if (identical(a, b)) {
    return(1)
  }
  if (!nzchar(a) || !nzchar(b)) {
    return(0)
  }

  a_chars <- strsplit(a, "", fixed = TRUE)[[1L]]
  b_chars <- strsplit(b, "", fixed = TRUE)[[1L]]
  a_len <- length(a_chars)
  b_len <- length(b_chars)
  match_distance <- max(floor(max(a_len, b_len) / 2) - 1, 0)

  a_match <- rep(FALSE, a_len)
  b_match <- rep(FALSE, b_len)
  matches <- 0L

  for (i in seq_len(a_len)) {
    start <- max(1L, i - match_distance)
    end <- min(i + match_distance, b_len)
    for (j in start:end) {
      if (!b_match[[j]] && identical(a_chars[[i]], b_chars[[j]])) {
        a_match[[i]] <- TRUE
        b_match[[j]] <- TRUE
        matches <- matches + 1L
        break
      }
    }
  }

  if (matches == 0L) {
    return(0)
  }

  a_matched <- a_chars[a_match]
  b_matched <- b_chars[b_match]
  transpositions <- sum(a_matched != b_matched) / 2

  (
    matches / a_len +
      matches / b_len +
      (matches - transpositions) / matches
  ) / 3
}

record_jaro_winkler_similarity <- function(a, b, prefix_scale = 0.1, max_prefix = 4L) {
  jaro <- record_jaro_similarity(a, b)
  if (jaro <= 0) {
    return(0)
  }

  a_chars <- strsplit(a, "", fixed = TRUE)[[1L]]
  b_chars <- strsplit(b, "", fixed = TRUE)[[1L]]
  prefix <- 0L
  for (i in seq_len(min(max_prefix, length(a_chars), length(b_chars)))) {
    if (!identical(a_chars[[i]], b_chars[[i]])) {
      break
    }
    prefix <- prefix + 1L
  }

  jaro + prefix * prefix_scale * (1 - jaro)
}
