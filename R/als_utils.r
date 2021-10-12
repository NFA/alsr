validate_report <- function(file) {
   page_texts <- pdftools::pdf_text(file)
   page1 <- strsplit(page_texts, "\r\n")[1]

   is_als <- grepl("ALS Scandinavia AB", page1)
   is_gba <- grepl("GBA Gesellschaft für Bioanalytik", page1)

   return(is_als | is_gba)
}

ALS_version <- function(file) {
   page_texts <- pdftools::pdf_text(file)
   page1 <- strsplit(page_texts, "\r\n")[1]

   if (grepl("Analyscertifikat", page1)) {
      return("V2")
   } else {
      return("V1")
   }
}


ALS_dataframe <- function(tbl_list, merge_tables, metadata) {
   tbl_list <- filter_als_tables(tbl_list)
   tbl_list <- tbls_to_dataframe(tbl_list, metadata)
   if (merge_tables) {
      tbls <- do.call("rbind", tbl_list)
      return(tbls)
   }
   tbl_list
}

filter_als_tables <- function(tbls) {
   correct_dims <- function(x) { dim(x)[2] >= 7}
   tbls <- tbls[sapply(tbls, correct_dims)]
   tbls
}

tbls_to_dataframe <- function(tbls, metadata) {
   tbls <- lapply(tbls, tbl_to_dataframe, metadata = metadata)
}

tbl_to_dataframe <- function(tbl, metadata) {
   tbl_header <- extract_ALS_header(tbl)
   tbl <- tbl[tbl_header$start:nrow(tbl), 1:4]
   tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
   colnames(tbl) <- c("Parameter", "Result", "Uncertainty", "Unit")

   tbl <- cbind(
      Analysis = metadata$analysis,
      Date = tbl_header$Date,
      Sampler = tbl_header$Sampler,
      Sample = tbl_header$Sample,
      Info = tbl_header$Info,
      Labnumber = tbl_header$LabID,
      tbl,
      BQL = FALSE
   )

   tbl[, c("Analysis", "Date", "Sampler", "Sample", "Info", "ID", "Parameter", "Result", "BQL", "Uncertainty", "Unit")]
   tbl <- format_tbl(tbl)
   tbl
}

extract_ALS_header <- function(tbl) {
   header <- list(
      Sample = "sample_name",
      Info = NA,
      Sampler = "sampler",
      Date = "1970-01-01",
      LabID = "unknown",
      start = 0
   )

   # The header part for each table stop at the line before "Parameter"
   end <- grep("Parameter", tbl) - 1L
   header$start <- end + 2L
   tbl_header <- tbl[1:end,]
   len <- dim(header)[1]


   # Find match indices in the table header
   matches <- grep("(Er beteckning|Provtagare|Provtagningsdatum|Labnummer)", tbl_header)
   match_ids <- seq_along(matches)

   header_fragments <- c()
   # Loop through the matches and concatenate the information
   for (match_id in match_ids) {
      start <- matches[match_id]
      end <- ifelse(match_id >= length(match_ids), start, matches[match_id + 1] - 1)
      lines <- seq(start, end)

      fragment <- tbl_header[lines, ]
      if (is.null(dim(fragment))) dim(fragment) <- c(1, length(fragment))

      fragment <- apply(fragment, 1, paste0, collapse = " ")
      fragment <- trimws(fragment)
      fragment <- fragment[fragment != ""]
      # Second row seems to indicate analysis specific
      fragment_length <- length(fragment)
      if (fragment_length > 1 & grepl("Er beteckning", fragment[1])) {
         header$Info <- paste(fragment[2:fragment_length], collapse = " ")
         fragment <- fragment[1]
      }

      header_fragments <- append(header_fragments, fragment)
   }

   match <- grep("Er beteckning", header_fragments, value = TRUE)
   if (length(match)) {
      header$Sample <- substr(match, 15, nchar(match))
   }

   match <- grep("Provtagare", header_fragments, value = TRUE)
   if (length(match)) {
      header$Sampler <- substr(match, 12, nchar(match))
   }

   match <- grep("Provtagningsdatum", header_fragments, value = TRUE)
   if (length(match)) {
      header$Date <- substr(match, 19, nchar(match))
   }

   match <- grep("Labnummer", header_fragments, value = TRUE)
   if (length(match)) {
      header$LabID <- substr(match, 11, nchar(match))
   }

   return(header)
}

format_tbl <- function(tbl) {
   tbl[, "Date"] <- as.Date(tbl[, "Date"])
   # Convert uncertainties to number
   tbl[, "Uncertainty"] <- as.als_number(tbl[, "Uncertainty"])
   tbl[, "BQL"] <- startsWith(tbl[, "Result"], "<")
   tbl[, "Result"] <- as.als_number(tbl[, "Result"])
   tbl[, "Parameter"] <- factor(tbl[, "Parameter"])

   tbl
}

as.als_number <- function(x) {
   as.numeric(sub("\\D*([0-9.]+)\\D*", "\\1", x))
}
