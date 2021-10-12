setClass("ALSReport",
         representation(
            filename = "character",
            orderid = "character",
            arrived = "Date",
            issued = "Date",
            replacement = "logical",
            project = "character",
            analyses = "character",
            # Analysis is in the report header for V1 reports, so have to parse it elsewhere from V2 reports
            analysis_v1 = "character",
            gba_report = "character",
            gba_order = "character",
            gba_no = "character",
            samples = "character",
            tables = "list",
            raw_page_data = "list"),
         prototype(
            filename = NA_character_,
            orderid = NA_character_,
            arrived = lubridate::NA_Date_,
            issued = lubridate::NA_Date_,
            replacement = FALSE,
            project = NA_character_,
            analyses = NA_character_,
            gba_report = NA_character_,
            gba_order = NA_character_,
            gba_no = NA_character_,
            analysis_v1 = NA_character_,
            samples = NA_character_
         ))

setClass("ALSReport-V1", contains = "ALSReport")
setClass("ALSReport-V2", contains = "ALSReport")
setClass("GBAReport", contains = "ALSReport")

setMethod("show", "ALSReport", function(object) {
   cli::cat_line(stringr::str_glue("# ALS Report ({als.version(object)})"), col = "grey")
   cli::cat_line("# Filename: ", object@filename, col = "grey")
   cli::cat_line("# Order ID: ", object@orderid, col = "grey")
   cli::cat_line("# Arrived: ", object@arrived, col = "grey")
   cli::cat_line("# Issued: ", object@issued, col = "grey")
   cli::cat_line("# Project: ", object@project, col = "grey")
   if (als.version(object) == "GBA") {
      cli::cat_line("# GBA specific", col = "grey")
      cli::cat_line("# \tTest Report No: ", object@gba_report, col = "grey")
      cli::cat_line("# \tOrder: ", pander::p(object@gba_order, wrap = ""), col = "grey")
      cli::cat_line("# \tGBA-No: ", object@gba_no, col = "grey")

   }
   cli::cat_line("# Analyses: ", pander::p(object@analyses, wrap = ""), col = "grey")
   cli::cat_line("# Samples: ", pander::p(object@samples, wrap = ""), col = "grey")
   cli::cat_line("# Number of tables: ", length(object@tables) , col = "grey")
   if (object@replacement)
      cli::cat_line("# Replacement: ", object@arrived, col = "grey")
})

setGeneric("als.version", function(object) standardGeneric("als.version"))
setGeneric("parse_metadata", function(object) standardGeneric("parse_metadata"))
setGeneric("parse_tables", function(object) standardGeneric("parse_tables"))

create_report <- function(file) {
   page_texts <- pdftools::pdf_text(file)

   if (any(stringr::str_detect(page_texts, "\r\n"))) {
      page_texts <- strsplit(pdftools::pdf_text(file), "\r\n")
   } else {
      page_texts <- strsplit(pdftools::pdf_text(file), "\n")
   }


   if (grepl("ALS Scandinavia AB", page_texts[1])) {
      if (grepl("Analyscertifikat", page_texts[1])) {
         cli::cli_alert_info("ALS V2 report.")
         return(new("ALSReport-V2", filename = basename(file), raw_page_data = page_texts))
      } else {
         cli::cli_alert_info("ALS V1 report.")
         return(new("ALSReport-V1", filename = basename(file), raw_page_data = page_texts))
      }
   } else if (grepl("GBA Gesellschaft für Bioanalytik", page_texts[1])) {
      cli::cli_alert_info("GBA report.")
      return(new("GBAReport", filename = basename(file), raw_page_data = page_texts))
   }
}

