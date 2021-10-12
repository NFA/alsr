split_tables <- function(tables_text) {
   tbl_starts <- grep("Er beteckning", tables_text)
   tbl_num <- length(tbl_starts)
   pg_length <- length(tables_text)

   # Edges mark all the start/end locations of tables
   tbl_edges <- c(tbl_starts, pg_length + 1)
   # Calculate the pairwise distance between tables
   tbl_sizes <- diff(tbl_edges)

   # split the table text into respective tables
   unname(split(tables_text, rep(1:tbl_num, tbl_sizes)))
}

count_ws <- function(texts) {
   spaces_lst <- stringr::str_split(texts, "[^\\s]")
   spaces_cnt <- lapply(spaces_lst, stringr::str_length)
   spaces_cnt <- lapply(spaces_cnt, function(num) num[num != 0])
   spaces_cnt
}

sanitize_input <- function(text) {
 # replace_v <- c(
 #    #Results
 #       "Malning stålfat" = "steel plate grinding",
 #       "glödförlust" = "loss on ignition",
 #       "ammoniumkvä"
 #
 #       # Units
 #       "% av TS"    = "%#av#TS",
 #       "mg/kg TS"   = "mg/kg#TS",
 #       "µg/kg TS"   = "µg/kg#TS",
 #       "mgCa/kg TS" = "mgCa/kg#TS"
 #    )

   #stringr::str_replace_all(text, replace_v)
   text <- text[!stringr::str_detect(text, "GBA")]
   text <- text[!stringr::str_detect(text, "Rapporteringsgränsen för")]
   #text
}

column_correction <- function(text) {
   sp <- stringr::str_dup(" ", 10)
   stringr::str_replace(text, "(\\d+)\\s+(\\d+|\\w)\\s+([A-Z]{2,4})$", paste0("\\1", sp, "\\2", sp, "\\3"))
}

fwf_correction <- function(header, body) {
   res <- body

   # Add uncertainty column incase it does not exist
   loc <- stringr::str_locate(header, "Mätosäkerhet|Osäkerhet") + c(1, -1)
   dashes <- stringr::str_dup("-", loc[,"end"] - loc[, "start"] - 1)
   column <- stringr::str_sub(res, loc[, "start"], loc[, "end"]) %>% str_squish()
   column <- column[column != ""]
   if (!length(column)) {
      stringr::str_sub(res, loc[,"start"], loc[, "end"]) <- dashes
   }

   # Change single spaces surrounded by word boundary
   res <- stringr::str_replace_all(res, "(?<=(\\b|\\)))[ ]{1}(?=(\\b|\\())", "_")


   # Change umlaut characters
   # Reason: fwf_empty goes through C++, where it iterates on bytes instead of
   # codepoints. Umlaut letters are two bytes in UTF-8 so column positions are
   # wrong
   umlauts <- c("å" = "a", "ä" = "a", "ö" = "o",
                "Å" = "A", "Ä" = "A", "Ö" = "ö")
   res <- stringr::str_replace_all(res, umlauts)

   res
}


