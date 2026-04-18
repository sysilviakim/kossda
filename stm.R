library(tidyverse)
library(pdftools)
library(quanteda)
library(stm)
library(here)
library(KoNLP)

# Helpers ======================================================================
particle_suffixes <- c(
  "과", "와", "은", "는", "이", "가", "을", "를"
)

extract_metadata <- function(path) {
  file <- basename(path)
  tibble(
    path = path,
    file = file,
    date = as.Date(
      str_extract(file, "\\d{4}\\.\\d{2}\\.\\d{2}"),
      format = "%Y.%m.%d"
    ),
    post_april19 = date >= as.Date("1960-04-19")
  )
}

extract_tokens <- function(text) {
  cleaned <- text %>%
    str_replace_all("[0-9]+", " ") %>%
    ## Match any single character that is NOT:
    ## a Hangul (Korean) character OR whitespace
    str_replace_all("[^\\p{Hangul}\\s]", " ") %>%
    ## If multiple whitespaces, squish into a single one
    str_squish()
  
  str_extract_all(cleaned, "[\\p{Hangul}]{2,}")[[1]] %>%
    str_squish() %>%
    str_remove(
      paste0("(", paste(particle_suffixes, collapse = "|"), ")$")
    ) %>%
    keep(~ nchar(.x) > 1)
}

# Build corpus =================================================================
pdf_list <- list.files(
  here("data"),
  pattern = "*.PDF$",
  full.names = TRUE
)

corpus <- pdf_list %>%
  map_dfr(extract_metadata) %>%
  mutate(
    text = map_chr(path, ~ paste(pdf_text(.x), collapse = "\n")),
    tokens = map(text, extract_tokens)
  )

# Document-term matrix (DTM) ===================================================
## Sometimes called DFM (document-feature matrix)
