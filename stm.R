library(tidyverse)
library(pdftools)
library(quanteda)
library(stm)
library(here)
library(KoNLP)
library(tidytext)

# Helpers ======================================================================
particle_suffixes <- c(
  "과", "와", "은", "는", "이", "가", "을", "를", "의",
  "하고", "하겠", "읍니다", "하여야", "있읍니다", "있읍니",
  "께서", "도", "으냐", "그러나", "드리고", "한다고", "하면",
  "주시지", "주세요", "으로", "으로써", "에나", "읍니다만", 
  "우리", "여러분", "것입니다", "것입니", "대해서",
  "특히", "통해서", "중에", "에라도", "이런", "합니다", "말씀드립니다마",
  "등등", "간에", "어떻게", "그렇다면", "것만이라도", "그러한",
  "칩니다", "주십시오", "빕니다", "이였으나", 
  "가지고", "있어서", "이것", "에", "하에"
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
  ## purrr::
  map_dfr(extract_metadata) %>%
  mutate(
    text = map_chr(path, ~ paste(pdf_text(.x), collapse = "\n")),
    tokens = map(text, extract_tokens)
  )

# Document-term matrix (DTM) ===================================================
## Sometimes called DFM (document-feature matrix)
tokens_assembly <- quanteda::tokens(corpus$tokens)
class(tokens_assembly)

dfm_all <- dfm(tokens_assembly)
dim(dfm_all)

## sparsity problem을 어떻게든 하기 위해서
dfm_trimmed <- dfm(tokens_assembly) %>%
  dfm_trim(min_docfreq = 2, min_termfreq = 5) 
dim(dfm_trimmed)

# TF-IDF =======================================================================
## (Cleaning X)
tfidf_assembly <- corpus %>%
  unnest_tokens(word, text) %>%
  group_by(file) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, file, n) %>%
  arrange(desc(tf_idf))

# Structural topic modeling ====================================================
stm_assembly <- convert(
  dfm_trimmed,
  to = "stm",
  docvars = corpus %>% select(date, post_april19, file)
)

## How to set K?
k_grid <- 3:7
search_assembly <- searchK(
  documents = stm_assembly$documents,
  vocab = stm_assembly$vocab,
  K = k_grid,
  prevalence = ~ post_april19,
  data = stm_assembly$meta,
  init.type = "Spectral",
  seed = 1234
)

View(search_assembly$results)
## High K vs. low K
## Exclusivity 
## Semantic Coherence 
## Heldout likelihood

## Final model fitting
final_assembly <- stm(
  documents = stm_assembly$documents,
  vocab = stm_assembly$vocab,
  K = 6,
  prevalence = ~ post_april19,
  data = stm_assembly$meta,
  init.type = "Spectral",
  seed = 1234
)

# Top words ====================================================================
out <- labelTopics(final_assembly, n = 10)
out


