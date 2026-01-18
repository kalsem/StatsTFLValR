# R/wrappers.R  (internal helpers; not exported)
# CRAN-safe: avoid helpers that change working directory or modify user workspace.

ps_getwd            <- function(...) base::getwd(...)
ps_dir_exists       <- function(...) base::dir.exists(...)
ps_dir_create       <- function(...) base::dir.create(...)
ps_file_exists      <- function(...) base::file.exists(...)
ps_source           <- function(...) base::source(...)

# Prefer requireNamespace() and pkg::fun usage in package code
ps_requireNamespace <- function(pkg, quietly = TRUE) {
  base::requireNamespace(pkg, quietly = quietly)
}

# Readers (OK)
ps_read_sas   <- function(...) haven::read_sas(...)
ps_read_xpt   <- function(...) haven::read_xpt(...)
ps_read_csv   <- function(...) utils::read.csv(...)
ps_read_excel <- function(...) readxl::read_excel(...)

ps_message <- function(...) base::message(...)
