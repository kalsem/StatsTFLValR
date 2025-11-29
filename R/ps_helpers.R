# R/wrappers.R  (internal helpers; not exported)

ps_rm                <- function(...) base::rm(...)
ps_setwd             <- function(...) base::setwd(...)
ps_getwd             <- function(...) base::getwd(...)
ps_dir_exists        <- function(...) base::dir.exists(...)
ps_dir_create        <- function(...) base::dir.create(...)
ps_file_exists       <- function(...) base::file.exists(...)
ps_source            <- function(...) base::source(...)
ps_requireNamespace  <- function(...) base::requireNamespace(...)
ps_library           <- function(pkg, character.only = FALSE) base::library(pkg, character.only = character.only)


# R/wrappers.R  -- internal, not exported

ps_list_files  <- function(...) base::list.files(...)
ps_read_sas    <- function(...) haven::read_sas(...)
ps_read_xpt    <- function(...) haven::read_xpt(...)
ps_read_csv    <- function(...) utils::read.csv(...)
ps_read_excel  <- function(...) readxl::read_excel(...)
ps_assign      <- function(name, value, envir = .GlobalEnv) base::assign(name, value, envir = envir)
ps_message     <- function(...) base::message(...)
