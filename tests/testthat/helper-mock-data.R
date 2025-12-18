mock_adsl <- function(n = 60, seed = 1) {
  set.seed(seed)
  tibble::tibble(
    STUDYID  = "TESTSTUDY",
    USUBJID  = sprintf("SUBJ%03d", 1:n),
    TRT01AN  = sample(c(1,2,3), n, replace = TRUE),
    TRTAN    = TRT01AN,
    SEX      = sample(c("F","M"), n, replace = TRUE),
    REGION   = sample(c("NA","EU","APAC"), n, replace = TRUE),
    SAFFL    = "Y"
  )
}

mock_adae <- function(adsl, seed = 2) {
  set.seed(seed)
  n <- nrow(adsl)
  m <- n * 2
  tibble::tibble(
    STUDYID   = "TESTSTUDY",
    USUBJID   = sample(adsl$USUBJID, m, replace = TRUE),
    TRTAN    = adsl$TRTAN[match(USUBJID, adsl$USUBJID)],
    AEBODSYS  = sample(c("GASTROINTESTINAL DISORDERS","NERVOUS SYSTEM DISORDERS"), m, replace = TRUE),
    AEDECOD   = sample(c("NAUSEA","VOMITING","HEADACHE"), m, replace = TRUE),
    AETOXGRN  = sample(c(1,2,3,4,5, NA), m, replace = TRUE, prob = c(.25,.25,.2,.15,.1,.05)),
    TRTEMFL   = "Y"
  )
}
