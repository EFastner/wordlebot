allowed_letters <- c('n')
not_allowed_letters <- c('w', 'e', 'd', 's', 'v')
locked_letters <- '.a.al'

possible_answers <-
  c(answerlist,wordlist) %>%
  grep(
    pattern = locked_letters,
    x = .,
    value = TRUE,
    perl = TRUE,
    ignore.case = TRUE
  ) %>%
  grep(
    pattern = paste0("^((?![", paste0(not_allowed_letters, collapse = ""), "]).)*$"),
    x = .,
    value = TRUE,
    perl = TRUE,
    ignore.case = TRUE) %>%
  grep(
    pattern = paste0(allowed_letters, collapse = ""),
    x = .,
    value = TRUE,
    perl = TRUE,
    ignore.case = TRUE
  )

