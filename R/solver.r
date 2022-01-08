# @export
solve_wordle <- function(){
  is_solved <- "n"
  allowed_letters <- ""
  not_allowed_letters <- ""

  first_guess <- sample(c(answerlist,wordlist), 1)

  cat(
    paste0(
      'Enter your first word\n',
      'Need an idea? Try ',
      first_guess,
      '\n'
    )
  )

  while (is_solved == "n") {

    if (tolower(readline(prompt = "Did we Solve It!? (y/n): ")) == "y"){
      cat("Awesome!\n")

      is_solved = "y"
    } else {

      letter_params <- get_letters()

      locked_positions <- letter_params[[1]]
      locked_letters <- gsub(pattern = "[.]", replacement = "", x = locked_positions)

      not_allowed_letters <- paste0(not_allowed_letters, letter_params[[3]], collapse = "")

      allowed_letters <- paste0(allowed_letters, letter_params[[2]], collapse = "")

      if(nchar(locked_letters) > 0){
        allowed_letters <-
          allowed_letters %>%
          gsub(pattern = paste0("[", locked_letters, "]", collapse = ""), replacement = "", x = .)

        not_allowed_letters <-
          not_allowed_letters %>%
          gsub(pattern = paste0("[", locked_letters, "]", collapse = ""), replacement = "", x = .)
      }

      possible_answers <-
        c(answerlist,wordlist) %>%
        grep(
          pattern = locked_positions,
          x = .,
          value = TRUE,
          perl = TRUE,
          ignore.case = TRUE
        )

      if(nchar(not_allowed_letters) > 0){
        possible_answers <-
          possible_answers %>%
          grep(
            pattern = paste0("^((?![", paste0(not_allowed_letters, collapse = ""), "]).)*$"),
            x = .,
            value = TRUE,
            perl = TRUE,
            ignore.case = TRUE)
      }

      if(nchar(allowed_letters) > 0){
        possible_answers <-
          possible_answers %>%
          grep(
            pattern = paste0(allowed_letters, collapse = ""),
            x = .,
            value = TRUE,
            perl = TRUE,
            ignore.case = TRUE
          )
      }

      cat("\nPossible Answers:\n")

      print(possible_answers)

      cat("\nEnter another guess\n")
    }

  }

}

get_letters <- function(){

  locked_positions <- NULL

  while (is.null(locked_positions)){
    locked_positions <-
      readline(prompt = 'Enter the positions of your green letters (Use "." if unknown): ') %>%
      gsub(pattern = "[^a-z.]", replacement = "", x = ., ignore.case = TRUE) %>%
      check_sequence()

    if (is.null(locked_positions)) {
      cat("Your sequence is too many letters! Try again")
    }
  }

  allowed_letters <-
    readline(prompt = 'Enter yellow letters (Leave Blank if None): ') %>%
    gsub(pattern = "[^a-z]", replacement = "", x = ., ignore.case = TRUE)

  not_allowed_letters <-
    readline(prompt = 'Enter gray letters (Leave Blank if None): ') %>%
    gsub(pattern = "[^a-z]", replacement = "", x = ., ignore.case = TRUE)

  return(list(locked_positions, allowed_letters, not_allowed_letters))
}

check_sequence <- function(pattern_string, string_length = 5) {

  if(nchar(pattern_string) > string_length){

    return(NULL)

  } else {

    return(paste0(pattern_string, replicate(string_length - nchar(pattern_string), "."), collapse = ""))

  }
}

