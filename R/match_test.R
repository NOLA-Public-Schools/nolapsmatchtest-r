#' Verify that lottery output meets constraints imposed by source data
#'
#' @param dir_review character
#' @param match tibble of choice-level match outcomes
#' @param gradelevels tibble of grade level records
#' @param contactsmatch tibble of contact records
#' @param choices tibble of application school ranking records
#' @param eps_gradelevel tibble of eligibility and priority information
#' for all grade levels in the lottery
#' @param eps_choice tibble of eligibility and priority information
#' for all application school rankings in the lottery
#'
#' @export
match_test <- function(
    dir_review, match,
    gradelevels, contactsmatch, choices, eps_gradelevel, eps_choice) {
  cat("\nValidating match file.\n")

  students_active <- contactsmatch |> filter(.data$is_active)

  match_test_grades(
    dir_review = dir_review,
    match = match
  )

  match_test_age(
    dir_review = dir_review,
    match = match
  )

  match_test_choices(
    dir_review = dir_review,
    match = match,
    choices = choices
  )

  match_test_eligibility_k12(
    dir_review = dir_review,
    match = match,
    choices = choices
  )

  match_test_eligibility_ec(
    dir_review = dir_review,
    match = match,
    choices = choices
  )

  match_test_guarantee(
    dir_review = dir_review,
    match = match,
    students_active = students_active
  )

  match_test_priorities(
    dir_review = dir_review,
    match = match,
    eps_gradelevel = eps_gradelevel,
    eps_choice = eps_choice
  )
}
