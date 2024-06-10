grades_ec <- function() {
  c("INF", "1YR", "2YR", "PK3", "PK4")
}

grades_k12 <- function() {
  c("K", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
}

test_helper <- function(bad, test_text) {
  testthat::with_reporter(
    "stop",
    {
      testthat::test_that(test_text, {
        testthat::expect_equal(nrow(bad), 0)
      })
    }
  )
}

write_if_bad <- function(x, dir_out) {
  if (nrow(x) > 0) {
    filename <- deparse(substitute(x))
    readr::write_csv(x, glue("{dir_out}/{filename}.csv"), na = "")
  }
}
