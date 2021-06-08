count_na <- function(data, column) {
  data %>%
    dplyr::pull({{ column }}) %>%
    is.na() %>%
    sum()
}

expect_no_na <- function(data, column) {
  na_count <- count_na(data, {{ column }})

  if (na_count == 0) {
    testthat::succeed()
    return(invisible(data))
  }

  column_name <- substitute(column)
  message <- glue::glue("{column_name} has {na_count} NA's")
  testthat::fail(message)
}


in_between <- function(x, min, max, tolerance = 0L) {
  x >= (min - tolerance) & x <= (max + tolerance)
}

tt_tol <- testthat::testthat_tolerance()

in_range <- function(data, column, min, max, tolerance = tt_tol) {
  data %>%
    dplyr::filter(!is.na({{ column }})) %>%
    dplyr::pull({{ column }}) %>%
    in_between(min, max, tolerance) %>%
    all()
}

expect_between <- function(data, column, min, max, tolerance = tt_tol) {
  if (in_range(data, {{ column }}, min, max, tolerance)) {
    testthat::succeed()
    return(invisible(data))
  }

  column_name <- substitute(column)
  message <- glue::glue("{column_name} isn't between {min} and {max}")
  testthat::fail(message)
}



get_value <- function(data, column, ...) {
  f <- dplyr::quos(...)
  data %>%
    dplyr::filter(!!!f) %>%
    dplyr::pull({{ column }})
}


