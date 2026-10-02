# Polyfill de testthat::expect_shape().
# Introducida en testthat 3.3.2.9000 (development); aun no disponible en CRAN.
# Eliminar este helper cuando DESCRIPTION exija una version de testthat que
# ya provea expect_shape() exportada.
# Referencia: https://github.com/r-lib/testthat/blob/main/R/expect-shape.R
expect_shape <- function(object, ..., nrow, ncol, dim) {
  rlang::check_dots_empty()
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  dim_object <- base::dim(object)
  if (is.null(dim_object)) {
    testthat::fail(sprintf("Expected %s to have dimensions.", act$lab))
    return(invisible(act$val))
  }

  if (!missing(nrow)) {
    act$nrow <- dim_object[1L]
    if (!identical(as.integer(act$nrow), as.integer(nrow))) {
      testthat::fail(c(
        sprintf("Expected %s to have %i rows.", act$lab, nrow),
        sprintf("Actual rows: %i.", act$nrow)
      ))
    } else {
      testthat::succeed()
    }
  } else if (!missing(ncol)) {
    if (length(dim_object) == 1L) {
      testthat::fail(sprintf("Expected %s to have two or more dimensions.", act$lab))
    } else {
      act$ncol <- dim_object[2L]
      if (!identical(as.integer(act$ncol), as.integer(ncol))) {
        testthat::fail(c(
          sprintf("Expected %s to have %i columns.", act$lab, ncol),
          sprintf("Actual columns: %i.", act$ncol)
        ))
      } else {
        testthat::succeed()
      }
    }
  } else if (!missing(dim)) {
    if (!is.numeric(dim) && !is.integer(dim)) {
      stop("`dim` must be a numeric vector.", call. = FALSE)
    }
    act$dim <- dim_object
    if (length(act$dim) != length(dim)) {
      testthat::fail(c(
        sprintf("Expected %s to have %i dimensions.", act$lab, length(dim)),
        sprintf("Actual dimensions: %i.", length(act$dim))
      ))
    } else if (!identical(as.integer(act$dim), as.integer(dim))) {
      testthat::fail(c(
        sprintf("Expected %s to have dim (%s).", act$lab, toString(dim)),
        sprintf("Actual dim: (%s).", toString(act$dim))
      ))
    } else {
      testthat::succeed()
    }
  } else {
    stop("Provee uno de `nrow`, `ncol` o `dim`.", call. = FALSE)
  }

  invisible(act$val)
}
