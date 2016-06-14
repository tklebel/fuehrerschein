#' Find variable label
#'
#' @param .data A data.frame containig the labels
#' @param .var A character string of length one with the name of the variable
#'
#' @return The variable lable of the requested variable.
#' @export
find_label <- function(.data, .var) {
  .data %>%
    dplyr::filter(names == .var) %>%
    dplyr::select(variable.labels) %>%
    unlist() %>%
    unname()
}
