#' Autmated bar plots
#'
#' This functions returns bar plots for all the factors in a data set.
#'
#' @param .data A \code{data.frame}.
#' @param .labels A second \code{data.frame}, containing the variables with
#'  corresponding labels to set titles
#'
#' @return Plots as side effect.
#' @export
plot_factors <- function(.data, .labels = NULL) {
  reshaped <- .data %>%
    dplyr::select_if(is.factor) %>%
    reshape_data()

    purrr::map(reshaped, plot_bar, .labels = .labels)
}

#' Plot bar for factors
#'
#' @param .data A \code{data.frame} with the variable of interest named
#'    \code{wert}.
#'
#' @keywords internal
#' @noRd
plot_bar <- function(.data, .labels) {
  title <- NULL
  if (!is.null(.labels)) {
    # find title for graph
    var <- .data[[1]][1]
    title <- find_label(.labels, var)
  }

  ggplot(.data, aes(wert)) +
    geom_bar() +
    scale_x_discrete(drop = F) +
    labs(x = NULL,
         y = NULL,
         title = title) +
    theme_bw()
}



#' Reshape to long form ans split by var
#'
#' @param .data A \code{data.frame}.
#'
#' @return A list with as many elements as there were variables in the original
#'    data.
#' @keywords internal
#' @noRd
reshape_data <- function(.data) {
  .data %>%
    tidyr::gather(var, wert) %>%
    dplyr::filter(!is.na(wert)) %>%
    split(.$var)
}
