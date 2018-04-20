#' Generate coefficient path plot.
#'
#' @param coef_df A data frame with 3 columns: \code{covariate}, \code{lambda},
#'   \code{value}.
#' @param best_lambda A scalar for the lambda value at which the cv score is
#'   lowest. For plotting a vertical line for the selected coefficient values.
#' @param style A character scalar. Possible values are \code{c("base", "clean")}.
#'   \code{"base"} returns a ggplot object using the base (gray) theme. \code{"clean"}
#'   returns a ggplot object using \code{theme_few}. Default is \code{"base"}.
#' @return A ggplot object.
make_coef_plot <- function(coef_df, best_lambda, style = "base") {
  if(style == "base") ggplot2::theme_set(theme_gray())
  if(style == "clean") ggplot2::theme_set(theme_few())

  ggplot2::ggplot(coef_df, aes(x = lambda, y = value, color = covariate))+
    ggplot2::geom_vline(aes(xintercept = best_lambda),
               color = "grey65",
               linetype = "dashed")+
    ggplot2::geom_line()+
    ggplot2::xlab(expression(log(lambda)))+
    ggplot2::ylab("Coefficient value")+
    ggplot2::theme(legend.position = "bottom")
}


#' Generate cross-validation score path plot.
#'
#' @param cvscore_df A data frame with 2 columns: \code{cvscore}, \code{lambda}.
#' @param score_name A string identifying the type of score used, e.g.,
#'   \code{"MSE"} for mean square error.
#' @param best_lambda A scalar for the lambda value at which the cv score is
#'   lowest. For plotting a vertical line for the selected coefficient values.
#' @param style A character scalar. Possible values are \code{c("base", "clean")}.
#'   \code{"base"} returns a ggplot object using the base (gray) theme. \code{"clean"}
#'   returns a ggplot object using \code{theme_few}. Default is \code{"base"}.
#' @return A ggplot object.
make_cvscore_plot <- function(cvscore_df, score_name, best_lambda, style = "base") {
  if(style == "base") ggplot2::theme_set(theme_gray())
  if(style == "clean") ggplot2::theme_set(theme_few())

  ggplot2::ggplot(mse_df, aes(x = lambda, y = score))+
    ggplot2::geom_vline(aes(xintercept = best_lambda),
                        color = "grey65",
                        linetype = "dashed")+
    ggplot2::geom_line()+
    ggplot2::xlab(expression(log(lambda)))+
    ggplot2::ylab(paste("Cross-validation", score_name))
}
