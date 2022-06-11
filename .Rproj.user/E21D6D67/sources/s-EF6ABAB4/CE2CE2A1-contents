#' Create a figure showing distribution and correlation
#'
#' @param data A data frame contain multiple phenotype of several samples
#' @param binwidth The width of the bins, same as geom_histogram
#'
#' @return A object of a figure
#' @export
#'
#' @examples
#' library(plot)
#' pheCorDist(sn)
pheCorDist <- function(data, binwidth = NULL) {
  #
  n <- ncol(data)
  cn <- colnames(data)
  Pall <- list()
  index <- 1
  #
  for (j in 1:n) {
    for (i in 1:n) {
      df <- data[, c(i, j)]
      colnames(df) <- c("col1", "col2")
      if (i == j) {
        p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = col1)) +
          ggplot2::geom_histogram(binwidth = binwidth) +
          ggplot2::labs(x = NULL, y = NULL, title = cn[i]) +
          cowplot::theme_half_open() +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      } else if (i > j) {
        p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = col2, y = col1)) +
          ggpointdensity::geom_pointdensity() +
          ggplot2::scale_color_continuous(type = "viridis") +
          ggplot2::labs(x = NULL, y = NULL, title = paste("*R*: ", round(cor(df$col1, df$col2, use = "na.or.complete"), 2), sep = "")) +
          cowplot::theme_half_open() +
          ggplot2::theme(
            legend.position = "NA",
            plot.title = ggtext::element_markdown(
              hjust = 0.5,
              face = "plain"
            )
          )
      } else if (i < j) {
        p <- ggplot2::ggplot() +
          cowplot::theme_nothing()
      }
      Pall[index][[1]] <- p
      index <- index + 1
    }
  }
  lay <- customLayout::lay_new(mat = matrix(1:n^2, nrow = n), widths = rep(1, n), heights = rep(1, n))
  customLayout::lay_grid(Pall, lay)
}
