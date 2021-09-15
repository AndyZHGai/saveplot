#' Save a plot as PDF
#'
#' @param filename  Filename to save to an PDF
#' @param plot A plot object
#' @param width Width of the image in inches
#' @param height Height of the image in inches
#' @param ... other parameters
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' saveplot(filename = "test.pdf", plot(1:10))
saveplot <- function(plot = plot(1:10), filename = "test", width = 10, height = 10, ...) {
  stopifnot(!missing(plot))
  if (missing(filename)) {
    filename <- paste(sample(LETTERS, 1), sample(letters, 1),
                      sample(0:9, 1), ".pdf", sep = "")
  } else {
    filename <- paste(filename, sample(0:9, 1), ".pdf", sep = "")
  }
  if (inherits(plot, "ggplot")) {
    ggplot2::ggsave(filename = filename,
                          plot = plot,
                          width = width,
                          height = height)
  }
  if (inherits(plot, "pheatmap")) {
    pdf(file = filename, width = width, height = height)
    grid::grid.newpage()
    grid::grid.draw(plot$gtable)
  }
  if (inherits(plot, "NULL")) {
    pdf(paste0(filename, ".pdf"), width = width, height = height)
    plot
  }
  grDevices::dev.off()
}

