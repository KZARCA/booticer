#' @export
plot_ce <- function(x, xlab = "Delta Effectiveness", ylab = "Delta Cost", unit_x = "QALY",
                    unit_y = "€", sep1000 = " "){
  ggplot(x) + aes(x = d_eff, y = d_cost, color = strategy) + geom_point() +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_y_continuous(labels = function(x) format(x, big.mark = sep1000)) +
    xlab(sprintf("%s (%s)", xlab, unit_x)) +
    ylab(sprintf("%s (%s)", ylab, unit_y)) +
    theme_bw()
}

#' @export
plot_ac <- function(x, min = 0, max = 100000, by = (max - min) /100,
                    xlab = "Cost-effectiveness Threshold", unit = "€/QALY", sep1000 = " ",
                    ylab = "Probability of strategy being the most cost-effective") {
  prop_ce <- get_nmb_tbl(x, min, max, by)
  ggplot(prop_ce) + aes(threshold, proportion, color = strategy) + geom_smooth(se = FALSE) +
    scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    scale_x_continuous(labels = function(x) format(x, big.mark = sep1000)) +
    xlab(sprintf("%s (%s)", xlab, unit)) +
    ylab(ylab) +
    theme_bw()
}

#' @export
export_image <- function (path, extension, dpi, hauteur, savePlotDesc, plotDesc, savePlotBivar, plotBivar) {
    graph <- plotDesc[[n]]
    if (!is.null(graph$labels$fill) & is.null(graph$guides$fill) | !is.null(graph$labels$colour) | !is.null(graph$mapping$x) && quo_name(graph$mapping$x) == "time"){
      base_aspect_ratio <- 4/3
    }
    do.call("ggsave", c(
      list(
        filename = sprintf("%s/desc%d.%s", path, n, extension),
        plot = graph %>% add_watermark(show = FALSE), # + theme_bw(base_size = 10)  + theme(axis.title=element_text(size=10)),
        dpi = dpi,
        #base_aspect_ratio = base_aspect_ratio,
        height = hauteur / 2.54,
        width = hauteur * base_aspect_ratio / 2.54),
      if(extension == "pdf") list(device = cairo_pdf),
      if (extension == "eps") list(encoding = "ISOLatin9"),
      if (extension == "tiff") list(compression = "lzw")))
}
