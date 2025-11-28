check_linewidth <- function (data, name) {
    if (is.null(data$linewidth) && !is.null(data$size)) {
        deprecate_soft0("3.4.0", I(paste0("Using the `size` aesthetic with ",
                                          name)), I("the `linewidth` aesthetic"))
        data$linewidth <- data$size
    }
    data
}
