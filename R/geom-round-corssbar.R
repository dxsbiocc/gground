#' @title round crossbar
#' @details `geom_round_crossbar()` is a cross between `geom_round_tile()` and
#' `geom_round_bar()`. It is a bar that is rounded at the ends, but it is not
#' filled in. It is useful for showing the range of values in a dataset.
#' @param mapping Set of aesthetic mappings created by `aes()` or `aes_()`.
#' @param data A layer specific dataset - only needed if you want to override the
#'  plot defaults.
#' @param stat The statistical transformation to use on the data for this layer,
#' as a string.
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#' @param ... Other arguments passed on to `layer()`.
#' @param fatten A multiplicative factor to fatten the line ends.
#' @param radius The radius of the rounded corners.
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#' @param orientation The orientation of the layer. The default (NA) automatically
#' determines the orientation from the aesthetic mappings.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If FALSE, overrides the default aesthetics.
#' @export
#' @rdname geom_round_crossbar
geom_round_crossbar <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          fatten = 2.5, radius = grid::unit(1, 'pt'),
                          na.rm = FALSE,
                          orientation = NA,
                          show.legend = NA,
                          inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundCrossbar,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            radius = radius,
            fatten = fatten,
            na.rm = na.rm,
            orientation = orientation,
            ...
        )
    )
}

#' @format NULL
#' @usage NULL
#' @export
draw_key_round_crossbar <- function(data, params, size) {
    gp <- grid::gpar(
        col = data$colour %||% "grey20",
        fill = ggplot2::fill_alpha(data$fill %||% "white", data$alpha),
        lwd = (data$linewidth %||% 0.5) * ggplot2::.pt,
        lty = data$linetype %||% 1,
        lineend = params$lineend %||% "butt",
        linejoin = params$linejoin %||% "mitre")

    if (isTRUE(params$flipped_aes)) {
        grid::grobTree(
            grid::roundrectGrob(r = min(params$radius, grid::unit(2, "pt")),
                                height = 0.75, width = 0.7),
            grid::linesGrob(0.5, c(0.125, 0.875)), gp = gp)
    }
    else {
        grid::grobTree(
            grid::roundrectGrob(r = min(params$radius, grid::unit(2, "pt")),
                                height = 0.75, width = 0.7),
            grid::linesGrob(c(0.125, 0.875), 0.5), gp = gp)
    }
}

#' @format NULL
#' @usage NULL
#' @export
GeomRoundCrossbar <- ggplot2::ggproto("GeomRoundCrossbar", ggplot2::Geom,
                        setup_params = function(data, params) {
                            ggplot2::GeomErrorbar$setup_params(data, params)
                        },

                        extra_params = c("na.rm", "orientation"),

                        setup_data = function(data, params) {
                            ggplot2::GeomErrorbar$setup_data(data, params)
                        },

                        default_aes = ggplot2::aes(colour = "black", fill = NA, linewidth = 0.5, linetype = 1,
                                          alpha = NA),

                        required_aes = c("x", "y", "ymin|xmin", "ymax|xmax"),

                        draw_key = draw_key_round_crossbar,

                        draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                                              linejoin = "mitre", fatten = 2.5, width = NULL,
                                              flipped_aes = FALSE, radius = grid::unit(1, 'pt')) {
                            data <- ggplot2:::check_linewidth(data, ggplot2::snake_class(self))
                            data <- ggplot2::flip_data(data, flipped_aes)

                            middle <- transform(data, x = xmin, xend = xmax, yend = y, linewidth = linewidth * fatten, alpha = NA)

                            has_notch <- !is.null(data$ynotchlower) && !is.null(data$ynotchupper) &&
                                !is.na(data$ynotchlower) && !is.na(data$ynotchupper)

                            if (has_notch) {
                                if (data$ynotchlower < data$ymin  ||  data$ynotchupper > data$ymax)
                                    cli::cli_inform(c(
                                        "Notch went outside hinges",
                                        i = "Do you want {.code notch = FALSE}?"
                                    ))

                                notchindent <- (1 - data$notchwidth) * (data$xmax - data$xmin) / 2

                                middle$x <- middle$x + notchindent
                                middle$xend <- middle$xend - notchindent

                                box <- ggplot2:::data_frame0(
                                    x = c(
                                        data$xmin, data$xmin, data$xmin + notchindent, data$xmin, data$xmin,
                                        data$xmax, data$xmax, data$xmax - notchindent, data$xmax, data$xmax,
                                        data$xmin
                                    ),
                                    y = c(
                                        data$ymax, data$ynotchupper, data$y, data$ynotchlower, data$ymin,
                                        data$ymin, data$ynotchlower, data$y, data$ynotchupper, data$ymax,
                                        data$ymax
                                    ),
                                    alpha = rep(data$alpha, 11),
                                    colour = rep(data$colour, 11),
                                    linewidth = rep(data$linewidth, 11),
                                    linetype = rep(data$linetype, 11),
                                    fill = rep(data$fill, 11),
                                    group = rep(seq_len(nrow(data)), 11)
                                )
                            } else {
                                # No notch
                                box <- ggplot2:::data_frame0(
                                    x = c(data$xmin, data$xmin, data$xmax, data$xmax, data$xmin),
                                    y = c(data$ymax, data$ymin, data$ymin, data$ymax, data$ymax),
                                    alpha = rep(data$alpha, 5),
                                    colour = rep(data$colour, 5),
                                    linewidth = rep(data$linewidth, 5),
                                    linetype = rep(data$linetype, 5),
                                    fill = rep(data$fill, 5),
                                    group = rep(seq_len(nrow(data)), 5) # each bar forms it's own group
                                )
                            }
                            box <- ggplot2::flip_data(box, flipped_aes)
                            middle <- ggplot2::flip_data(middle, flipped_aes)

                            ggplot2:::ggname("geom_round_crossbar", grid::gTree(children = grid::gList(
                                ggforce::GeomShape$draw_panel(box, panel_params, coord, radius = radius),
                                ggplot2::GeomSegment$draw_panel(middle, panel_params, coord, lineend = lineend, linejoin = linejoin)
                            )))
                        },

                        rename_size = TRUE
)
