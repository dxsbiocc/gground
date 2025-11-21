#' @title round rectangle
#' @param radius radius of rectangle corners (using [grid::unit()]s)
#' @export
#' @rdname geom_round_tile
#' @importFrom grid unit
#' @importFrom ggplot2 layer
#' @importFrom rlang list2
#' @examples
#' library(gground)
#'
#' df <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = rep(c(1, 2), each = 5),
#'   z = factor(rep(1:5, each = 2)),
#'   w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
#' )
#'
#' ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) +
#'   geom_round_rect(aes(fill = z), colour = "white")
geom_round_rect <- function(
        mapping = NULL, data = NULL,
        stat = "identity", position = "identity",
        ...,
        radius = 2,
        linejoin = "round",
        na.rm = FALSE,
        show.legend = NA,
        inherit.aes = TRUE) {
    if (grid::is.unit(radius)) {
        radius <- radius
    } else {
        radius <- grid::unit(radius, "pt")
    }
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundRect,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(
            radius = radius,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' 图例
#' @importFrom grid roundrectGrob unit gpar
#' @importFrom ggplot2 fill_alpha
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @return A grob representing the key.
draw_key_round_rect <- function(data, params, size) { # nocov start
    if (is.null(data$linewidth)) {
        data$linewidth <- 0.5
    }
    lwd <- min(data$linewidth, min(size) / 4)

    if (grid::is.unit(params$radius)) {
        params$radius <- params$radius
    } else {
        params$radius <- unit(params$radius, "pt")
    }

    grob <- roundrectGrob(
        r = min(params$radius, unit(3, "pt")),
        width = unit(1, "npc") - unit(lwd, "mm"),
        height = unit(1, "npc") - unit(lwd, "mm"),
        name = "rrkey",
        gp = gpar(
            col = data$colour %||% NA,
            fill = fill_alpha(data$fill %||% "grey20", data$alpha),
            lty = data$linetype %||% 1,
            lwd = lwd * .pt,
            linejoin = params$linejoin %||% "mitre",
            lineend = params$lineend %||% "butt"
        )
    )
    # Magic number is 5 because we convert mm to cm (divide by 10) but we
    # draw two lines in each direction (times 2)
    attr(grob, "width")  <- lwd / 5
    attr(grob, "height") <- lwd / 5
    grob
}

#' @format NULL
#' @importFrom ggplot2 ggproto Geom aes fill_alpha
#' @importFrom ggforce GeomShape
#' @importFrom grid roundrectGrob grobTree gpar
#' @importFrom vctrs vec_interleave
#' @usage NULL
#' @export
GeomRoundRect <- ggproto(
    "GeomRoundRect", Geom,
    required_aes = c("xmin", "xmax", "ymin", "ymax"),
    default_aes = aes(colour = NA, fill = "grey35", linewidth = 0.5, linetype = 1,
                               alpha = NA),
    # Defined how to draw graphics on the drawing panel.
    draw_panel = function(data, panel_params, coord,
                          lineend = "butt", linejoin = "round",
                          radius = 2) {
        if (grid::is.unit(radius)) {
            radius <- radius
        } else {
            radius <- grid::unit(radius, "pt")
        }
        data <- ggplot2:::check_linewidth(data, snake_class(self))
        if (!coord$is_linear()) {
            aesthetics <- setdiff(
                names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
            )
            index <- rep(seq_len(nrow(data)), each = 4)

            new <- data[index, aesthetics, drop = FALSE]
            # order is very important here for the correct drawing of the rectangle in
            # polar coordinates
            if (coord$theta == 'y') {
                new$x <- vec_interleave(data$xmin, data$xmin, data$xmax, data$xmax)
                new$y <- vec_interleave(data$ymin, data$ymax, data$ymax, data$ymin)
            } else {
                new$x <- vec_interleave(data$xmin, data$xmax, data$xmax, data$xmin)
                new$y <- vec_interleave(data$ymax, data$ymax, data$ymin, data$ymin)
            }

            new$group <- index

            ggplot2:::ggname("geom_round_rect", GeomShape$draw_panel(
                new, panel_params, coord, radius = radius
            ))
        } else {
            coords <- coord$transform(data, panel_params)
            rects <- mapply(function(xmin, xmax, ymin, ymax, fill, colour, alpha,
                                     linewidth, linetype, linejoin, lineend) {
                roundrectGrob(
                    x = mean(c(xmin, xmax)),
                    y = mean(c(ymin, ymax)),
                    width = xmax - xmin,
                    height = ymax - ymin,
                    r = radius,
                    default.units = "native",
                    just = "center",
                    gp = gpar(
                        fill = fill_alpha(fill, alpha),
                        col = colour,
                        lwd = linewidth * .pt,
                        lty = linetype,
                        linejoin = linejoin,
                        lineend = lineend
                    )
                )
            }, coords$xmin, coords$xmax, coords$ymin, coords$ymax,
            coords$fill, coords$colour, coords$alpha, coords$linewidth,
            coords$linetype, linejoin, lineend,
            SIMPLIFY = FALSE)

            ggplot2:::ggname("geom_round_rect", do.call(grobTree, rects))
        }
    },
    # 定义了如何绘制图例中的键
    draw_key = draw_key_round_rect
)
