#' @title round rectangle
#' @param radius radius of rectangle corners (using [grid::unit()]s)
#' @export
#' @rdname geom_round_tile
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
        radius = grid::unit(1, 'pt'),
        linejoin = "round",
        na.rm = FALSE,
        show.legend = NA,
        inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundRect,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            radius = radius,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' 图例
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
draw_key_round_rect <- function(data, params, size) { # nocov start
    grid::roundrectGrob(
        r = min(params$radius, grid::unit(3, "pt")),
        default.units = "native",
        width = 1,
        name = "lkey",
        gp = grid::gpar(
            col = params$color %||% "white",
            fill = ggplot2::alpha(data$fill %||% data$colour %||% "grey20", data$alpha),
            lty = data$linetype %||% 1,
            lwd = data$linewidth %||% 1
        )
    )
}
#' @format NULL
#' @usage NULL
#' @export
GeomRoundRect <- ggplot2::ggproto(
    "GeomRoundRect", ggplot2::Geom,
    required_aes = c("xmin", "xmax", "ymin", "ymax"),
    default_aes = ggplot2::aes(colour = NA, fill = "grey35", linewidth = 0.5, linetype = 1,
                               alpha = NA),
    setup_data = function(self, data, params) {
        if (all(c("xmin", "xmax", "ymin", "ymax") %in% names(data))) {
            return(data)
        }

        # Fill in missing aesthetics from parameters
        required <- strsplit(self$required_aes, "|", fixed = TRUE)
        missing  <- setdiff(unlist(required), names(data))
        default <- params[intersect(missing, names(params))]
        data[names(default)] <- default

        if (is.null(data$xmin) || is.null(data$xmax)) {
            x <- resolve_rect(
                data[["xmin"]], data[["xmax"]],
                data[["x"]], data[["width"]],
                fun = ggplot2:::snake_class(self), type = "x"
            )
            i <- lengths(x) > 1
            data[c("xmin", "xmax")[i]] <- x[i]
        }
        if (is.null(data$ymin) || is.null(data$ymax)) {
            y <- resolve_rect(
                data[["ymin"]], data[["ymax"]],
                data[["y"]], data[["height"]],
                fun = ggplot2:::snake_class(self), type = "y"
            )
            i <- lengths(y) > 1
            data[c("ymin", "ymax")[i]] <- y[i]
        }
        data
    },
    # 定义了如何在绘图面板上绘制图形。通常需要将数据转换到绘图坐标系，
    # 并使用 grid 包的图形对象（如 rectGrob、pointsGrob 等）来绘制
    draw_panel = function(data, panel_params, coord,
                          lineend = "butt", linejoin = "round",
                          radius = grid::unit(1, "pt")) {
        data <- ggplot2:::check_linewidth(data, snake_class(self))
        if (!coord$is_linear()) {
            aesthetics <- setdiff(
                names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
            )
            index <- rep(seq_len(nrow(data)), each = 4)

            new <- data[index, aesthetics, drop = FALSE]
            new$x <- vctrs::vec_interleave(data$xmin, data$xmax, data$xmax, data$xmin)
            new$y <- vctrs::vec_interleave(data$ymax, data$ymax, data$ymin, data$ymin)
            new$group <- index

            ggplot2:::ggname("geom_round_rect", ggforce::GeomShape$draw_panel(
                new, panel_params, coord, radius = radius
            ))
        } else {
            coords <- coord$transform(data, panel_params)
            rects <- mapply(function(xmin, xmax, ymin, ymax, fill, colour, alpha,
                                     linewidth, linetype, linejoin, lineend) {
                grid::roundrectGrob(
                    x = mean(c(xmin, xmax)),
                    y = mean(c(ymin, ymax)),
                    width = xmax - xmin,
                    height = ymax - ymin,
                    r = radius,
                    default.units = "npc",
                    just = "center",
                    gp = grid::gpar(
                        fill = alpha(fill, alpha),
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

            ggplot2:::ggname("geom_custom_rect", do.call(grid::grobTree, rects))
        }
    },
    # 定义了如何绘制图例中的键
    draw_key = draw_key_round_rect
)

resolve_rect <- function(min = NULL, max = NULL, center = NULL, length = NULL,
                         fun, type) {
    absent <- c(is.null(min), is.null(max), is.null(center), is.null(length))
    if (sum(absent) > 2) {
        missing <- switch(
            type,
            x = c("xmin", "xmax", "x", "width"),
            y = c("ymin", "ymax", "y", "height")
        )
        cli::cli_abort(c(
            "{.fn {fun}} requires two of the following aesthetics: \\
      {.or {.field {missing}}}.",
            i = "Currently, {.field {missing[!absent]}} is present."
        ))
    }

    if (absent[1] && absent[2]) {
        min <- center - 0.5 * length
        max <- center + 0.5 * length
        return(list(min = min, max = max))
    }
    if (absent[1]) {
        if (is.null(center)) {
            min <- max - length
        } else {
            min <- max - 2 * (max - center)
        }
    }
    if (absent[2]) {
        if (is.null(center)) {
            max <- min + length
        } else {
            max <- min + 2 * (center - min)
        }
    }
    list(min = min, max = max)
}
