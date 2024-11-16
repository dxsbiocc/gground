#' title: Round Column Geom
#' @export
#' @rdname geom_round_bar
geom_round_col <- function(mapping = NULL, data = NULL,
                     position = "stack",
                     ...,
                     just = 0.5,
                     radius = grid::unit(0.1, "npc"),
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = "identity",
        geom = GeomRoundCol,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            radius = radius,
            just = just,
            na.rm = na.rm,
            ...
        )
    )
}

#' @format NULL
#' @usage NULL
#' @export
#' @include geom-round-rect.R
# TODO: deprecate this
GeomRoundCol <- ggplot2::ggproto("GeomRoundCol", GeomRoundBar)
