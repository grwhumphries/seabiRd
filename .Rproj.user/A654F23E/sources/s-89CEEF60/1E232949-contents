#' Color palettes
#'
#' @export
seabird_palettes <- list(
  `bluefootedbooby` = c("#c3eaf1","#dcd2d1","#3f4555","#89634c","#bdc09d","#704d3d"),
  `atlanticpuffin` = c("#e38643","#100f19","#f4f2f1","#2c2a38","#c1012c","#875309"),
  `kingpenguin` = c("#1d1a18","#eaf3f1","#3d373b","#ea9200", "#9db2bd","#ee964d"),
  `wavedalbatross` = c("#e6e7e2","#d9b20c","#d4bb87", "#928e84","#504531","#291914"),
  `whiskeredauklet` = c("#4e5051","#c13214", "#a49899","#151616","#a7a5ae","#f4f2f1"),
  `incatern` = c("#e92b19","#e7ca2b","#f4f2f1","#757f8f","#333842","#c74611"),
  `chileanskua` = c("#302b2b","#543f35","#f8dcad","#8a563d","#a88b75","#7a6356"),
  `redleggedkittiwake` = c("#ddd58d","#f4f2f1","#96979d","#d5d7d8","#414038","#ac2c22")
)


#' Function to interpolate a color palette
#'
#' @param palette Character name of palette in seabird_palettes
#' @param reverse Boolean true if palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @export
seabird_pal <- function(palette = "bluefootedbooby", reverse = FALSE, ...){
  pal <- seabird_palettes[[palette]]

  if(reverse){
    pal <- rev(pal)
  }

  grDevices::colorRampPalette(pal, ...)
}

#' Color scale for Seabird colors
#'
#' @param palette Character name of palette in seabird_palettes
#' @param discrete Boolean if color aesthetic is discrete
#' @param reverse Boolean indicating whether palette should be reversed
#' @param ... Additional arguments used to discrete_scale() or scale_fill_gradientn()
#'   to automatically interpolate between colours.
#' @export
scale_color_seabird <- function(palette = "bluefootedbooby",
                                discrete = TRUE, reverse=FALSE,...){
  pal <- seabird_pal(palette = palette, reverse = reverse)

  if(discrete){
    ggplot2::discrete_scale("colour", paste0("seabird_", palette), palette = pal, ...)
  }else{
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale for Seabird colors
#'
#' @param palette Character name of palette in seabird_palettes
#' @param discrete Boolean if color aesthetic is discrete
#' @param reverse Boolean if palette should be reversed
#' @param ... Additional arguments used to discrete_scale() or scale_fill_gradientn()
#'   to automatically interpolate between colours.
#' @export
scale_fill_seabird <- function(palette = "bluefootedbooby",
                               discrete = TRUE, reverse = FALSE, ...){
  pal <- seabird_pal(palette = palette, reverse = reverse)

  if(discrete){
    ggplot2::discrete_scale("fill", paste0("seabird_", palette), palette = pal, ...)
  }else{
    ggplot2::scale_fill_gradientn(colours = pal(256),...)
  }
}
