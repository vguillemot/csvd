#_____________________________________________________________________
# function theme_csvd()
# Inspired from function theme_ptca()
# Hervé Abdi from original from Vincent Guillemot
# April 11, 2018. Current version: May 7, 2018.
# A good place to pick up colors for theme_ptca() and alternative os
# https://www.w3schools.com/colors/colors_picker.asp
#_____________________________________________________________________
# Preambule theme_ptca ----
#' @title  A ggplot2 theme that matches the defaults
#' of the factorial figures in PTCA4CATA
#'
#' @description \code{theme_ptca}
#' A ggplot2 theme that matches the defaults
#' of the factorial figures in PTCA4CATA.
#'
#' @param mire (Default = \code{TRUE}) when \code{TRUE}
#' plot the factorial axis at values \code{x0} and \code{y0}.
#' @param x0 (Default = 0) the X-axis origin. Relevant only
#' when \code{mire = TRUE}.
#' @param y0 (Default = 0) the Y-axis origin.
#' Relevant only
#' when \code{mire = TRUE}.
#' @author Vincent Guillemot
#' @import ggplot2
#' @examples
#'\dontrun{
#' x = c(5, 8, 1, 70)
#' y = c(10, 12, 20, 50)
#' p <-  ggplot(as.data.frame(cbind(x,y)),
#'                  aes(x = x, y = y)) +
#'                     geom_point(size=2, shape=19) + theme_ptca()
#' }
#' @export
theme_csvd <- function(mire = TRUE, x0 = 0, y0 = 0) {
  dark.col <- '#26004d'  # a darker version of purple
  col.fill <- adjustcolor('lavender', alpha.f = .2)
  col.facet <- 'lavender'
  col.bkg <- 'darkorchid'
  col.axes <- adjustcolor('darkorchid', alpha.f = .2)
  width.axes <- 1.1
  theme_list <- list(
    theme_grey() + # %+replace%
      theme(
        legend.key   = element_rect(fill = NA, color = NA),
        legend.title = element_text(color = dark.col, face = 'bold'),
        legend.text  = element_text(color = dark.col),
        axis.text    = element_text(color = dark.col),
        axis.ticks   = element_line(color = dark.col),
        axis.title   = element_text(color = dark.col),
        panel.background = element_rect(color = col.bkg, 
                                        fill = col.fill),
        aspect.ratio = 1,
        strip.background = element_rect(fill = col.facet),
        strip.text = element_text(colour = dark.col, face = 'bold')
      ))
  if (mire) theme_list <- append(theme_list, list(
    geom_vline(xintercept = x0, color = col.axes, size = width.axes),
    geom_hline(yintercept = y0, color = col.axes, size = width.axes)))
  return(theme_list)
}
# end of theme_ptca() ----
# ____________________________________________________________________