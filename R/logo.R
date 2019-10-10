#' Logo designer
#'
#' Simple function developed to desing logos
#'
#' @param name Name of you company/institution (char string)
#' @param text_size Logo text size
#' @param cols Logo colours - either avalid palette name or colour names
#' @param additional_text Additional title text
#' @param expr Posibility to add expressions to logo
#' @param expr.index Expression index (possition)
#' @param all_caps LOGICAL
#' @param include_title LOGICAL Include the name of your company in logo title
#' @param family Font style
#' @param fontface Font type
#'
#' @export
#' @import ggplot2 data.table RColorBrewer stats grDevices
#'
#' @examples
#' 
#' library(RFun)
#' 
#' a <- logo(name = 'KVHEM', 
#' additional_text = 'Katedra vodního hospodářství 
#'                              a environmentálního modelování')
#' a + theme(title = element_text(size = 12.5))
#' 
#' b <- logo(name = 'KVHEM', 
#'           additional_text = 'Katedra vodního hospodářství 
#'                              \na environmentálního modelování', 
#'           include_title = FALSE, 
#'           family = 'AvantGarde')
#' b + theme(panel.grid.major = element_blank(),
#'           panel.grid.minor = element_blank(),
#'           axis.ticks = element_blank(),
#'           axis.text = element_blank(),
#'           axis.line = element_blank(),
#'           plot.title = element_text(size = 25, 
#'                                     hjust = 0.25, 
#'                                     color = 'steelblue4'))
#' 
#' c <- logo(name = 'R-Users Group', 
#'           text_size = 10, 
#'           cols = 'Greens', 
#'           family = 'Courier')
#' c
#' 
#' x <- logo(name = 'DRUtES', 
#'           additional_text = '\nDual Richards Unsaturated Equation Solver', 
#'           cols = c('royalblue4', 
#'                    'lightsteelblue1'), 
#'           expr = expression(integral()[Omega]), 
#'           text_size = 9.5, 
#'           include_title = FALSE, 
#'           fontface = 'bold.italic')
#' x + theme(panel.grid.major = element_blank(),
#'           panel.grid.minor = element_blank(),
#'           axis.ticks = element_blank(),
#'           axis.text = element_blank(),
#'           axis.line = element_line(colour = 'royalblue4'))
#' 
logo <- function(name, 
                 text_size = 20, 
                 cols = 'Blues', 
                 additional_text = NULL, 
                 expr = NULL, 
                 expr.index = seq_along(expr) - 1, 
                 all_caps = FALSE, 
                 include_title = TRUE, 
                 family = 'Palatino', 
                 fontface = 'bold') {

  fill <- prs <- txt <- xmax <- xmin <- ymax <- ymin <- NULL
  
  if(all_caps) {
    
    lttrs <- unlist(x = strsplit(x = toupper(x = name), 
                                 split = ''))
  } else {
    
    lttrs <- unlist(x = strsplit(x = name, 
                                 split = ''))
  }

  lttrs.w.expr <- c(lttrs, 
                    as.character(x = expr))
  id  <- c(seq_along(along.with = lttrs), 
           expr.index + 0.5)
  lttrs.w.expr <- lttrs.w.expr[order(id)]

  x <- 0:(length(x = lttrs.w.expr) - 1)
  d <- data.table(xmin = x, 
                  xmax = x + 1, 
                  ymin = 0, 
                  ymax = 1, 
                  fill = x, 
                  txt = lttrs.w.expr, 
                  prs = nchar(x = lttrs.w.expr) != 1)

  ll <- length(x = x)

  d.raw <- d[which(x = !d[, prs]), ]
  d.expr <- d[which(x = d[, prs]), ]
  
  if(length(x = cols) != 1) {
    
    col_pal <- colorRampPalette(
      colors = as.vector(x = unlist(x = strsplit(cols, 
                                                 split = ' ')))
      )(ll + 4)
    
  } else {
    
    col_pal <- rev(x = colorRampPalette(
      colors = brewer.pal(n = 9, name = cols)
      )(ll + 4))
  }

  logo <- ggplot() +
    geom_rect(data = d, 
              mapping = aes(xmin = xmin, 
                            ymin = ymin, 
                            xmax = xmax, 
                            ymax = ymax, 
                            fill = factor(fill))) +
    scale_fill_manual(values =  col_pal[1:dim(x = d)[1]]) +
    theme_classic() +
    theme(aspect.ratio = 1 / ll,
          plot.title = element_text(hjust = 0.5, 
                                    family = family, 
                                    face = fontface, 
                                    colour = col_pal[1]),
          plot.background = element_rect(
            fill = col_pal[length(x = col_pal) - 1]),
          panel.background = element_rect(
            fill = col_pal[length(x = col_pal)- 1]),
          axis.ticks = element_line(
            colour = col_pal[1]),
          axis.text = element_text(
            colour = col_pal[1]),
          axis.line = element_line(
            colour = col_pal[1]),
          legend.position = 'none') +
    geom_text(data = d.raw,
              mapping = aes(x = xmin + .5, 
                            y = ymin + .5, 
                            label = txt),
              parse = FALSE, 
              colour = col_pal[length(x = col_pal)],
              size = text_size,
              family = family,
              fontface = fontface) +
    labs(x = '', 
         y = '', 
         title = ifelse(test = include_title, 
                        yes = paste(name, 
                                    ifelse(test = include_title & 
                                             !is.null(additional_text), 
                                           yes = '-', 
                                           no = ''), 
                                    additional_text), 
                        no = additional_text))

  if(!is.null(expr)) {
    
    logo <- logo +
      geom_text(data = d.expr,
                mapping = aes(x = xmin + .5, 
                              y = ymin + .5, 
                              label = txt),
                parse = TRUE,
                colour = col_pal[length(x = col_pal)],
                size = text_size,
                family = family,
                fontface = fontface)
    }

  logo
}
